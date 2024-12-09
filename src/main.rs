use std::collections::{hash_map, HashMap};
use std::fmt::{self, Write};
use std::fs::{self, File};
use std::io;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use buffers::Buffers;
use error::Error;
use lspower::{jsonrpc, lsp, Client, LanguageServer, LspService, Server};
use redscript::ast::{Expr, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
use redscript::definition::{Class, Enum, Function, Type};
use redscript_compiler::diagnostics::Diagnostic;
use redscript_compiler::parser;
use redscript_compiler::scope::{Reference, TypeId};
use redscript_compiler::source_map::Files;
use redscript_compiler::symbol::Symbol;
use redscript_compiler::typechecker::{type_of, Callable, Member, TypedAst};
use redscript_compiler::unit::{CompilationUnit, TypecheckOutput};
use redscript_formatter::{format_document, FormatSettings};
use serde::{Deserialize, Serialize};
use source_links::SourceLinks;
use tinytemplate::TinyTemplate;
use tokio::sync::{OnceCell, RwLock};

mod buffers;
mod error;
mod source_links;
mod util;

const DOT_REDSCRIPT: &str = ".redscript-ide";

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend {
        client,
        config: OnceCell::new(),
        state: RwLock::new(None),
        pool: OnceCell::new(),
        workspace_folders: RwLock::new(HashMap::new()),
        hooks: RwLock::new(Hooks::default()),
        buffers: Buffers::default(),
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}

#[derive(Serialize)]
struct PathContext {
    game_dir: Option<PathBuf>,
    workspace_dir: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Hook {
    CreateFile(String),
}

#[derive(Debug, Default, Deserialize)]
struct Hooks {
    successful_check: Vec<Hook>,
}

#[derive(Debug, Default, Deserialize)]
struct DotRedscript {
    redscript_dir: Option<PathBuf>,
    hooks: Hooks,
}

impl DotRedscript {
    pub fn load(root_dir: &Path) -> Result<Option<Self>, Error> {
        let path = root_dir.join(DOT_REDSCRIPT);
        let contents = match fs::read_to_string(path) {
            Ok(res) => res,
            Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(Error::Other(err.into())),
        };
        toml::from_str(&contents).map_err(Error::DotRedscriptParseFailure)
    }
}

#[derive(Debug, Clone, Deserialize)]
struct Config {
    script_cache_path: Option<PathBuf>,
    game_dir: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
#[serde(from = "(Option<PathBuf>, Option<PathBuf>)")]
struct ConfigFields(Config);

impl From<(Option<PathBuf>, Option<PathBuf>)> for ConfigFields {
    fn from((script_cache_path, game_dir): (Option<PathBuf>, Option<PathBuf>)) -> Self {
        Self(Config {
            script_cache_path: script_cache_path.filter(|p| p.components().count() > 0),
            game_dir: game_dir.filter(|p| p.components().count() > 0),
        })
    }
}

struct Backend {
    client: Client,
    config: OnceCell<Config>,
    pool: OnceCell<ConstantPool>,
    workspace_folders: RwLock<HashMap<PathBuf, PathBuf>>,
    state: RwLock<Option<ServerState>>,
    hooks: RwLock<Hooks>,
    buffers: Buffers,
}

struct ServerState {
    compiled_pool: ConstantPool,
    source_links: SourceLinks,
}

impl ServerState {
    pub fn new(compiled_pool: ConstantPool, files: &Files, output: &TypecheckOutput) -> Self {
        Self {
            source_links: SourceLinks::new(&compiled_pool, output, files),
            compiled_pool,
        }
    }
}

impl Backend {
    const CONFIG_FIELDS: &'static [&'static str] =
        &["redscript.scriptCachePath", "redscript.gameDir"];

    async fn load_dot_redscript(&self, dir: PathBuf) -> Result<DotRedscript, Error> {
        let dot_redscript = DotRedscript::load(&dir)?.unwrap_or_default();

        let mut hooks = self.hooks.write().await;
        hooks
            .successful_check
            .extend(dot_redscript.hooks.successful_check.iter().cloned());

        Ok(dot_redscript)
    }

    async fn execute_hooks(&self, hooks: &[Hook], workspace_dir: PathBuf) {
        let context = PathContext {
            game_dir: self.config.get().and_then(|conf| conf.game_dir.clone()),
            workspace_dir,
        };
        for hook in hooks {
            self.execute_hook(hook, &context).await;
        }
    }

    async fn execute_hook(&self, hook: &Hook, ctx: &PathContext) {
        match hook {
            Hook::CreateFile(path) => {
                let result: Result<(), Box<dyn std::error::Error + Send + Sync>> = (|| {
                    let mut template = TinyTemplate::new();
                    template.add_template("path", path)?;
                    let path = template.render("path", &ctx)?;
                    File::create(path)?;
                    Ok(())
                })(
                );

                if let Err(err) = result {
                    self.log_error(format!("failed to run hook: {}", err)).await;
                }
            }
        }
    }

    async fn add_workspace_folder(&self, dir: PathBuf) -> Result<(), Error> {
        let dot_redscript = self.load_dot_redscript(dir.clone()).await?;
        let source_dir = dot_redscript.redscript_dir.map_or_else(
            || dir.clone(),
            |p| if p.is_absolute() { p } else { dir.join(p) },
        );
        let source_dir = match source_dir.try_exists() {
            Ok(true) => source_dir,
            _ => return Err(Error::InvalidRedscriptSourceDir(source_dir)),
        };

        self.workspace_folders.write().await.insert(dir, source_dir);
        Ok(())
    }

    async fn post_initialize(&self) -> Result<(), Error> {
        let config = self.get_configuration().await?;
        let path = config
            .script_cache_path
            .clone()
            .map(Ok)
            .unwrap_or_else(|| {
                config
                    .game_dir
                    .as_ref()
                    .ok_or(Error::MissingConfiguration)
                    .and_then(|dir| {
                        let default_bk = dir
                            .join("r6")
                            .join("cache")
                            .join("modded")
                            .join("final.redscripts.bk");
                        default_bk
                            .exists()
                            .then_some(default_bk)
                            .or_else(|| {
                                let fallback =
                                    dir.join("r6").join("cache").join("final.redscripts.bk");
                                fallback.exists().then_some(fallback)
                            })
                            .or_else(|| {
                                let fallback =
                                    dir.join("r6").join("cache").join("final.redscripts");
                                fallback.exists().then_some(fallback)
                            })
                            .ok_or_else(|| Error::RedscriptCacheNotFound(dir.to_owned()))
                    })
            })?;

        let bundle = ScriptBundle::load(&mut File::open(path)?)?;
        self.pool.set(bundle.pool).unwrap();

        let files = Files::from_dirs(self.workspace_folders.read().await.values())?;
        if let Err(err) = self.typecheck_workspace(&files).await {
            self.log_info(format!("initial typecheck reported an error: {err}"))
                .await;
        }

        Ok(())
    }

    async fn get_configuration(&self) -> Result<Config, Error> {
        if let Some(config) = self.config.get() {
            return Ok(config.clone());
        }

        let items = Self::CONFIG_FIELDS
            .iter()
            .map(|str| lsp::ConfigurationItem {
                section: Some((*str).to_owned()),
                scope_uri: None,
            })
            .collect();
        let conf_json = self
            .client
            .configuration(items)
            .await
            .map_err(|err| Error::Other(err.into()))?;

        let config: ConfigFields = serde_json::from_value(serde_json::Value::Array(conf_json))
            .map_err(|err| Error::Other(err.into()))?;
        self.config.set(config.0.clone()).unwrap();

        Ok(config.0)
    }

    async fn typecheck_workspace(&self, files: &Files) -> Result<TypecheckOutcome, Error> {
        if let Some(mut compiled_pool) = self.pool.get().cloned() {
            let is_fatal = match CompilationUnit::new_with_defaults(&mut compiled_pool)?
                .typecheck_files(files, false, false)
            {
                Ok(output) => {
                    let state = ServerState::new(compiled_pool, files, &output);
                    *self.state.write().await = Some(state);

                    self.publish_diagnostics(output.diagnostics(), files).await;
                    output.diagnostics().iter().any(Diagnostic::is_fatal)
                }
                Err(err) => {
                    let diagnostic = Diagnostic::from_error(err)?;
                    let is_fatal = diagnostic.is_fatal();
                    self.publish_diagnostics(&[diagnostic], files).await;
                    is_fatal
                }
            };
            if is_fatal {
                Ok(TypecheckOutcome::Failed)
            } else {
                Ok(TypecheckOutcome::Succeeded)
            }
        } else {
            self.log_error("project not initialized").await;
            Ok(TypecheckOutcome::NotReady)
        }
    }

    async fn expr_at_location<A>(
        &self,
        pos: lsp::TextDocumentPositionParams,
        fix_parse_error: bool,
        on_expr: impl for<'a> Fn(&'a Expr<TypedAst>, TypeId, &ConstantPool) -> Result<A, Error>,
    ) -> Result<Option<A>, Error> {
        let buf = self
            .buffers
            .get(&pos.text_document.uri)
            .expect("no buffer found for URI");
        let mut pool = self.get_cloned_pool().await?;
        let mut copy = buf.contents().clone();
        let mut needle = buf
            .get_pos(pos.position.line, pos.position.character)
            .expect("position outside of the buffer");

        // very dumb heuristic to fix common parse errors
        // this removes the dot character and inserts a semicolon at the end of the line if it's missing
        if fix_parse_error {
            let idx: usize = (needle - 1).into();

            if copy.byte(idx) == b'.' {
                needle = needle - 1;

                for c in (0..idx).map(|i| copy.byte(i)).rev() {
                    needle = needle - 1;
                    if ![b' ', b'\n', b'\r', b'\t'].contains(&c) {
                        break;
                    }
                }

                let char_idx = copy.byte_to_char(idx);
                copy.remove(char_idx..char_idx + 1);

                let mut insert_colon = None;

                for (i, c) in copy.bytes_at(idx).enumerate() {
                    if c == b';' {
                        break;
                    }
                    if c == b'\n' {
                        insert_colon = Some(idx + i);
                        break;
                    }
                }

                if let Some(colon_idx) = insert_colon {
                    let char_idx = copy.byte_to_char(colon_idx);
                    copy.remove(char_idx..char_idx + 1);
                    copy.insert(char_idx, ";\n");
                };
            }
        }

        // TODO: avoid copying the string
        match parser::parse_str(&copy.to_string()) {
            Ok(module) => {
                let output = CompilationUnit::new_with_defaults(&mut pool)?.typecheck(
                    vec![module],
                    &Files::default(),
                    false,
                    true,
                )?;
                if let Some((expr, scope)) = output
                    .functions()
                    .binary_search_by(|fun| fun.span.compare_pos(needle))
                    .ok()
                    .and_then(|idx| output.functions().get(idx))
                    .and_then(|fun| {
                        util::find_in_seq(&fun.code.exprs, needle).map(|expr| (expr, &fun.scope))
                    })
                {
                    let typ = type_of(expr.as_ref(), scope, &pool)?;
                    return Ok(Some(on_expr(expr.as_ref(), typ, &pool)?));
                }
            }
            Err(err) => {
                self.log_info(format!("encountered a parse error: {err}"))
                    .await;
            }
        }
        Ok(None)
    }

    async fn completion(
        &self,
        params: lsp::CompletionParams,
    ) -> Result<Option<lsp::CompletionResponse>, Error> {
        let matched = self
            .expr_at_location(params.text_document_position, true, |expr, typ, pool| {
                let is_static = matches!(expr, Expr::Ident(Reference::Symbol(_), _));
                match typ.unwrapped() {
                    TypeId::Class(idx) | TypeId::Struct(idx) => {
                        let completions =
                            Self::class_completions(*idx, *idx, typ, is_static, pool)?;
                        Ok(Some(lsp::CompletionResponse::Array(completions)))
                    }
                    TypeId::Enum(idx) if is_static => {
                        let completions = Self::enum_completions(*idx, pool)?;
                        Ok(Some(lsp::CompletionResponse::Array(completions)))
                    }
                    _ => Ok(None),
                }
            })
            .await?;

        Ok(matched.flatten())
    }

    // TODO: use this instead of calculating everything in completion
    async fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
    ) -> Result<lsp::CompletionItem, Error> {
        if let Some(idx_val) = item.data.as_ref().and_then(serde_json::Value::as_u64) {
            let pool = self.get_cloned_pool().await?;
            let idx = PoolIndex::new(idx_val as u32);
            let fun = pool.function(idx)?;

            let name = pool.def_name(idx)?;
            let pretty_name = name.split(';').next().unwrap_or(&name);

            let mut snippet = String::new();
            for (i, param_idx) in fun.parameters.iter().enumerate() {
                let name = pool.def_name(*param_idx)?;
                if i != 0 {
                    write!(snippet, ", ").unwrap();
                }
                write!(snippet, "${{{}:{}}}", i + 1, name).unwrap();
            }
            let detail = util::render_function(idx, true, &pool)?;

            let item = lsp::CompletionItem {
                label: format!("{}(⠤)", pretty_name),
                kind: Some(lsp::CompletionItemKind::METHOD),
                insert_text: Some(format!("{}({})", pretty_name, snippet)),
                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                detail: Some(detail),
                ..lsp::CompletionItem::default()
            };
            Ok(item)
        } else {
            Ok(item)
        }
    }

    async fn hover(&self, params: lsp::HoverParams) -> Result<Option<lsp::Hover>, Error> {
        let buf = self
            .buffers
            .get(&params.text_document_position_params.text_document.uri)
            .expect("No buffer found for URI");
        let matched = self
            .expr_at_location(
                params.text_document_position_params,
                true,
                |expr, typ, pool| {
                    let text = match expr {
                        Expr::Call(Callable::Function(idx), _, _, _) => {
                            util::render_function(*idx, false, pool)?
                        }
                        Expr::MethodCall(_, idx, _, _) => util::render_function(*idx, false, pool)?,
                        _ => typ.pretty(pool)?.deref().to_owned(),
                    };
                    let contents = lsp::HoverContents::Scalar(lsp::MarkedString::LanguageString(
                        lsp::LanguageString {
                            language: "redscript".to_owned(),
                            value: text,
                        },
                    ));
                    let span = expr.span();
                    let start = buf.get_loc(span.low).unwrap();
                    let end = buf.get_loc(span.high).unwrap();
                    let range = lsp::Range::new(
                        lsp::Position::new(start.0, start.1),
                        lsp::Position::new(end.0, end.1),
                    );

                    let hover = lsp::Hover {
                        contents,
                        range: Some(range),
                    };
                    Ok(Some(hover))
                },
            )
            .await?;
        Ok(matched.flatten())
    }

    async fn publish_diagnostics<'a, I>(&self, diagnostics: I, files: &Files)
    where
        I: IntoIterator,
        I::IntoIter: ExactSizeIterator<Item = &'a Diagnostic>,
    {
        let diagnostics = diagnostics.into_iter();
        let mut messages: HashMap<PathBuf, Vec<lsp::Diagnostic>> =
            HashMap::with_capacity(diagnostics.len());

        for diagnostic in diagnostics {
            let severity = if diagnostic.is_fatal() {
                lsp::DiagnosticSeverity::ERROR
            } else {
                lsp::DiagnosticSeverity::WARNING
            };
            let loc = files.lookup(diagnostic.span()).unwrap();

            let range = lsp::Range::new(
                lsp::Position::new(loc.start.line as u32, loc.start.col as u32),
                lsp::Position::new(loc.end.line as u32, loc.end.col as u32),
            );
            let source = Some("redscript".to_owned());
            let msg = diagnostic.to_string();
            let diagnostic =
                lsp::Diagnostic::new(range, Some(severity), None, source, msg, None, None);
            match messages.entry(loc.file.path().to_owned()) {
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().push(diagnostic);
                }
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(vec![diagnostic]);
                }
            }
        }

        for file in files.files() {
            let uri = lsp::Url::from_file_path(file.path()).unwrap();
            self.client.publish_diagnostics(uri, vec![], None).await;
        }

        for (path, batch) in messages {
            let uri = lsp::Url::from_file_path(path).unwrap();
            self.client.publish_diagnostics(uri, batch, None).await;
        }
    }

    fn class_completions(
        idx: PoolIndex<Class>,
        initial_idx: PoolIndex<Class>,
        typ: TypeId,
        is_static: bool,
        pool: &ConstantPool,
    ) -> Result<Vec<lsp::CompletionItem>, Error> {
        let mut completions = vec![];
        let class = pool.class(idx)?;

        if !is_static {
            for idx in &class.fields {
                let field = pool.field(*idx)?;
                let type_ = pool.def_name(field.type_)?;
                let type_name = TypeName::from_repr(&type_);

                let name = pool.def_name(*idx)?;
                let item = lsp::CompletionItem {
                    label: name.deref().to_owned(),
                    kind: Some(lsp::CompletionItemKind::FIELD),
                    detail: Some(type_name.pretty().clone().to_string()),
                    ..lsp::CompletionItem::default()
                };
                completions.push(item);
            }
        }

        for idx in &class.functions {
            let fun = pool.function(*idx)?;

            let has_static_receiver = (|| {
                let def = pool.definition(initial_idx).ok()?;
                let param = fun.parameters.first()?;
                let param = pool.parameter(*param).ok()?;
                let res = match (&typ, pool.type_(param.type_).ok()?) {
                    (TypeId::Ref(_), &Type::Ref(inner))
                    | (TypeId::WeakRef(_), &Type::WeakRef(inner))
                    | (TypeId::ScriptRef(_), &Type::ScriptRef(inner)) => {
                        pool.definition(inner).ok()?.name == def.name
                    }
                    (TypeId::Struct(_), Type::Class) => {
                        pool.definition(param.type_).ok()?.name == def.name
                    }
                    _ => false,
                };
                Some(res)
            })()
            .is_some_and(|test| test && fun.flags.is_static());

            if (is_static || !has_static_receiver) && fun.flags.is_static() != is_static {
                continue;
            }
            let name = pool.def_name(*idx)?;
            let pretty_name = name.split(';').next().unwrap_or(&name);

            let mut snippet = String::new();

            let params = if has_static_receiver && !is_static {
                &fun.parameters[1..]
            } else {
                &fun.parameters
            };

            for (i, param_idx) in params.iter().enumerate() {
                let name = pool.def_name(*param_idx)?;
                if i != 0 {
                    write!(snippet, ", ").unwrap();
                }
                write!(snippet, "${{{}:{}}}", i + 1, name).unwrap();
            }
            let detail = util::render_function(*idx, true, pool)?;

            let item = lsp::CompletionItem {
                label: format!("{}(⠤)", pretty_name),
                kind: Some(lsp::CompletionItemKind::METHOD),
                insert_text: Some(format!("{}({})", pretty_name, snippet)),
                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                detail: Some(detail),
                ..lsp::CompletionItem::default()
            };
            completions.push(item);
        }

        if !class.base.is_undefined() {
            let base = Self::class_completions(class.base, initial_idx, typ, is_static, pool)?;
            completions.extend(base);
        }
        Ok(completions)
    }

    fn enum_completions(
        idx: PoolIndex<Enum>,
        pool: &ConstantPool,
    ) -> Result<Vec<lsp::CompletionItem>, Error> {
        let mut completions = vec![];
        let enum_ = pool.enum_(idx)?;

        for member in &enum_.members {
            let name = pool.def_name(*member)?;
            let item = lsp::CompletionItem {
                label: name.deref().to_owned(),
                kind: Some(lsp::CompletionItemKind::FIELD),
                ..lsp::CompletionItem::default()
            };
            completions.push(item);
        }
        Ok(completions)
    }

    async fn goto_definition(
        &self,
        params: lsp::GotoDefinitionParams,
    ) -> Result<Option<lsp::GotoDefinitionResponse>, Error> {
        fn try_create_redmod_link(
            idx: PoolIndex<Function>,
            pool: &ConstantPool,
            config: &Config,
        ) -> Option<lsp::GotoDefinitionResponse> {
            let source = pool.function(idx).ok()?.source.as_ref()?;
            if source.file == PoolIndex::DEFAULT_SOURCE {
                return None;
            };
            let file = pool.definition(source.file).ok()?.value.as_source_file()?;
            let path = config
                .game_dir
                .as_ref()?
                .join("tools")
                .join("redmod")
                .join("scripts")
                .join(&file.path);
            if path.exists() {
                Some(create_response(&path, source.line))
            } else {
                None
            }
        }

        fn create_response(path: &Path, line: u32) -> lsp::GotoDefinitionResponse {
            lsp::GotoDefinitionResponse::Scalar(lsp::Location {
                uri: lsp::Url::from_file_path(path).unwrap(),
                range: lsp::Range::new(lsp::Position::new(line, 0), lsp::Position::new(line, 0)),
            })
        }

        let state = self.state.read().await;
        let Some(state) = state.as_ref() else {
            return Ok(None);
        };

        let matched = self
            .expr_at_location(
                params.text_document_position_params,
                false,
                |expr, _, pool| {
                    let idx = match expr {
                        Expr::Ident(Reference::Symbol(sym), _) => match sym {
                            Symbol::Class(idx, _) | Symbol::Struct(idx, _) => idx.cast(),
                            Symbol::Enum(idx) => idx.cast(),
                            Symbol::Functions(_) => return Ok(None),
                        },
                        &Expr::Call(Callable::Function(idx), _, _, _)
                        | &Expr::MethodCall(_, idx, _, _) => {
                            if let Some(resp) = self
                                .config
                                .get()
                                .and_then(|conf| try_create_redmod_link(idx, pool, conf))
                            {
                                return Ok(Some(resp));
                            };
                            idx.cast()
                        }
                        Expr::New(TypeId::Class(idx) | TypeId::Struct(idx), _, _) => idx.cast(),
                        Expr::Member(_, Member::ClassField(idx) | Member::StructField(idx), _) => {
                            idx.cast()
                        }
                        Expr::Member(_, Member::EnumMember(idx, _), _) => idx.cast(),
                        _ => return Ok(None),
                    };
                    let Some(item) = state
                        .source_links
                        .get_link_key(idx, pool)
                        .and_then(|key| state.source_links.get(key))
                    else {
                        return Ok(None);
                    };
                    Ok(Some(create_response(item.path(), item.line() as u32)))
                },
            )
            .await?;
        Ok(matched.flatten())
    }

    async fn format(
        &self,
        params: lsp::DocumentFormattingParams,
    ) -> Result<Option<Vec<lsp::TextEdit>>, Error> {
        let uri = params.text_document.uri;
        let buf = self.buffers.get(&uri).expect("no buffer found for URI");
        let path = uri.to_file_path().map_err(|_| Error::NonFileUri)?;
        let contents = buf.contents();
        let mut map = redscript_ast::SourceMap::new();
        let id = map.add(path, contents.to_string());
        let file = map.get(id).unwrap();

        let settings = FormatSettings {
            indent: params.options.tab_size as u16,
            ..Default::default()
        };
        let (module, errors) = format_document(file.source(), id, &settings);
        if let (Some(module), []) = (module, &errors[..]) {
            let last_line = contents.len_lines() - 1;
            let edit = lsp::TextEdit::new(
                lsp::Range::new(
                    lsp::Position::new(0, 0),
                    lsp::Position::new(
                        last_line as u32,
                        contents.chars_at(contents.line_to_char(last_line)).count() as u32,
                    ),
                ),
                module.to_string(),
            );
            return Ok(Some(vec![edit]));
        };

        let mut msg = String::new();
        for err in errors {
            writeln!(msg, "{}", err.pretty(&map)).unwrap();
        }
        self.spawn_error_popup(format!("formatting failed:\n{msg}"))
            .await;

        Ok(None)
    }

    async fn get_cloned_pool(&self) -> Result<ConstantPool, Error> {
        let guard = self.state.read().await;
        let state = guard.as_ref().ok_or(Error::ServerNotInitialized)?;
        Ok(state.compiled_pool.clone())
    }

    async fn spawn_error_popup(&self, message: String) {
        let msg = lsp::ShowMessageParams {
            typ: lsp::MessageType::ERROR,
            message: message.clone(),
        };
        self.log_error(message).await;
        self.client
            .send_custom_notification::<lsp::notification::ShowMessage>(msg)
            .await;
    }

    async fn did_change_workspace_folders(
        &self,
        params: lsp::DidChangeWorkspaceFoldersParams,
    ) -> Result<(), Error> {
        for removed in params.event.removed {
            let Ok(path) = removed.uri.to_file_path() else {
                continue;
            };
            self.workspace_folders.write().await.remove(&path);
        }

        for added in params.event.added {
            let Ok(path) = added.uri.to_file_path() else {
                continue;
            };
            self.add_workspace_folder(path).await?;
        }

        Ok(())
    }

    async fn resolve_file_workspace(&self, url: &lsp::Url) -> Result<FileResolution, Error> {
        let folders = self.workspace_folders.read().await;
        let path = url.to_file_path().map_err(|_| Error::NonFileUri)?;
        if let Some((root, _)) = folders.iter().find(|(folder, _)| path.starts_with(folder)) {
            let files = Files::from_dirs(folders.values())?;
            Ok(FileResolution::Workspace(files, root.clone()))
        } else {
            Ok(FileResolution::NonWorkspace(Files::from_files([path])?))
        }
    }

    async fn log_info(&self, msg: impl fmt::Display) {
        self.client.log_message(lsp::MessageType::INFO, msg).await;
    }

    async fn log_error(&self, msg: impl fmt::Display) {
        self.client.log_message(lsp::MessageType::ERROR, msg).await;
    }
}

#[lspower::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        params: lsp::InitializeParams,
    ) -> jsonrpc::Result<lsp::InitializeResult> {
        if let Some(options) = params.initialization_options {
            let config: Config = serde_json::from_value(options)
                .map_err(|err| jsonrpc::Error::invalid_params(err.to_string()))?;
            self.config.set(config.into()).unwrap();
        }

        for dir in params
            .workspace_folders
            .unwrap_or_default()
            .iter()
            .filter_map(|dir| dir.uri.to_file_path().ok())
        {
            if let Err(err) = self.add_workspace_folder(dir).await {
                self.spawn_error_popup(err.to_string()).await;
            }
        }

        let completion = lsp::CompletionOptions {
            trigger_characters: Some(vec![".".to_owned()]),
            resolve_provider: Some(true),
            ..lsp::CompletionOptions::default()
        };

        let capabilities = lsp::ServerCapabilities {
            text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(
                lsp::TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(completion),
            hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
            definition_provider: Some(lsp::OneOf::Left(true)),
            document_formatting_provider: Some(lsp::OneOf::Left(true)),
            workspace: Some(lsp::WorkspaceServerCapabilities {
                workspace_folders: Some(lsp::WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(lsp::OneOf::Left(true)),
                }),
                file_operations: None,
            }),
            ..lsp::ServerCapabilities::default()
        };

        let result = lsp::InitializeResult {
            capabilities,
            server_info: None,
        };
        Ok(result)
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        match self.post_initialize().await {
            Err(err) => self.spawn_error_popup(err.to_string()).await,
            Ok(()) => self.log_info("redscript server initialized!").await,
        }
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.buffers.add(doc.uri, doc.text);
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        for change in params.content_changes {
            self.buffers.update(&params.text_document.uri, change);
        }
    }

    async fn did_save(&self, params: lsp::DidSaveTextDocumentParams) {
        let result = async {
            let outcome = self
                .resolve_file_workspace(&params.text_document.uri)
                .await?;
            let res = self.typecheck_workspace(outcome.files()).await?;
            Ok::<_, Error>((res, outcome))
        }
        .await;
        match result {
            Ok((TypecheckOutcome::Succeeded, FileResolution::Workspace(_, root))) => {
                self.execute_hooks(&self.hooks.read().await.successful_check, root)
                    .await;
            }
            Ok(_) => {}
            Err(err) => {
                self.log_info(format!("typecheck reported an error: {err}"))
                    .await;
            }
        }
    }

    async fn completion(
        &self,
        params: lsp::CompletionParams,
    ) -> jsonrpc::Result<Option<lsp::CompletionResponse>> {
        Ok(self.completion(params).await?)
    }

    async fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
    ) -> jsonrpc::Result<lsp::CompletionItem> {
        Ok(self.completion_resolve(item).await?)
    }

    async fn hover(&self, params: lsp::HoverParams) -> jsonrpc::Result<Option<lsp::Hover>> {
        Ok(self.hover(params).await?)
    }

    async fn goto_definition(
        &self,
        params: lsp::GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<lsp::GotoDefinitionResponse>> {
        Ok(self.goto_definition(params).await?)
    }

    async fn formatting(
        &self,
        params: lsp::DocumentFormattingParams,
    ) -> jsonrpc::Result<Option<Vec<lsp::TextEdit>>> {
        Ok(self.format(params).await?)
    }

    async fn did_change_workspace_folders(&self, params: lsp::DidChangeWorkspaceFoldersParams) {
        if let Err(err) = self.did_change_workspace_folders(params).await {
            self.spawn_error_popup(err.to_string()).await;
        }
    }
}

#[derive(Debug)]
enum TypecheckOutcome {
    Failed,
    Succeeded,
    NotReady,
}

#[derive(Debug)]
enum FileResolution {
    Workspace(Files, PathBuf),
    NonWorkspace(Files),
}

impl FileResolution {
    pub fn files(&self) -> &Files {
        match self {
            Self::Workspace(files, _) | Self::NonWorkspace(files) => files,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_dot_redscript() {
        let input = r#"
        redscript_dir = "r6"

        [[hooks.successful_check]]
        create_file = "/foo/bar"

        [[hooks.successful_check]]
        create_file = "/bar"
        "#;

        let res: DotRedscript = toml::from_str(input).unwrap();
        assert_eq!(res.redscript_dir, Some(PathBuf::from("r6")));
        assert_eq!(
            res.hooks.successful_check,
            vec![
                Hook::CreateFile("/foo/bar".to_owned()),
                Hook::CreateFile("/bar".to_owned())
            ]
        );
    }
}
