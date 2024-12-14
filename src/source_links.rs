use std::collections::HashMap;
use std::ops::Not;
use std::path::Path;

use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Definition};
use redscript_compiler::source_map::Files;
use redscript_compiler::unit::TypecheckOutput;

pub struct SourceLinks {
    path_table: intaglio::path::SymbolTable,
    name_table: intaglio::SymbolTable,

    source_items: HashMap<SourceLinkKey, SourceLink>,
}

impl SourceLinks {
    pub fn new(pool: &ConstantPool, output: &TypecheckOutput, files: &Files) -> Self {
        let mut this = Self {
            path_table: intaglio::path::SymbolTable::new(),
            name_table: intaglio::SymbolTable::new(),
            source_items: HashMap::new(),
        };

        for sr in output.source_refs() {
            let Some(file) = files.lookup_file(sr.pos()) else {
                continue;
            };
            let Some(pos) = file.lookup(sr.pos()) else {
                continue;
            };
            let item = SourceLink {
                path: this.path_table.intern(file.path().to_owned()).unwrap(),
                line: pos.line,
            };
            let Some(key) = this.create_link_key(sr.index(), pool) else {
                continue;
            };
            this.source_items.insert(key, item);
        }
        this
    }

    pub fn get(&self, key: SourceLinkKey) -> Option<SourceLinkPos<'_>> {
        let link = self.source_items.get(&key)?;
        self.resolve_link(link)
    }

    fn resolve_link(&self, key: &SourceLink) -> Option<SourceLinkPos<'_>> {
        let path = self.path_table.get(key.path)?;
        Some(SourceLinkPos {
            path,
            line: key.line,
        })
    }

    fn render_link(&self, key: &SourceLinkKey) -> Option<String> {
        let name = self.name_table.get(key.name())?;
        let name = name.split_once(';').map_or(name, |(pre, _)| pre);
        match key.parent() {
            Some(parent) => Some(format!("{}::{name}", self.name_table.get(parent)?)),
            None => Some(name.to_owned()),
        }
    }

    pub fn create_link_key(
        &mut self,
        idx: PoolIndex<Definition>,
        pool: &ConstantPool,
    ) -> Option<SourceLinkKey> {
        let def = pool.definition(idx).ok()?;
        let name = pool.names.get(def.name).ok()?;
        let parent_name = def
            .parent
            .is_undefined()
            .not()
            .then(|| pool.def_name(def.parent).ok())
            .flatten();

        let result = match (&def.value, parent_name) {
            (AnyDefinition::Class(_), _) => {
                SourceLinkKey::Class(self.name_table.intern(name.as_ref().to_owned()).unwrap())
            }
            (AnyDefinition::Enum(_), _) => {
                SourceLinkKey::Enum(self.name_table.intern(name.as_ref().to_owned()).unwrap())
            }
            (AnyDefinition::Function(_), Some(parent_name)) => SourceLinkKey::Method(
                self.name_table.intern(name.as_ref().to_owned()).unwrap(),
                self.name_table
                    .intern(parent_name.as_ref().to_owned())
                    .unwrap(),
            ),
            (AnyDefinition::Function(_), None) => {
                SourceLinkKey::Function(self.name_table.intern(name.as_ref().to_owned()).unwrap())
            }
            (AnyDefinition::Field(_), Some(parent_name)) => SourceLinkKey::Field(
                self.name_table.intern(name.as_ref().to_owned()).unwrap(),
                self.name_table
                    .intern(parent_name.as_ref().to_owned())
                    .unwrap(),
            ),
            _ => return None,
        };
        Some(result)
    }

    pub fn get_link_key(
        &self,
        idx: PoolIndex<Definition>,
        pool: &ConstantPool,
    ) -> Option<SourceLinkKey> {
        let def = pool.definition(idx).ok()?;
        let name = pool.names.get(def.name).ok()?;
        let parent_name = def
            .parent
            .is_undefined()
            .not()
            .then(|| pool.def_name(def.parent).ok())
            .flatten();

        let result = match (&def.value, parent_name) {
            (AnyDefinition::Class(_), _) => {
                SourceLinkKey::Class(self.name_table.check_interned(name.as_ref())?)
            }
            (AnyDefinition::Enum(_), _) => {
                SourceLinkKey::Enum(self.name_table.check_interned(name.as_ref())?)
            }
            (AnyDefinition::Function(_), Some(parent_name)) => SourceLinkKey::Method(
                self.name_table.check_interned(name.as_ref())?,
                self.name_table.check_interned(parent_name.as_ref())?,
            ),
            (AnyDefinition::Function(_), None) => {
                SourceLinkKey::Function(self.name_table.check_interned(name.as_ref())?)
            }
            (AnyDefinition::Field(_), Some(parent_name)) => SourceLinkKey::Field(
                self.name_table.check_interned(name.as_ref())?,
                self.name_table.check_interned(parent_name.as_ref())?,
            ),
            _ => return None,
        };
        Some(result)
    }

    pub fn search<'a>(
        &'a self,
        query: &'a str,
    ) -> impl Iterator<Item = (String, SourceLinkKind, SourceLinkPos<'a>)> {
        self.source_items.iter().filter_map(move |(key, link)| {
            let name = self.name_table.get(key.name())?;
            let pos = name.contains(query).then(|| self.resolve_link(link))??;
            let name = self.render_link(key)?;
            Some((name, key.kind(), pos))
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum SourceLinkKind {
    Class,
    Enum,
    Function,
    Method,
    Field,
}

#[derive(Debug)]
struct SourceLink {
    path: intaglio::Symbol,
    line: usize,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum SourceLinkKey {
    Class(intaglio::Symbol),
    Enum(intaglio::Symbol),
    Function(intaglio::Symbol),
    Method(intaglio::Symbol, intaglio::Symbol),
    Field(intaglio::Symbol, intaglio::Symbol),
}

impl SourceLinkKey {
    #[inline]
    pub fn name(&self) -> intaglio::Symbol {
        match self {
            Self::Class(name)
            | Self::Enum(name)
            | Self::Function(name)
            | Self::Method(name, _)
            | Self::Field(name, _) => *name,
        }
    }

    #[inline]
    pub fn kind(&self) -> SourceLinkKind {
        match self {
            Self::Class(_) => SourceLinkKind::Class,
            Self::Enum(_) => SourceLinkKind::Enum,
            Self::Function(_) => SourceLinkKind::Function,
            Self::Method(_, _) => SourceLinkKind::Method,
            Self::Field(_, _) => SourceLinkKind::Field,
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<intaglio::Symbol> {
        match self {
            Self::Method(_, parent) | Self::Field(_, parent) => Some(*parent),
            _ => None,
        }
    }
}

pub struct SourceLinkPos<'a> {
    path: &'a Path,
    line: usize,
}

impl SourceLinkPos<'_> {
    pub fn path(&self) -> &Path {
        self.path
    }

    pub fn line(&self) -> usize {
        self.line
    }
}
