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
        let path = self.path_table.get(link.path)?;
        Some(SourceLinkPos {
            path,
            line: link.line,
        })
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
}

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
