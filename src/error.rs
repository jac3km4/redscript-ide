use std::io;
use std::path::PathBuf;

use redscript::bundle::PoolError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("compiler error: {0}")]
    Compile(#[from] redscript_compiler::error::Error),
    #[error("no workspace open, redscript LS requires a workspace to work")]
    NoWorkspaceOpen,
    #[error("non-file URI was encountered, redscript LS only supports local files")]
    NonFileUri,
    #[error("redscript source directory at {0} is invalid")]
    InvalidRedscriptSourceDir(PathBuf),
    #[error("configuration for redscript LS is missing, make sure it exists in User Preferences")]
    MissingConfiguration,
    #[error("redscript cache file not found at {0}, the extension is configured incorrectly")]
    RedscriptCacheNotFound(PathBuf),
    #[error("parsing the .redscript file at {0} failed")]
    DotRedscriptParseError(toml::de::Error),
    #[error("attempted to access the server while it was not initialized")]
    ServerNotInitialized,
    #[error("unexpected error: {0}")]
    Other(Box<dyn std::error::Error + Send + Sync>),
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Other(Box::new(value))
    }
}

impl From<PoolError> for Error {
    fn from(value: PoolError) -> Self {
        Self::Other(Box::new(value))
    }
}

impl From<Error> for lspower::jsonrpc::Error {
    fn from(err: Error) -> Self {
        Self {
            code: lspower::jsonrpc::ErrorCode::ServerError(1),
            message: err.to_string(),
            data: None,
        }
    }
}
