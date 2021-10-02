use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("redscript error: {0}")]
    ScriptError(#[from] redscript::error::Error),
    #[error("JsonRPC error: {0}")]
    JsonRpcError(#[from] lspower::jsonrpc::Error),
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("server error: {0}")]
    ServerError(String),
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
