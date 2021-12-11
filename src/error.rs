use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("compiler error: {0}")]
    Compile(#[from] redscript_compiler::error::Error),
    #[error("pool error: {0}")]
    Pool(#[from] redscript::bundle::PoolError),
    #[error("JsonRPC error: {0}")]
    JsonRpc(#[from] lspower::jsonrpc::Error),
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("server error: {0}")]
    Server(String),
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
