#[derive(Debug)]
pub struct Diagnostic {
    pub message: String,
    pub severity: Severity,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Diagnostic {
        Self {
            message: message.into(),
            severity: Severity::Error,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Severity {
    Error,
}
