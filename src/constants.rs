use regex::Regex;
use std::sync::LazyLock;

const VARIABLE_PATTERN: &'static str = r"^[_a-zA-Z](?:[\w\-]*[a-zA-Z0-9])?$";
const VALUE_PATTERN: &'static str = r#"^(?:\d+(?:\.\d+)?|".*")$"#;

pub static VARIABLE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(VARIABLE_PATTERN).unwrap());
pub static VALUE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(VALUE_PATTERN).unwrap());
