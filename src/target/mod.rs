#[cfg(test)]
mod tests;

#[derive(Debug, Eq, PartialEq)]
pub enum Target {
    ARM64,
}

#[derive(Debug)]
pub struct TargetParseError;

impl Target {
    pub fn get_values_string() -> String {
        vec![Target::ARM64]
            .iter()
            .map(|x| x.to_string() + ",")
            .collect::<String>()
            .trim_end_matches(",")
            .to_string()
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::ARM64 => write!(f, "Arm64"),
        }
    }
}

impl std::str::FromStr for Target {
    type Err = TargetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "arm64" => Ok(Target::ARM64),
            _ => Err(TargetParseError),
        }
    }
}
