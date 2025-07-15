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

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_get_values_should_return_all_enum_vals() {
        let string_val = Target::get_values_string();
        assert_eq!(string_val, "Arm64");
    }

    #[test]
    fn test_display_should_display_enum() {
        let test_cases = vec![(Target::ARM64, "Arm64")];
        for (input, expected) in test_cases {
            assert_eq!(input.to_string(), expected);
        }
    }

    #[test]
    fn test_from_str_should_convert_valid_target() {
        let test_cases = vec![("Arm64", Target::ARM64), ("arm64", Target::ARM64)];
        for (input, expected) in test_cases {
            assert_eq!(Target::from_str(input).unwrap(), expected);
        }
    }

    #[test]
    fn test_from_str_should_err_for_invalid_target() {
        Target::from_str("invalid").unwrap_err();
    }
}
