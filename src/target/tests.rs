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
