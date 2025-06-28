use clap::builder::TypedValueParser;
use clap::error::{ContextKind, ContextValue, Error, ErrorKind};
use std::ffi::OsStr;

#[derive(Clone)]
pub struct HexParser {}

impl TypedValueParser for HexParser {
    type Value = u32;
    fn parse_ref(
        &self,
        cmd: &clap::Command,
        arg: Option<&clap::Arg>,
        value: &OsStr,
    ) -> Result<Self::Value, Error> {
        value
            .to_str()
            .ok_or_else(|| {
                let mut err = Error::new(ErrorKind::ValueValidation).with_cmd(cmd);
                err.insert(ContextKind::InvalidValue, ContextValue::None);
                err
            })
            .and_then(|s| {
                let mut err = Error::new(ErrorKind::ValueValidation).with_cmd(cmd);
                if let Some(arg) = arg {
                    err.insert(
                        ContextKind::InvalidArg,
                        ContextValue::String(arg.to_string()),
                    );
                }
                err.insert(
                    ContextKind::InvalidValue,
                    ContextValue::String(s.to_string()),
                );
                err.insert(
                    ContextKind::Suggested,
                    ContextValue::StyledStrs(vec![
                        "This argument only supports hexadecimal (i.e. 0x10, 0X10, #10) or decimal (i.e. 16) values".into(),
                    ]),
                );

                // Let's try a few different types!
                // 1. Hexadecimal (0x , 0X, or #)
                if s.starts_with("0x") || s.starts_with("0X") || s.starts_with('#') {
                    u32::from_str_radix(&s[2..], 16).map_err(|_| err)
                }
                // 2. Decimal
                else if let Ok(value) = s.parse::<u32>() {
                    Ok(value)
                } else {
                    Err(err)
                }
            })
    }
}
