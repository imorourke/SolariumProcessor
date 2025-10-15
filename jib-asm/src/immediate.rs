use core::fmt;

#[derive(Debug, Clone)]
pub struct ImmediateError(pub String);

impl From<jib::text::CharacterError> for ImmediateError {
    fn from(value: jib::text::CharacterError) -> Self {
        Self(format!("{value}"))
    }
}

impl fmt::Display for ImmediateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Immediate Error => {}", self.0)
    }
}

macro_rules! gen_read_immediate {
    ($fnname:ident, $t:ident) => {
        pub fn $fnname(arg: &str) -> Result<$t, ImmediateError> {
            let res = if let Some(i) = arg.strip_prefix("0x") {
                $t::from_str_radix(i, 16)
            } else if let Some(start) = arg.strip_prefix('\'')
                && let Some(c) = start.strip_suffix('\'')
            {
                if let Some(rest) = c.strip_prefix('\\') {
                    if rest == "n" {
                        Ok(jib::text::character_to_byte('\n')? as $t)
                    } else {
                        return Err(ImmediateError(format!("unknown escape character {c}")));
                    }
                } else if c.len() == 1
                    && let Some(val) = c.chars().next()
                {
                    Ok(jib::text::character_to_byte(val)? as $t)
                } else {
                    return Err(ImmediateError(format!(
                        "unable to convert character '{c}' to value"
                    )));
                }
            } else {
                arg.parse::<$t>()
            };

            match res {
                Ok(v) => Ok(v),
                Err(_) => Err(ImmediateError(arg.to_string())),
            }
        }
    };
}

gen_read_immediate!(parse_imm_i8, i8);
gen_read_immediate!(parse_imm_u8, u8);
gen_read_immediate!(parse_imm_i16, i16);
gen_read_immediate!(parse_imm_u16, u16);
gen_read_immediate!(parse_imm_i32, i32);
gen_read_immediate!(parse_imm_u32, u32);
