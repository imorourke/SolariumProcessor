use alloc::fmt;

#[derive(Copy, Clone, Debug)]
pub enum CharacterError {
    CharacterToByte(char),
    ByteToCharacter(u8),
}

impl fmt::Display for CharacterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CharacterToByte(c) => write!(f, "unable to convert {:02X} to word", *c as u8),
            Self::ByteToCharacter(b) => write!(f, "unable to convert {:02X} to character", b),
        }
    }
}

const CHAR_NULL: u8 = b'\0';
const CHAR_NEWLINE: u8 = b'\n';
const CHAR_TAB: u8 = b'\t';

/// Converts an input character into a memory-word supported by the SProc
pub fn character_to_byte(c: char) -> Result<u8, CharacterError> {
    let char_val: u8 = match c as u8 {
        CHAR_NULL => CHAR_NULL,
        CHAR_NEWLINE => CHAR_NEWLINE,
        CHAR_TAB => CHAR_TAB,
        0x20..=0x7E => c as u8,
        _ => return Err(CharacterError::CharacterToByte(c)),
    };

    Ok(char_val)
}

/// Converts a memory word into a text character
pub fn byte_to_character(b: u8) -> Result<char, CharacterError> {
    let char_val = match b {
        CHAR_NULL => '\0',
        CHAR_NEWLINE => '\n',
        CHAR_TAB => '\t',
        0x20..=0x7E => b as char,
        _ => return Err(CharacterError::ByteToCharacter(b)),
    };

    Ok(char_val)
}
