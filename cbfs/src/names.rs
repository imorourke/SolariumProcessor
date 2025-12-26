#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum StringArrayError {
    InvalidName,
}

pub fn string_to_array<const N: usize, T: AsRef<str>>(s: T) -> Result<[u8; N], StringArrayError> {
    let mut vals = [0; N];

    if s.as_ref().len() > vals.len() {
        return Err(StringArrayError::InvalidName);
    }

    for (v, c) in vals.iter_mut().zip(s.as_ref().chars()) {
        *v = c as u8;
    }

    Ok(vals)
}

pub fn array_to_string(val: &[u8]) -> String {
    val.iter()
        .take_while(|c| **c != 0)
        .map(|c| *c as char)
        .collect::<String>()
}
