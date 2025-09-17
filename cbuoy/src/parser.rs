use jib_asm::AsmTokenLoc;

use crate::{
    compiler::CompilingState,
    functions::{AsmFunctionDefinition, StandardFunctionDefinition, StandardFunctionType},
    tokenizer::{
        KEYWORD_ASMFN, KEYWORD_CONST, KEYWORD_FN, KEYWORD_FNINT, KEYWORD_GLOBAL, KEYWORD_STRUCT,
        KEYWORD_USING, TokenError, TokenIter, tokenize,
    },
    typing::{StructDefinition, Type},
    variables::VariableDefinition,
};

pub fn parse(s: &str) -> Result<Vec<AsmTokenLoc>, TokenError> {
    let mut state = CompilingState::default();
    let tokens = tokenize(s)?;
    let mut token_iter = TokenIter::from(&tokens);

    while let Some(next) = token_iter.peek().map(|v| v.get_value().to_string()) {
        if next == KEYWORD_GLOBAL {
            let var = VariableDefinition::parse(KEYWORD_GLOBAL, &mut token_iter, &mut state)?;
            state.add_global_var(var)?;
        } else if next == KEYWORD_CONST {
            let var = VariableDefinition::parse(KEYWORD_CONST, &mut token_iter, &mut state)?;
            state.add_const_var(var)?;
        } else if next == KEYWORD_FN {
            StandardFunctionDefinition::parse(
                &mut token_iter,
                &mut state,
                StandardFunctionType::Default,
            )?;
        } else if next == KEYWORD_FNINT {
            StandardFunctionDefinition::parse(
                &mut token_iter,
                &mut state,
                StandardFunctionType::Interrupt,
            )?
        } else if next == KEYWORD_ASMFN {
            AsmFunctionDefinition::parse(&mut token_iter, &mut state)?;
        } else if next == KEYWORD_STRUCT {
            StructDefinition::read_definition(&mut token_iter, &mut state)?;
        } else if next == KEYWORD_USING {
            token_iter.expect(KEYWORD_USING)?;
            let alias_token = token_iter.next()?;

            token_iter.expect("=")?;

            let type_val = Type::read_type(&mut token_iter, &mut state)?;

            token_iter.expect(";")?;

            state.add_type_alias(alias_token, type_val)?;
        } else {
            return Err(token_iter
                .next()?
                .clone()
                .into_err("unknown token provided"));
        }
    }

    state.get_assembler()
}

#[cfg(test)]
mod tests {
    use crate::{
        literals::{Literal, LiteralValue},
        tokenizer::{Token, TokenLocation},
    };

    #[test]
    fn test_literal_parsing() {
        let tokens = [
            ("0u32", Some(LiteralValue::U32(0))),
            ("23834i8", None),
            ("1i8", Some(LiteralValue::I8(1))),
            ("23834i16", Some(LiteralValue::I16(23834))),
            ("394i23", None),
            ("38905u32", Some(LiteralValue::U32(38905))),
            ("389u35", None),
            ("3.234i8", None),
            ("3.2f32", Some(LiteralValue::F32(3.2))),
            (".3f32", Some(LiteralValue::F32(0.3))),
            ("32.34f32", Some(LiteralValue::F32(32.34))),
            ("32.34", Some(LiteralValue::F32(32.34))),
            ("394", Some(LiteralValue::I32(394))),
        ];

        for (s, expected) in tokens {
            let t = Token::new(s, TokenLocation { line: 0, column: 0 });
            let lit: Result<Literal, _> = t.try_into();

            println!("{:?} <=> {:?}", lit, expected);
            assert_eq!(lit.is_ok(), expected.is_some());
            if let Ok(v) = lit {
                if let Some(ev) = expected {
                    assert_eq!(v.get_value().get_dtype(), ev.get_dtype());
                    assert_eq!(v.get_value().as_u32(), ev.as_u32());
                } else {
                    panic!("unexpected value mismatch");
                }
            }
        }
    }
}
