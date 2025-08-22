
use std::io::{self, prelude::*};

use crate::lexer::Lexer;
use crate::parser::Parser;

mod lexer;
mod token;
mod parser;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

fn main() -> Result<()> {
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut contents = String::new();
        io::stdin().read_line(&mut contents)?;
        let contents = contents.trim().to_string();

        if contents.is_empty() {
            continue;
        } else if contents == "exit" || contents == "quit" {
            break;
        }

        let tokens = match Lexer::lex_all(contents) {
            Ok(tokens) => tokens,
            Err(e) => {
                eprintln!("Lexing error: {e}");
                continue;
            }
        };

        let mut p = Parser::new(tokens);
        let expr = p.parse_expr()?;

        println!("{}", expr);  
        println!("{}", expr.eval());  
    }

    Ok(())
}

