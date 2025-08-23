#![allow(unused, dead_code)]

use std::io::{self, prelude::*};
use std::fs;
use std::env;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;

mod lexer;
mod token;
mod parser;
mod ast;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        
        let mut contents = String::new();
        io::stdin().read_line(&mut contents).unwrap();
        let mut contents = contents.trim().to_string();

        if contents.is_empty() {
            continue;
        } else if contents == "exit" || contents == "quit" {
            break;
        }

        contents.push('\n');
        let tokens = match Lexer::lex_all(contents) {
            Ok(tokens) => tokens,
            Err(e) => {
                eprintln!("Lexing error: {e}");
                continue;
            }
        };

        let mut p = Parser::new(tokens);

        let stmts = match p.parse() {
            Ok(stmts) => stmts,
            Err(err) => {
                println!("aaa");
                println!("{err}");
                continue;
            }
        };

        for stmt in stmts {
            if let Err(e) = stmt.exec() {
                eprintln!("{e}");
            }
        }
        // let expr = match p.parse_expr() {
        //     Ok(expr) => expr,
        //     Err(err) => {
        //         println!("{err}");
        //         continue;
        //     }
        // };
        //
        // match expr.eval() {
        //     Ok(res) => {
        //         println!("{res}");  
        //         println!("{expr}");  
        //     },
        //     Err(err) => eprintln!("{err}")
        // }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if (args.len() == 1) {
        repl();
    } else if (args.len() == 2) {
        let filename = args[1].to_string();
        let contents = fs::read_to_string(filename)?; 
        
        let tokens = Lexer::lex_all(contents)?;
        let mut p = Parser::new(tokens);
        let expr = p.parse_expr()?;
        println!("{}", expr.eval()?);
    } else {
        return Err("too many arguments provided".into());
    }

    Ok(())
}

