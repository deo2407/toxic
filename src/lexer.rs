use crate::token::{TokenType, Token};

pub struct Lexer {
    chars: Vec<char>,
    start: usize,      // start of current token
    current: usize,    // current position in source
    line: usize,
}

impl Lexer {
    fn new(source: String) -> Self {
        Self {
            chars: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn lex_all(source: String) -> Result<Vec<Token>, String> {
        let mut lexer = Lexer::new(source.to_string());  
        let mut tokens = Vec::new(); 

        loop {
            let token = lexer.scan_token()?;
            tokens.push(token.clone());

            if token.token_type == TokenType::EOF {
                break;
            }
        }
        Ok(tokens)
    }

    fn scan_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace(); 
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::EOF);
        }

        let c = self.advance();

        if Self::is_digit(c) {
            return self.number();
        }

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '+' => self.make_token(TokenType::Plus),
            '-' => self.make_token(TokenType::Minus),
            '*' => self.make_token(TokenType::Multiply),
            '/' => self.make_token(TokenType::Divide),
            '^' => self.make_token(TokenType::Power),
            '<' => {
                if self.match_char('=') {
                    return self.make_token(TokenType::LessEq);
                }
                self.make_token(TokenType::Less)
            },
            '>' => {
                if self.match_char('=') {
                    return self.make_token(TokenType::GreaterEq);
                }
                self.make_token(TokenType::Greater)
            },
            '!' => {
                if self.match_char('=') {
                    return self.make_token(TokenType::BangEq);
                }
                Err(format!("Unknow token {c}"))
            },
            '=' => {
                if self.match_char('=') {
                    return self.make_token(TokenType::EqualEq);
                }
                Err(format!("Unknow token {c}"))
            },
            _ => return Err(format!("Unknow token {c}")),
        }
    }

    fn make_token(&self, token_type: TokenType) -> Result<Token, String> {
        Ok(Token {
            token_type,
            lexeme: self.get_lexeme(),
            line: self.line,
        })
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                },
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                },
                Some('/') => {
                    // consume comments
                    if self.peek_next() == Some('/') {
                        while let Some(ch) = self.peek() {
                            if ch == '\n' { break; }
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        } 
    }

    fn number(&mut self) -> Result<Token, String> {
        while Self::is_digit(self.peek().unwrap_or(' ')) { self.advance(); }

        if self.peek().unwrap_or(' ') == '.' {
            self.advance();
            while Self::is_digit(self.peek().unwrap_or(' ')) { self.advance(); }
        }

        let num_str = self.chars[self.start..self.current]
            .iter()
            .collect::<String>();
        let num: f64 = num_str.parse().unwrap();

        self.make_token(TokenType::Number(num))
    }

    fn match_char(&mut self, expect: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.chars[self.current] != expect {
            return false;
        }
        self.current += 1;

        true
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        let ch = self.chars[self.current]; 
        self.current += 1;
        ch
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.current).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.current + 1).copied()
    }

    fn get_lexeme(&self) -> String {
        self.chars[self.start..self.current].iter().collect()
    }

    fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{TokenType, Token};

    #[test]
    fn lex_simple_expression() {
        let source = "1 + 2 * (3 - 4)".to_string();
        let tokens = Lexer::lex_all(source).unwrap();

        let expected = vec![
            Token { token_type: TokenType::Number(1.0), lexeme: "1".to_string(), line: 1 },
            Token { token_type: TokenType::Plus, lexeme: "+".to_string(), line: 1 },
            Token { token_type: TokenType::Number(2.0), lexeme: "2".to_string(), line: 1 },
            Token { token_type: TokenType::Multiply, lexeme: "*".to_string(), line: 1 },
            Token { token_type: TokenType::LeftParen, lexeme: "(".to_string(), line: 1 },
            Token { token_type: TokenType::Number(3.0), lexeme: "3".to_string(), line: 1 },
            Token { token_type: TokenType::Minus, lexeme: "-".to_string(), line: 1 },
            Token { token_type: TokenType::Number(4.0), lexeme: "4".to_string(), line: 1 },
            Token { token_type: TokenType::RightParen, lexeme: ")".to_string(), line: 1 },
            Token { token_type: TokenType::EOF, lexeme: "".to_string(), line: 1 },
        ];

        assert_eq!(tokens.len(), expected.len());
        for (tok, exp) in tokens.iter().zip(expected.iter()) {
            assert_eq!(tok.token_type, exp.token_type);
            assert_eq!(tok.lexeme, exp.lexeme);
            assert_eq!(tok.line, exp.line);
        }
    }

    #[test]
    fn lex_whitespace_and_newlines() {
        let source = " 1\n+ 2 ".to_string();
        let tokens = Lexer::lex_all(source).unwrap();

        assert_eq!(tokens[0].token_type, TokenType::Number(1.0));
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Number(2.0));
        assert_eq!(tokens[3].token_type, TokenType::EOF);
        assert_eq!(tokens[2].line, 2);
    }

    #[test]
    fn lex_comments() {
        let source = "1 // this is a comment\n+ 2".to_string();
        let tokens = Lexer::lex_all(source).unwrap();

        assert_eq!(tokens[0].token_type, TokenType::Number(1.0));
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Number(2.0));
        assert_eq!(tokens[3].token_type, TokenType::EOF);
        assert_eq!(tokens[2].line, 2); 
    }

    #[test]
    fn lex_float() {
        let source = "1.321".to_string();
        let tokens = Lexer::lex_all(source).unwrap();

        assert_eq!(tokens[0].token_type, TokenType::Number(1.321));
    }

    #[test]
    fn lex_double_char_tokens() {
        let source = ">= <= != ==".to_string();
        let tokens = Lexer::lex_all(source).unwrap();

        assert_eq!(tokens[0].token_type, TokenType::GreaterEq);
        assert_eq!(tokens[1].token_type, TokenType::LessEq);
        assert_eq!(tokens[2].token_type, TokenType::BangEq);
        assert_eq!(tokens[3].token_type, TokenType::EqualEq);
    }
}

