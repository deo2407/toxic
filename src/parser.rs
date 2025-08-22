use std::fmt;

use crate::token::*;

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Unary { op: TokenType, rhs: Box<Expr> },
    Binary { lhs: Box<Expr>, op: TokenType, rhs: Box<Expr> },
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn eval(&self) -> f64 {
        match self {
            Expr::Number(n) => *n,
            Expr::Binary { lhs, op, rhs } => {
                let left = lhs.eval();
                let right = rhs.eval();
                match op {
                    TokenType::Plus => left + right,
                    TokenType::Minus => left - right,
                    TokenType::Multiply => left * right,
                    TokenType::Divide => left / right,
                    TokenType::Power => left.powf(right.try_into().unwrap()),
                    TokenType::EqualEq => (left == right) as u8 as f64,
                    TokenType::BangEq => (left != right) as u8 as f64,
                    TokenType::Greater => (left > right) as u8 as f64,
                    TokenType::GreaterEq => (left >= right) as u8 as f64,
                    TokenType::Less => (left < right) as u8 as f64,
                    TokenType::LessEq => (left <= right) as u8 as f64,
                    _ => panic!("Unsupported binary operator")
                }
            },
            Expr::Unary {op, rhs} => {
                let val = rhs.eval();
                match op {
                    TokenType::Minus => -val,
                    _ => panic!("Unsupported unary operator")
                }
            },
            Expr::Grouping(inner) => inner.eval(),
        } 
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Unary { op, rhs } => write!(f, "{}{}", op, rhs),
            Expr::Binary { lhs, op, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {})", expr)
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_expr_bp(0.0)
    }

    fn parse_expr_bp(&mut self, min_bp: f32) -> Result<Expr, String> {
        let token = self.advance();
        let token_type = token.token_type;

        let mut lhs = match &token.token_type {
            TokenType::Number(n) => Expr::Number(*n),
            TokenType::LeftParen => {
                let expr = self.parse_expr_bp(0.0)?;
                self.consume(TokenType::RightParen, "Expect ')' after expression")?;
                Expr::Grouping(Box::new(expr))
            },
            TokenType::Minus | TokenType::Plus => {
                let ((), r_bp) = Self::prefix_bindind_power(&token.token_type);
                let rhs = self.parse_expr_bp(r_bp)?;
                Expr::Unary { op: token_type, rhs: Box::new(rhs)}
            },
            _ => return Self::report_unexpected_token(&token),
        };

        loop {
            let op = match self.match_operator() {
                Some(op) => op,
                None => match &self.peek().token_type {
                    TokenType::EOF | TokenType::RightParen => break,
                    _ => return Self::report_unexpected_token(self.peek()),
                },
            };

            if let Some((l_bp, r_bp)) = Self::infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                self.advance(); // consume operator
                let rhs = self.parse_expr_bp(r_bp)?;
                lhs = Expr::Binary {
                    lhs: Box::new(lhs),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                };
                continue;
            } else {
                return Self::report_unexpected_token(self.advance()); 
            }
        }

        Ok(lhs)
    }
    
    fn prefix_bindind_power(op: &TokenType) -> ((), f32) {
        let res = match op {
            TokenType::Plus | TokenType::Minus => ((), 9.0),
            _ =>  panic!("bad token '{}'", op),
        };
        res
    }

    fn infix_binding_power(op: &TokenType) -> Option<(f32, f32)> {
        let res = match op {
            TokenType::EqualEq | TokenType::BangEq => (0.5, 0.6), 
            TokenType::Less | TokenType::LessEq | TokenType::Greater | TokenType::GreaterEq => (0.7, 0.8),
            TokenType::Plus | TokenType::Minus => (1.0, 1.1),
            TokenType::Multiply | TokenType::Divide => (2.0, 2.1),
            TokenType::Power => (4.1, 4.0),
            _ => return None,
        };
        Some(res)
    }

    fn report_unexpected_token(token: &Token) -> Result<Expr, String> {
        Err(format!(
            "[line {}] Unexpected token {}",
            token.line,
            token.lexeme
        ))
    }

    fn match_operator(&mut self) -> Option<TokenType> {
        match self.peek().token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Multiply
            | TokenType::Divide
            | TokenType::Power
            | TokenType::EqualEq 
            | TokenType::BangEq 
            | TokenType::Greater 
            | TokenType::GreaterEq
            | TokenType::Less
            | TokenType::LessEq => {
                let tok = self.peek().token_type.clone();
                Some(tok)
            }
            _ => None,
        }
    }

    fn consume(&mut self, token_type: TokenType, error_msg: &str) -> Result<&Token, String> {
        if self.peek().token_type == token_type {
            Ok(self.advance())
        } else {
            Err(format!(
                "[line {}] Error at '{}': {}",
                self.peek().line,
                self.peek().lexeme,
                error_msg,
            ))
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn display_number() {
        let expr = Expr::Number(53.0); 
        assert_eq!(expr.to_string(), "53");
    }

    #[test]
    fn display_unary() {
        let expr = Expr::Unary { 
            op: TokenType::Minus, 
            rhs: Box::new(Expr::Number(43.0)),
        };
        assert_eq!(expr.to_string(), "-43");
    }

    #[test]
    fn display_binary() {
        let expr = Expr::Binary { 
            lhs: Box::new(Expr::Number(72.0)),
            op: TokenType::Plus, 
            rhs: Box::new(Expr::Number(43.0)),
        };
        assert_eq!(expr.to_string(), "(+ 72 43)");
    }

    #[test]
    fn display_nested() {
        // (1 + 2) * -3
        // ( * (group (+ 1 2)) -3)
        let expr = Expr::Binary {
            lhs: Box::new(Expr::Grouping(Box::new(Expr::Binary {
                lhs: Box::new(Expr::Number(1.0)),
                op: TokenType::Plus,
                rhs: Box::new(Expr::Number(2.0)),
            }))),
            op: TokenType::Multiply,
            rhs: Box::new(Expr::Unary {
                op: TokenType::Minus,
                rhs: Box::new(Expr::Number(3.0)),
            }),
        };

        assert_eq!(expr.to_string(), "(* (group (+ 1 2)) -3)");
    }

    #[test]
    fn parse_expr_add() {
        let tokens = Lexer::lex_all("1 + 2".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();
        assert_eq!(expr.to_string(), "(+ 1 2)") 
    }

    #[test]
    fn parse_expr_mul() {
        let tokens = Lexer::lex_all("41 * 2".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();
        assert_eq!(expr.to_string(), "(* 41 2)") 
    }

    #[test]
    fn parse_expr_mul_error() {
        let tokens = Lexer::lex_all("* 41".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr();

        assert!(expr.is_err());
        assert!(expr.unwrap_err().contains("Unexpected token"));
    }

    #[test]
    fn parse_expr_unary() {
        let tokens = Lexer::lex_all("- 41".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.to_string(), "-41");
    }

    #[test]
    fn parse_expr_unary_nested() {
        let tokens = Lexer::lex_all("- (31 + 12 + 13)".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.to_string(), "-(group (+ (+ 31 12) 13))");
    }

    #[test]
    fn parse_expr_unary_nested_bracket_err() {
        let tokens = Lexer::lex_all("- (31 + 12 + 13".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr();

        assert!(expr.is_err());
        assert!(expr.unwrap_err().contains("Expect ')'"));
    }

    #[test]
    fn parse_precedence() {
        let tokens = Lexer::lex_all("1 + 2 * 3".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.to_string(), "(+ 1 (* 2 3))")
    }

    #[test]
    fn parse_parentheses() {
        let tokens = Lexer::lex_all("(1 + 2) * 3".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.to_string(), "(* (group (+ 1 2)) 3)")
    }

    #[test]
    fn parse_power_precedence() {
        let tokens = Lexer::lex_all("2 ^ 3 * 3".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.to_string(), "(* (^ 2 3) 3)")
    }

    #[test]
    fn eval_power() {
        let tokens = Lexer::lex_all("2 ^ 3 * 3".to_string()).unwrap();

        let mut p = Parser::new(tokens); 
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.eval(), 24.0)
    }

    #[test]
    fn eval_float() {
        let tokens = Lexer::lex_all("4.5 * 3".to_string()).unwrap();

        let mut p = Parser::new(tokens);
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.eval(), 13.5);
    }

    #[test]
    fn eval_not_equal() {
        let tokens = Lexer::lex_all("4.5 != 3".to_string()).unwrap();

        let mut p = Parser::new(tokens);
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.eval(), 1.0);
    }

    #[test]
    fn eval_greater_equal() {
        let tokens = Lexer::lex_all("54.5 >= 3.89".to_string()).unwrap();

        let mut p = Parser::new(tokens);
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.eval(), 1.0);
    }

    #[test]
    fn eval_less_nested() {
        let tokens = Lexer::lex_all("10 ^ 3 / 5 < 7 * 200".to_string()).unwrap();

        let mut p = Parser::new(tokens);
        let expr = p.parse_expr().unwrap();

        assert_eq!(expr.eval(), 1.0);
    }
}

