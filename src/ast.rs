use std::fmt;

use crate::token::*;

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    VarDecl {
        name: Token,
        initializer: Option<Expr>,
    },
    Block(Vec<Expr>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Func {
        name: Token,
        params: Vec<Token>,
        body: Box<Stmt>
    },
    Return(Option<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary { op: TokenType, rhs: Box<Expr> },
    Binary { lhs: Box<Expr>, op: TokenType, rhs: Box<Expr> },
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn eval(&self) -> Result<Literal, String> {
        match self {
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Binary { lhs, op, rhs } => {
                let left = lhs.eval()?;
                let right = rhs.eval()?;
                self.eval_binary(op, left, right)
            },
            Expr::Unary {op, rhs} => {
                let val = rhs.eval()?;
                self.eval_unary(op, val)
            },
            Expr::Grouping(inner) => inner.eval(),
        } 
    }

    fn eval_unary(&self, op: &TokenType, val: Literal) -> Result<Literal, String> {
        match (op, val) {
            (TokenType::Minus, Literal::Number(n)) => Ok(Literal::Number(-n)),
            (TokenType::Bang, Literal::Bool(b)) => Ok(Literal::Bool(!b)),
            _ => Err("Unsupported unary opperation".into())
        }
    }

    fn eval_binary(&self, op: &TokenType, left: Literal, right: Literal) -> Result<Literal, String> {
        match (op, left, right) {
            (TokenType::Plus, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
            (TokenType::Plus, Literal::Str(l), Literal::Str(r)) => Ok(Literal::Str(l + &r)),
            
            (TokenType::Minus, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l - r)),
            (TokenType::Multiply, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l * r)),
            (TokenType::Divide, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l / r)),
            (TokenType::Power, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l.powf(r))),

            (TokenType::EqualEq, l, r) => Ok(Literal::Bool(l == r)),
            (TokenType::BangEq, l, r) => Ok(Literal::Bool(l != r)),

            (TokenType::Greater, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Bool(l > r)),
            (TokenType::GreaterEq, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Bool(l >= r)),
            (TokenType::Less, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Bool(l < r)),
            (TokenType::LessEq, Literal::Number(l), Literal::Number(r)) => Ok(Literal::Bool(l <= r)),

            _ => Err("Type error in binary expression".into()),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(n) => write!(f, "{}", n),
            Expr::Unary { op, rhs } => write!(f, "{}{}", op, rhs),
            Expr::Binary { lhs, op, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {})", expr)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    Bool(bool),
    Str(String),
    Nil
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::Str(s) => write!(f, "{}", s),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "Nil"),
        }
    }
}
