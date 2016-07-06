use std;
use std::fmt;
use std::fmt::Formatter;
use std::fmt::Display;
use std::error::Error as BaseError;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    InvalidChar,
    InvalidString,
    InvalidInteger,
    InvalidFloat,
    InvalidSymbol,
    UnexpectedChar(char),
    UnknownEscapeChar(char),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.span,
            match self.kind {
                ErrorKind::InvalidChar |
                ErrorKind::InvalidString |
                ErrorKind::InvalidInteger |
                ErrorKind::InvalidFloat |
                ErrorKind::InvalidSymbol => self.description().to_string(),
                ErrorKind::UnexpectedChar(c) => format!("unexpected '{}'", c),
                ErrorKind::UnknownEscapeChar(c) => format!("unknown escape character {} in string literal", c),
            }
        )
    }
}



impl BaseError for Error {
    fn description(&self) -> &str {
        match self.kind {
            ErrorKind::InvalidChar => "failed to parse char",
            ErrorKind::InvalidString => "failed to parse string",
            ErrorKind::InvalidInteger => "failed to parse integer",
            ErrorKind::InvalidFloat => "failed to parse float",
            ErrorKind::InvalidSymbol => "failed to parse symbol",
            ErrorKind::UnexpectedChar(_) => "unexpected char",
            ErrorKind::UnknownEscapeChar(_) => "unknown escape character in string literal",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        None
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Import,
    Package,
    Func,
    Var,
    If,
    Else,
    While,
    Struct,
    Return,
    For,
    In,
    New,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    NewLine,
    Dot,
    Comma,
    Colon,
    Equal,
    EqualEqual,
    Plus,
    PlusPlus,
    Minus,
    Star,
    Over,
    Modulo,
    NotEqual,
    ColonColon,
    Hash,
    Less,
    LessOrEqual,
    More,
    MoreOrEqual,
    Concat,
    Return,
    Amp,
    At,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SToken {
    pub tok: Token,
    pub sp: Span,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Span {
    pub srow: i32,
    pub scol: i32,
    pub erow: i32,
    pub ecol: i32,
    pub file: String,
}

impl Span {
    pub fn concat(sp1: Span, sp2: Span) -> Span {
        Span {
            scol: sp1.scol,
            srow: sp1.srow,
            ecol: sp2.ecol,
            erow: sp2.erow,
            file: sp1.file,
        }
    }

    pub fn nil_span() -> Span {
        Span {
            scol: 0,
            srow: 0,
            ecol: 0,
            erow: 0,
            file: "".to_string(),
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.srow)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    Identifier(String),
    Keyword(Keyword),
    StringLiteral(String),
    CharLiteral(char),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Symbol(Symbol),
}

pub struct Reader<'a> {
    pub filename: String,
    pub src: &'a str,
    itr: std::str::Chars<'a>,
    current_char: Option<char>,
    start_row: i32,
    start_col: i32,
    current_row: i32,
    current_col: i32,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str, filename: String) -> Self {
        let mut reader = Reader {
            filename: filename,
            src: input,
            itr: input.chars(),
            current_char: None,
            current_col: 0,
            current_row: 1,
            start_col: 0,
            start_row: 1,
        };
        reader.next_char();

        reader
    }

    fn get_current_span(&self) -> Span {
        Span {
            file: self.filename.clone(),
            scol: self.start_col,
            srow: self.start_row,
            ecol: self.current_col,
            erow: self.current_row,
        }
    }

    pub fn next_token(&mut self) -> Result<SToken> {
        self.skip_whitespace();

        self.start_col = self.current_col;
        self.start_row = self.current_row;

        let new_token = match self.current_char {
            Some(c) => {
                if c.is_alphabetic() || c == '_' || (c as u32) > 127 {
                    self.read_word()
                } else if c.is_numeric() || (c == '.' && self.peek_char().unwrap_or('\0').is_numeric()) {
                    self.read_number()
                } else if c == '\'' {
                    self.read_char()
                } else if c == '"' {
                    self.read_string()
                } else {
                    self.read_symbol()
                }
            },
            None => Ok(Token::EOF),
        };

        Ok(SToken {
            tok: try!(new_token),
            sp: self.get_current_span(),
        })
    }

    fn read_word(&mut self) -> Result<Token> {
        let mut word = String::new();

        while let Some(c) = self.current_char {
            if c.is_alphabetic() || c.is_numeric() || c == '_' || (c as u32) > 127 {
                word.push(c);
            } else {
                break;
            }

            self.next_char();
        };

        match word.as_ref() {
            "import" => Ok(Token::Keyword(Keyword::Import)),
            "package" => Ok(Token::Keyword(Keyword::Package)),
            "func" => Ok(Token::Keyword(Keyword::Func)),
            "var" => Ok(Token::Keyword(Keyword::Var)),
            "true" => Ok(Token::BoolLiteral(true)),
            "false" => Ok(Token::BoolLiteral(false)),
            "if" => Ok(Token::Keyword(Keyword::If)),
            "else" => Ok(Token::Keyword(Keyword::Else)),
            "while" => Ok(Token::Keyword(Keyword::While)),
            "struct" => Ok(Token::Keyword(Keyword::Struct)),
            "return" => Ok(Token::Keyword(Keyword::Return)),
            "for" => Ok(Token::Keyword(Keyword::For)),
            "in" => Ok(Token::Keyword(Keyword::In)),
            "new" => Ok(Token::Keyword(Keyword::New)),
            _ => Ok(Token::Identifier(word)),
        }
    }

    fn read_number(&mut self) -> Result<Token> {
        let mut float = false;
        let mut number = String::new();

        while let Some(c) = self.current_char {
            if c.is_numeric() {
                number.push(c);
            } else if c == '.' {
                if float {
                    return Err(Error { kind: ErrorKind::UnexpectedChar('.'), span: self.get_current_span() });
                } else {
                    float = true;
                    number.push(c);
                }
            } else {
                break;
            }

            self.next_char();
        };

        if float {
            if let Some(f) = number.parse::<f64>().ok() {
                Ok(Token::FloatLiteral(f))
            } else {
                Err(Error { kind: ErrorKind::InvalidFloat, span: self.get_current_span() })
            }
        } else if let Some(i) = number.parse::<i64>().ok() {
            Ok(Token::IntegerLiteral(i))
        } else {
            Err(Error { kind: ErrorKind::InvalidInteger, span: self.get_current_span() })
        }
    }

    fn read_char(&mut self) -> Result<Token> {
        let mut c = match self.next_char() {
            Some(c) => c,
            None => return Err(Error { kind: ErrorKind::InvalidChar, span: self.get_current_span() }),
        };

        if c == '\\' { // TODO: make escaping more accurate and complete
            c = self.next_char().unwrap();
        };

        if let Some(next_c) = self.next_char() {
            if next_c == '\'' {
                self.next_char();
                Ok(Token::CharLiteral(c))
            } else {
                Err(Error { kind: ErrorKind::InvalidChar, span: self.get_current_span() })
            }
        } else {
            Err(Error { kind: ErrorKind::InvalidChar, span: self.get_current_span() })
        }
    }

    fn read_string(&mut self) -> Result<Token> {
        let mut string = String::new();

        let mut escaped = false;
        let mut closed = false;
        while let Some(c) = self.next_char() {
            if c == '\\' && !escaped {
                escaped = true;
            } else if c == '"' && !escaped {
                closed = true;
                self.next_char();
                break;
            } else if escaped {
                escaped = false;
                if c == 'n' {
                    string.push('\n');
                } else {
                    return Err(Error { kind: ErrorKind::UnknownEscapeChar(c), span: self.get_current_span() });
                }
            } else {
                if c == '\n' {
                    self.current_col = 0;
                    self.current_row += 1;
                }
                escaped = false;
                string.push(c);
            }
        };

        if closed {
            Ok(Token::StringLiteral(string))
        } else {
            Err(Error { kind: ErrorKind::InvalidString, span: self.get_current_span() })
        }
    }

    fn read_symbol(&mut self) -> Result<Token> {
        let tok = match self.current_char.unwrap() {
            '(' => Ok(Token::Symbol(Symbol::LeftParenthesis)),
            ')' => Ok(Token::Symbol(Symbol::RightParenthesis)),
            '[' => Ok(Token::Symbol(Symbol::LeftBracket)),
            ']' => Ok(Token::Symbol(Symbol::RightBracket)),
            '{' => Ok(Token::Symbol(Symbol::LeftBrace)),
            '}' => Ok(Token::Symbol(Symbol::RightBrace)),
            '&' => Ok(Token::Symbol(Symbol::Amp)),
            '@' => Ok(Token::Symbol(Symbol::At)),
            '\n' => {
                self.current_col = 0;
                self.current_row += 1;
                Ok(Token::Symbol(Symbol::NewLine))
            },
            '.' => Ok(Token::Symbol(Symbol::Dot)),
            ',' => Ok(Token::Symbol(Symbol::Comma)),
            ':' => {
                match self.peek_char().unwrap_or('\0') {
                    ':' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::ColonColon))
                    },
                    _ => Ok(Token::Symbol(Symbol::Colon)),
                }
            },
            '=' => {
                match self.peek_char().unwrap_or('\0') {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::EqualEqual))
                    },
                    _ => Ok(Token::Symbol(Symbol::Equal)),
                }
            },
            '+' => {
                match self.peek_char().unwrap_or('\0') {
                    '+' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::PlusPlus))
                    },
                    _ => Ok(Token::Symbol(Symbol::Plus)),
                }
            },
            '-' => {
                match self.peek_char().unwrap_or('\0') {
                    '>' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::Return))
                    },
                    _ => Ok(Token::Symbol(Symbol::Minus)),
                }
            },
            '*' => Ok(Token::Symbol(Symbol::Star)),
            '/' => Ok(Token::Symbol(Symbol::Over)),
            '%' => Ok(Token::Symbol(Symbol::Modulo)),
            '!' => {
                match self.peek_char().unwrap_or('\0') {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::NotEqual))
                    },
                    _ => Err(Error { kind: ErrorKind::InvalidSymbol, span: self.get_current_span() })
                }
            },
            '#' => Ok(Token::Symbol(Symbol::Hash)),
            '<' => {
                match self.peek_char().unwrap_or('\0') {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::LessOrEqual))
                    },
                    '>' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::Concat))
                    }
                    _ => Ok(Token::Symbol(Symbol::Less)),
                }
            },
            '>' => {
                match self.peek_char().unwrap_or('\0') {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::MoreOrEqual))
                    },
                    _ => Ok(Token::Symbol(Symbol::More))
                }
            },
            _ => Err(Error { kind: ErrorKind::InvalidSymbol, span: self.get_current_span() }),
        };

        self.next_char();

        tok
    }

    fn next_char(&mut self) -> Option<char> {
        self.current_char = self.itr.next();
        self.current_col += 1;
        self.current_char
    }

    fn peek_char(&self) -> Option<char> {
        match self.itr.clone().peekable().peek() {
            Some(c) => Some(*c),
            None => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c != ' ' && c != '\t' && c != '\r' {
                break;
            }
            self.next_char();
        }
    }
}
