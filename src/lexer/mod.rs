use std;

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
    Times,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct SToken {
    pub tok: Token,
    pub sp: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub scol: i32,
    pub srow: i32,
    pub ecol: i32,
    pub erow: i32,
}

impl Span {
    pub fn concat(sp1: Span, sp2: Span) -> Span {
        Span {
            scol: sp1.scol,
            srow: sp1.srow,
            ecol: sp2.ecol,
            erow: sp2.erow,
        }
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
    pub src: &'a str,
    itr: std::str::Chars<'a>,
    current_char: Option<char>,
    start_row: i32,
    start_col: i32,
    current_row: i32,
    current_col: i32,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut reader = Reader {
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

    pub fn next_token(&mut self) -> Result<SToken, String> {
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
            sp: Span {
                scol: self.start_col,
                srow: self.start_row,
                ecol: self.current_col,
                erow: self.current_row,
            }
        })
    }

    fn read_word(&mut self) -> Result<Token, String> {
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

    fn read_number(&mut self) -> Result<Token, String> {
        let mut float = false;
        let mut number = String::new();

        while let Some(c) = self.current_char {
            if c.is_numeric() {
                number.push(c);
            } else if c == '.' {
                if float {
                    return Err(format!("Lexer error: unexpected '.', {:?}", Span {
                        scol: self.start_col,
                        srow: self.start_row,
                        ecol: self.current_col,
                        erow: self.current_row,
                    }));
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
                Err(format!("Lexer error: failed to parse float, {:?}", Span {
                    scol: self.start_col,
                    srow: self.start_row,
                    ecol: self.current_col,
                    erow: self.current_row,
                }))
            }
        } else {
            if let Some(i) = number.parse::<i64>().ok() {
                Ok(Token::IntegerLiteral(i))
            } else {
                Err(format!("Lexer error: failed to parse integer, {:?}", Span {
                    scol: self.start_col,
                    srow: self.start_row,
                    ecol: self.current_col,
                    erow: self.current_row,
                }))
            }
        }
    }

    fn read_char(&mut self) -> Result<Token, String> {
        let mut c = match self.next_char() {
            Some(c) => c,
            None => return Err(format!("Lexer error: failed to parse char, {:?}", Span {
                scol: self.start_col,
                srow: self.start_row,
                ecol: self.current_col,
                erow: self.current_row,
            })),
        };

        if c == '\\' { // TODO: make escaping more accurate and complete
            c = self.next_char().unwrap();
        };

        if let Some(next_c) = self.next_char() {
            if next_c == '\'' {
                self.next_char();
                Ok(Token::CharLiteral(c))
            } else {
                Err(format!("Lexer error: failed to parse char, {:?}", Span {
                    scol: self.start_col,
                    srow: self.start_row,
                    ecol: self.current_col,
                    erow: self.current_row,
                }))
            }
        } else {
            Err(format!("Lexer error: failed to parse char, {:?}", Span {
                scol: self.start_col,
                srow: self.start_row,
                ecol: self.current_col,
                erow: self.current_row,
            }))
        }
    }

    fn read_string(&mut self) -> Result<Token, String> {
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
            Err(format!("Lexer error: failed to parse string, {:?}", Span {
                scol: self.start_col,
                srow: self.start_row,
                ecol: self.current_col,
                erow: self.current_row,
            }))
        }
    }

    fn read_symbol(&mut self) -> Result<Token, String> {
        let tok = match self.current_char.unwrap() {
            '(' => Ok(Token::Symbol(Symbol::LeftParenthesis)),
            ')' => Ok(Token::Symbol(Symbol::RightParenthesis)),
            '[' => Ok(Token::Symbol(Symbol::LeftBracket)),
            ']' => Ok(Token::Symbol(Symbol::RightBracket)),
            '{' => Ok(Token::Symbol(Symbol::LeftBrace)),
            '}' => Ok(Token::Symbol(Symbol::RightBrace)),
            '\n' => {
                self.current_col = 0;
                self.current_row += 1;
                Ok(Token::Symbol(Symbol::NewLine))
            },
            '.' => Ok(Token::Symbol(Symbol::Dot)),
            ',' => Ok(Token::Symbol(Symbol::Comma)),
            ':' => {
                match self.peek_char().unwrap() {
                    ':' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::ColonColon))
                    },
                    _ => Ok(Token::Symbol(Symbol::Colon)),
                }
            },
            '=' => {
                match self.peek_char().unwrap() {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::EqualEqual))
                    },
                    _ => Ok(Token::Symbol(Symbol::Equal)),
                }
            },
            '+' => {
                match self.peek_char().unwrap() {
                    '+' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::PlusPlus))
                    },
                    _ => Ok(Token::Symbol(Symbol::Plus)),
                }
            },
            '-' => {
                match self.peek_char().unwrap() {
                    '>' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::Return))
                    },
                    _ => Ok(Token::Symbol(Symbol::Minus)),
                }
            },
            '*' => Ok(Token::Symbol(Symbol::Times)),
            '/' => Ok(Token::Symbol(Symbol::Over)),
            '%' => Ok(Token::Symbol(Symbol::Modulo)),
            '!' => {
                match self.peek_char().unwrap() {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::NotEqual))
                    },
                    _ => Err(format!("Lexer error: failed to parse symbol, {:?}", Span {
                        scol: self.start_col,
                        srow: self.start_row,
                        ecol: self.current_col,
                        erow: self.current_row,
                    }))
                }
            },
            '#' => Ok(Token::Symbol(Symbol::Hash)),
            '<' => {
                match self.peek_char().unwrap() {
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
                match self.peek_char().unwrap() {
                    '=' => {
                        self.next_char();
                        Ok(Token::Symbol(Symbol::MoreOrEqual))
                    },
                    _ => Ok(Token::Symbol(Symbol::More))
                }
            },
            _ => Err(format!("Lexer error: failed to parse symbol, {:?}", Span {
                scol: self.start_col,
                srow: self.start_row,
                ecol: self.current_col,
                erow: self.current_row,
            })),
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
