use std::mem;
use std::rc::Rc;
use std::vec;

use crate::phase::Phase;
use crate::source_file::*;
use crate::reporting::*;

#[derive(Clone, Debug)]
pub struct Token {
    pub value: String,
    pub span: Span,
    pub ty: TokenType,
}

impl Spannable for Token {

    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    AlphaNum,
    LineBreak,
    Str,
    Char,
    Punct,
}

pub struct Lexer {
    src: Rc<SourceFile>,
    chars: vec::IntoIter<char>,
    current_token: Option<String>,
    start_of_current: Option<Position>,
    current_position: Position,
    state: LexerState,
    token_queue: Vec<Token>,
}

#[derive(Clone, PartialEq, Eq)]
enum LexerState {
    None,
    AlphaNum,
    LineBreak,
    StrLit,
    StrEsc,
    StrEnd,
    CharLit,
    CharEsc,
    CharEnd,
    Punct,
    Backslash,
    CommentStart,
    Comment,
    MultiComment(Option<Box<LexerState>>),
    MultiNest(Option<Box<LexerState>>),
    MultiEnd(Option<Box<LexerState>>),
}

impl Lexer {

    pub fn new(src: Rc<SourceFile>) -> Lexer {
        let chars = src.lines.join("\n").chars().collect::<Vec<_>>().into_iter();
        Lexer {
            src,
            chars,
            current_token: None,
            start_of_current: None,
            current_position: Position::new(0,0),
            state: LexerState::None,
            token_queue: Vec::new(),
        }
    }

    fn new_token(&mut self, c: char, state: LexerState) {
        self.state = state;
        self.start_of_current = Some(self.current_position);
        let mut new_str = String::new();
        new_str.push(c);
        self.current_token = Some(new_str);
    }

    fn push_char(&mut self, c: char) {
        self.current_token.as_mut().unwrap().push(c);
    }

    fn end_token(&mut self, ty: TokenType) {
        let mut swapped = None;
        mem::swap(&mut swapped, &mut self.current_token);
        self.token_queue.insert(0, Token {
            value: swapped.unwrap(),
            span: Span::new(self.src.clone(), self.start_of_current.unwrap(), self.current_position),
            ty,
        });
        self.state = LexerState::None;
    }

    fn start_comment(&mut self) {
        self.state = LexerState::CommentStart;
    }

    // this returns true if the token stack
    // has contents for popping
    fn consume_char(&mut self) -> bool {
        use self::TokenType as TT;
        use self::LexerState as LS;
        let next = if let Some(next) = self.chars.next() {
            next
        } else {
            // we're done, so shove whatever's left into
            // the buffer; we'll validate all the tokens
            // later, anyways.
            match self.state {
                LS::None | LS::Comment | LS::CommentStart | LS::MultiComment(_) | LS::MultiNest(_) | LS::MultiEnd(_) => {},
                LS::AlphaNum => {
                    self.end_token(TT::AlphaNum);
                },
                LS::LineBreak => {
                    self.end_token(TT::LineBreak);
                }
                LS::StrLit | LS::StrEsc | LS::StrEnd => {
                    self.end_token(TT::Str)
                },
                LS::CharLit | LS::CharEsc | LS::CharEnd => {
                    self.end_token(TT::Char)
                },
                LS::Punct | LS::Backslash => {
                    self.end_token(TT::Punct);
                },
            }
            self.state = LS::None;
            return true;
        };

        match self.state.clone() { // FIXME: get rid of clone
            LS::None => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {},
                    '\n' => {
                        self.new_token(next, LS::LineBreak);
                    },
                    ';' => {
                        self.start_comment();
                    },
                    '\\' => {
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::AlphaNum => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.push_char(next);
                    },
                    '"' => {
                        self.end_token(TT::AlphaNum);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::AlphaNum);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {
                        self.end_token(TT::AlphaNum);
                    },
                    '\n' => {
                        self.end_token(TT::AlphaNum);
                        self.new_token(next, LS::LineBreak);
                    },
                    ';' => {
                        self.end_token(TT::AlphaNum);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::AlphaNum);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::AlphaNum);
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::LineBreak => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.end_token(TT::LineBreak);
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.end_token(TT::LineBreak);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::LineBreak);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' | '\n' => {
                        self.push_char(next);
                    },
                    ';' => {
                        self.end_token(TT::LineBreak);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::LineBreak);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::LineBreak);
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::StrLit => {
                match next {
                    '"' => {
                        self.push_char(next);
                        self.state = LS::StrEnd
                    },
                    '\\' => {
                        self.push_char(next);
                        self.state = LS::StrEsc
                    },
                    _ => {
                        self.push_char(next);
                    },
                }
            },
            LS::StrEsc => {
                match next {
                    '"' => {
                        self.push_char(next);
                        self.state = LS::StrLit
                    },
                    _ => {
                        self.push_char(next);
                    },
                }
            },
            LS::StrEnd => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {
                        self.end_token(TT::Str);
                    },
                    '\n' => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::LineBreak);
                    },
                    ';' => {
                        self.end_token(TT::Str);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::Str);
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::CharLit => {
                match next {
                    '"' => {
                        self.push_char(next);
                        self.state = LS::StrEnd
                    },
                    '\\' => {
                        self.push_char(next);
                        self.state = LS::StrEsc
                    },
                    _ => {
                        self.push_char(next);
                    },
                }
            },
            LS::CharEsc => {
                match next {
                    '"' => {
                        self.push_char(next);
                        self.state = LS::StrLit
                    },
                    _ => {
                        self.push_char(next);
                    },
                }
            },
            LS::CharEnd => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {
                        self.end_token(TT::Char);
                    },
                    '\n' => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::LineBreak);
                    },
                    ';' => {
                        self.end_token(TT::Char);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::Char);
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::CommentStart => {
                match next {
                    '[' => {
                        self.state = LS::MultiComment(None);
                    },
                    '\n' => {
                        self.new_token(next, LS::LineBreak);
                    },
                    _ => {
                        self.state = LS::Comment;
                    },
                }
            },
            LS::Comment => {
                match next {
                    '\n' => {
                        self.new_token(next, LS::LineBreak);
                    },
                    _ => {},
                }
            },
            LS::MultiComment(inner) => {
                match next {
                    ']' => {
                        self.state = LS::MultiEnd(inner);
                    },
                    ';' => {
                        self.state = LS::MultiNest(inner);
                    },
                    _ => {},
                }
            },
            LS::MultiNest(inner) => {
                match next {
                    '[' => {
                        self.state = LS::MultiComment(Some(Box::new(LS::MultiComment(inner))));
                    },
                    _ => {
                        self.state = LS::MultiComment(inner);
                    },
                }
            },
            LS::MultiEnd(inner) => {
                match next {
                    ';' => {
                        self.state = if let Some(ptr) = inner {
                            *ptr
                        } else {
                            LS::None
                        }
                    }
                    _ => {
                        self.state = LS::MultiComment(inner);
                    },
                }
            },
            LS::Punct => {
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {
                        self.end_token(TT::Punct);
                    },
                    '\n' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::LineBreak);
                    },
                    ';' => {
                        self.end_token(TT::Punct);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::Punct);
                    },
                }
            },
            LS::Backslash => {
                // since we want any token we break off
                // to just have the first character (since we might
                // have consumed spaces), we have this small hack
                let mut real_pos = self.current_position;
                self.current_position = self.start_of_current.unwrap().advance_col();
                match next {
                    'a' ... 'z' | '$' | '_' | '<' | '>' |
                    'A' ... 'Z' | '0' ... '9' | '.' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::AlphaNum);
                    },
                    '"' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::StrLit);
                    },
                    '\'' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::CharLit);
                    },
                    ' ' | '\t' => {
                        // we're gonna ignore spaces
                    },
                    '\n' => {
                        // and if we hit a newline, ignore it, too!
                        // we also don't push this backslash as a token
                        self.current_token = None;
                        self.state = LS::None;
                    },
                    ';' => {
                        self.end_token(TT::Punct);
                        self.start_comment();
                    },
                    '\\' => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::Backslash);
                    },
                    _ => {
                        self.end_token(TT::Punct);
                        self.new_token(next, LS::Punct);
                    },
                }
                self.current_position = real_pos;
            },
        }

        if next == '\n' {
            self.current_position.advance_line_mut();
        } else {
            self.current_position.advance_col_mut();
        }

        !self.token_queue.is_empty()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.consume_char() {}
        self.token_queue.pop()
    }
}

impl Phase for Lexer {

    type Input = Rc<SourceFile>;
    type Output = Vec<Token>;

    fn run(input: Vec<Self::Input>) -> Reported<Vec<Self::Output>> {
        Reported::new().complete(input.into_iter().map(|str| {
            Lexer::new(str).collect()
        }).collect())
    }
}