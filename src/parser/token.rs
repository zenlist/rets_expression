use winnow::ascii::{alpha1, digit1, escaped_transform, take_escaped};
use winnow::combinator::{alt, opt, repeat};
use winnow::error::{InputError, ParserError};
use winnow::stream::AsChar;
use winnow::token::{any, none_of, one_of, take_until, take_while};
use winnow::{prelude::*, Located};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Whitespace(&'a str),
    Comment(Comment<'a>),
    Atom(Atom),
    BinaryOperator(BinaryOperator),
    BareKeyword(BareKeyword),
    DottedKeyword(DottedKeyword),
    /// 'string with single quotes'
    SingleQuotedTerm(SingleQuotedTerm<'a>),
    /// "string with double quotes"
    DoubleQuotedTerm(DoubleQuotedTerm<'a>),
    Identifier(Identifier<'a>),
    IsoTimestamp(IsoTimestamp<'a>),
    IsoDate(IsoDate<'a>),
    Int(Int<'a>),
    Float(Float<'a>),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Location {
    start: usize,
    end: usize,
}
impl From<std::ops::Range<usize>> for Location {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
impl From<Location> for std::ops::Range<usize> {
    fn from(value: Location) -> Self {
        value.start..value.end
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Comment<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Identifier<'a>(&'a str, Location);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SingleQuotedTerm<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct DoubleQuotedTerm<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IsoTimestamp<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IsoDate<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Int<'a>(&'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Float<'a>(&'a str);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperator {
    /// =
    Equal,
    /// !=
    NotEqual,
    /// <
    LessThan,
    /// <=
    LessThanEqual,
    /// >
    GreaterThan,
    /// >=
    GreaterThanEqual,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// ||
    DoublePipe,
}

impl<'s, 't, E: ParserError<&'s [Token<'t>]>> Parser<&'s [Token<'t>], BinaryOperator, E>
    for BinaryOperator
{
    fn parse_next(&mut self, input: &mut &'s [Token<'t>]) -> PResult<BinaryOperator, E> {
        any.verify_map(|token: Token<'_>| match token {
            Token::BinaryOperator(b) if b == *self => Some(b),
            _ => None,
        })
        .parse_next(input)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Atom {
    /// (
    LeftParen,
    /// )
    RightParen,
    /// ,
    Comma,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DottedKeyword {
    Or,
    And,
    Not,
    Contains,
    In,
    Mod,
    True,
    False,
    Empty,
    Null,
    Today,
    Now,
}

impl<'s, 't, E: ParserError<&'s [Token<'t>]>> Parser<&'s [Token<'t>], DottedKeyword, E>
    for DottedKeyword
{
    fn parse_next(&mut self, input: &mut &'s [Token<'t>]) -> PResult<DottedKeyword, E> {
        any.verify_map(|token: Token<'_>| match token {
            Token::DottedKeyword(b) if b == *self => Some(b),
            _ => None,
        })
        .parse_next(input)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BareKeyword {
    Iif,
    Last,
}

impl<'s, 't, E: ParserError<&'s [Token<'t>]>> Parser<&'s [Token<'t>], BareKeyword, E>
    for BareKeyword
{
    fn parse_next(&mut self, input: &mut &'s [Token<'t>]) -> PResult<BareKeyword, E> {
        any.verify_map(|token: Token<'_>| match token {
            Token::BareKeyword(b) if b == *self => Some(b),
            _ => None,
        })
        .parse_next(input)
    }
}

impl<'s, 't, E: ParserError<&'s [Token<'t>]>> Parser<&'s [Token<'t>], Atom, E> for Atom {
    fn parse_next(&mut self, input: &mut &'s [Token<'t>]) -> PResult<Atom, E> {
        any.verify_map(|token: Token<'_>| match token {
            Token::Atom(b) if b == *self => Some(b),
            _ => None,
        })
        .parse_next(input)
    }
}

impl<'a> winnow::stream::ContainsToken<Token<'a>> for Token<'a> {
    #[inline(always)]
    fn contains_token(&self, token: Token<'a>) -> bool {
        *self == token
    }
}

impl<'a, 'b> winnow::stream::ContainsToken<Token<'a>> for &'_ [Token<'b>] {
    #[inline]
    fn contains_token(&self, token: Token<'a>) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<'a, 'b, const LEN: usize> winnow::stream::ContainsToken<Token<'a>> for &'_ [Token<'b>; LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'a>) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<'a, 'b, const LEN: usize> winnow::stream::ContainsToken<Token<'a>> for [Token<'b>; LEN] {
    #[inline]
    fn contains_token(&self, token: Token<'a>) -> bool {
        self.iter().any(|t| *t == token)
    }
}

pub fn lex<'a>(i: &mut Located<&'a str>) -> PResult<Vec<Token<'a>>> {
    repeat(1.., token).parse_next(i)
}

pub fn token<'a>(i: &mut Located<&'a str>) -> PResult<Token<'a>> {
    alt((
        whitespace,
        Comment::parse.map(Token::Comment),
        BareKeyword::parse.map(Token::BareKeyword),
        Identifier::parse.map(Token::Identifier),
        DottedKeyword::parse.map(Token::DottedKeyword),
        BinaryOperator::parse.map(Token::BinaryOperator),
        Float::parse.map(Token::Float),
        Int::parse.map(Token::Int),
        Atom::parse.map(Token::Atom),
        SingleQuotedTerm::parse.map(Token::SingleQuotedTerm),
        DoubleQuotedTerm::parse.map(Token::DoubleQuotedTerm),
        // double_quoted_term,
        // iso_timestamp,
        // iso_date,
    ))
    .parse_next(i)
}

fn whitespace<'a>(i: &mut Located<&'a str>) -> PResult<Token<'a>> {
    take_while(1.., |char: char| char.is_whitespace())
        .map(Token::Whitespace)
        .parse_next(i)
}

impl<'a> Identifier<'a> {
    pub fn name(self) -> &'a str {
        self.0
    }
    fn parse(i: &mut Located<&'a str>) -> PResult<Self> {
        (
            one_of(|c: char| c.is_alpha() || c == '_'),
            take_while(0.., |c: char| c.is_alphanum() || c == '_'),
        )
            .take()
            .with_span()
            .map(|(text, range)| Self(text, range.into()))
            .parse_next(i)
    }
}

impl<'a> Comment<'a> {
    fn parse(i: &mut Located<&'a str>) -> PResult<Comment<'a>> {
        alt((single_line_comment, multi_line_comment)).parse_next(i)
    }
}

fn single_line_comment<'a>(i: &mut Located<&'a str>) -> PResult<Comment<'a>> {
    ("//", take_until(0.., "\n"), "\n")
        .take()
        .map(Comment)
        .parse_next(i)
}

fn multi_line_comment<'a>(i: &mut Located<&'a str>) -> PResult<Comment<'a>> {
    ("/*", take_until(0.., "*/"), "*/")
        .take()
        .map(Comment)
        .parse_next(i)
}

impl DottedKeyword {
    fn parse(i: &mut Located<&'_ str>) -> PResult<Self> {
        // TODO: How do we use `dispatch!` with a Located<&'_, str>?
        (".", alpha1, ".")
            .verify_map(|(_, s, _)| match s {
                "OR" => Some(DottedKeyword::Or),
                "AND" => Some(DottedKeyword::And),
                "NOT" => Some(DottedKeyword::Not),
                "CONTAINS" => Some(DottedKeyword::Contains),
                "IN" => Some(DottedKeyword::In),
                "MOD" => Some(DottedKeyword::Mod),
                "TRUE" => Some(DottedKeyword::True),
                "FALSE" => Some(DottedKeyword::False),
                "EMPTY" => Some(DottedKeyword::Empty),
                "NULL" => Some(DottedKeyword::Null),
                "TODAY" => Some(DottedKeyword::Today),
                "NOW" => Some(DottedKeyword::Now),
                _ => None,
            })
            .parse_next(i)
    }
}

impl Atom {
    fn parse(i: &mut Located<&'_ str>) -> PResult<Self> {
        alt((
            "(".value(Atom::LeftParen),
            ")".value(Atom::RightParen),
            ",".value(Atom::Comma),
            "[".value(Atom::LeftBracket),
            "]".value(Atom::RightBracket),
        ))
        .parse_next(i)
    }
}

impl BinaryOperator {
    fn parse(i: &mut Located<&'_ str>) -> PResult<Self> {
        alt((
            "=".value(BinaryOperator::Equal),
            "!=".value(BinaryOperator::NotEqual),
            "<=".value(BinaryOperator::LessThanEqual),
            "<".value(BinaryOperator::LessThan),
            ">=".value(BinaryOperator::GreaterThanEqual),
            ">".value(BinaryOperator::GreaterThan),
            "+".value(BinaryOperator::Plus),
            "-".value(BinaryOperator::Minus),
            "*".value(BinaryOperator::Asterisk),
            "/".value(BinaryOperator::Slash),
            "||".value(BinaryOperator::DoublePipe),
        ))
        .parse_next(i)
    }
}

impl BareKeyword {
    fn parse(i: &mut Located<&'_ str>) -> PResult<Self> {
        // TODO: How do we use `dispatch!` with a Located<&'_, str>?
        alpha1
            .verify_map(|s| match s {
                "IIF" => Some(BareKeyword::Iif),
                "LAST" => Some(BareKeyword::Last),
                _ => None,
            })
            .parse_next(i)
    }
}

impl<'a> SingleQuotedTerm<'a> {
    fn parse(i: &mut Located<&'a str>) -> PResult<Self> {
        (
            '\'',
            take_escaped(none_of(['\'', '\\']), '\\', one_of(['\\', '\''])),
            '\'',
        )
            .map(|(_, str, _)| Self(str))
            .parse_next(i)
    }
    pub fn value(&self) -> String {
        let mut input = self.0;

        pub fn get_value<'s>(i: &mut &'s str) -> PResult<String, InputError<&'s str>> {
            escaped_transform(
                take_while(1.., |c: char| c != '\'' && c != '\\'),
                '\\',
                alt(("\\".value("\\"), "\'".value("'"))),
            )
            .parse_next(i)
        }

        get_value(&mut input).unwrap()
    }
}

impl<'a> DoubleQuotedTerm<'a> {
    fn parse(i: &mut Located<&'a str>) -> PResult<Self> {
        (
            '"',
            take_escaped(none_of(['"', '\\']), '\\', one_of(['\\', 'n', 't', '"'])),
            '"',
        )
            .map(|(_, str, _)| Self(str))
            .parse_next(i)
    }
    pub fn value(&self) -> String {
        let mut input = self.0;

        pub fn get_value<'s>(i: &mut &'s str) -> PResult<String, InputError<&'s str>> {
            escaped_transform(
                take_while(1.., |c: char| c != '"' && c != '\\'),
                '\\',
                alt((
                    "\\".value("\\"),
                    "\"".value("\""),
                    "n".value("\n"),
                    "t".value("\t"),
                )),
            )
            .parse_next(i)
        }

        get_value(&mut input).unwrap()
    }
}

impl<'a> Int<'a> {
    fn parse(i: &mut Located<&'a str>) -> PResult<Self> {
        (opt(one_of(('+', '-'))), digit1)
            .take()
            .verify(|x: &str| x.parse::<i64>().ok().is_some())
            .map(Int)
            .parse_next(i)
    }
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}

impl<'a> Float<'a> {
    fn parse(i: &mut Located<&'a str>) -> PResult<Self> {
        // Case one: .42
        fn dot_number<'a>(i: &mut Located<&'a str>) -> PResult<Float<'a>> {
            ('.', take_while(1.., |c: char| c.is_numeric()))
                .take()
                .map(Float)
                .parse_next(i)
        }

        // Case two: 42e42 and 42.42e42
        fn number_e<'a>(i: &mut Located<&'a str>) -> PResult<Float<'a>> {
            (
                take_while(1.., |c: char| c.is_numeric()),
                '.',
                take_while(0.., |c: char| c.is_numeric()),
                'e',
                take_while(1.., |c: char| c.is_numeric()),
            )
                .take()
                .map(Float)
                .parse_next(i)
        }

        // Case three: 42. and 42.42
        fn number_dot_number<'a>(i: &mut Located<&'a str>) -> PResult<Float<'a>> {
            (
                take_while(1.., |c: char| c.is_numeric()),
                '.',
                take_while(0.., |c: char| c.is_numeric()),
            )
                .take()
                .map(Float)
                .parse_next(i)
        }

        alt((dot_number, number_e, number_dot_number)).parse_next(i)
    }
    pub fn as_str(&self) -> &'a str {
        self.0
    }
}
