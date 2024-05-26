use serde_json::json;
use winnow::{
    combinator::{alt, delimited, fail, preceded, separated, separated_foldl1},
    prelude::*,
    token::{any, one_of},
    Located,
};

use super::token::{lex, Atom, BareKeyword, BinaryOperator, DottedKeyword, Identifier, Token};

use crate::{
    AndNode, Expression, ExpressionOp, FieldNode, FunctionNode, IifNode, LastFieldNode, ListNode,
    LiteralNode, NotNode, OpNode, OrNode,
};

pub(crate) fn parse(input: &str) -> Result<Expression, String> {
    let tokens = lex
        .parse(Located::new(input))
        .map_err(|err| err.to_string())?;
    let mut tokens = tokens
        .into_iter()
        .filter(|token| !matches!(token, Token::Whitespace(_) | Token::Comment(_)))
        .collect::<Vec<_>>();

    exp.parse(&mut tokens)
        .map_err(|err| format!("Error at {}", err.offset()))
}

fn exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    or_exp(input)
}

fn or_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    let mut list: Vec<_> = separated(1.., and_exp, DottedKeyword::Or).parse_next(input)?;

    let expr = if list.len() == 1 {
        list.pop().unwrap()
    } else {
        Expression::from(OrNode::new(list))
    };
    Ok(expr)
}

fn and_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    let mut list: Vec<_> = separated(1.., not_exp, DottedKeyword::And).parse_next(input)?;

    let expr = if list.len() == 1 {
        list.pop().unwrap()
    } else {
        Expression::from(AndNode::new(list))
    };
    Ok(expr)
}

fn not_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    alt((
        preceded(DottedKeyword::Not, not_exp).map(|exp| Expression::from(NotNode::new(exp))),
        eq_exp,
    ))
    .parse_next(input)
}

fn eq_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn first_case(input: &mut &[Token<'_>]) -> PResult<Expression> {
        (
            cmp_exp,
            alt((
                BinaryOperator::NotEqual.value(ExpressionOp::Ne),
                BinaryOperator::Equal.value(ExpressionOp::Eq),
            )),
            cmp_exp,
        )
            .map(|(l, op, r)| Expression::from(OpNode::new(l, op, r)))
            .parse_next(input)
    }

    alt((first_case, cmp_exp)).parse_next(input)
}

fn cmp_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn first_case(input: &mut &[Token<'_>]) -> PResult<Expression> {
        (
            cnt_exp,
            alt((
                BinaryOperator::LessThanEqual.value(ExpressionOp::Lte),
                BinaryOperator::LessThan.value(ExpressionOp::Lt),
                BinaryOperator::GreaterThanEqual.value(ExpressionOp::Gte),
                BinaryOperator::GreaterThan.value(ExpressionOp::Gt),
            )),
            cnt_exp,
        )
            .map(|(l, op, r)| Expression::from(OpNode::new(l, op, r)))
            .parse_next(input)
    }

    alt((first_case, cnt_exp)).parse_next(input)
}

fn cnt_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn first_case(input: &mut &[Token<'_>]) -> PResult<Expression> {
        (
            sum_exp,
            alt((
                DottedKeyword::Contains.value(ExpressionOp::Contains),
                DottedKeyword::In.value(ExpressionOp::In),
            )),
            sum_exp,
        )
            .map(|(l, op, r)| Expression::from(OpNode::new(l, op, r)))
            .parse_next(input)
    }

    alt((first_case, sum_exp)).parse_next(input)
}

fn sum_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn op(input: &mut &[Token<'_>]) -> PResult<ExpressionOp> {
        alt((
            BinaryOperator::Plus.value(ExpressionOp::Add),
            BinaryOperator::Minus.value(ExpressionOp::Sub),
            BinaryOperator::DoublePipe.value(ExpressionOp::Concat),
        ))
        .parse_next(input)
    }

    separated_foldl1(
        prod_exp,
        op,
        |l: Expression, op: ExpressionOp, r: Expression| Expression::from(OpNode::new(l, op, r)),
    )
    .parse_next(input)
}

fn prod_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn op(input: &mut &[Token<'_>]) -> PResult<ExpressionOp> {
        alt((
            BinaryOperator::Asterisk.value(ExpressionOp::Mul),
            BinaryOperator::Slash.value(ExpressionOp::Div),
            DottedKeyword::Mod.value(ExpressionOp::Mod),
        ))
        .parse_next(input)
    }

    separated_foldl1(
        atom_exp,
        op,
        |l: Expression, op: ExpressionOp, r: Expression| Expression::from(OpNode::new(l, op, r)),
    )
    .parse_next(input)
}

fn atom_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    alt((
        func_exp,
        collection_exp,
        parenthesized_exp,
        list_exp,
        value_exp,
    ))
    .parse_next(input)
}

fn parenthesized_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    (Atom::LeftParen, exp, Atom::RightParen)
        .map(|(_, exp, _)| exp)
        .parse_next(input)
}

fn list_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    (
        Atom::LeftParen,
        separated(0.., exp, Atom::Comma),
        Atom::RightParen,
    )
        .map(|(_, args, _): (_, Vec<Expression>, _)| Expression::from(ListNode::new(args)))
        .parse_next(input)
}

// TODO: rename this to iif_expression?
fn func_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    (
        BareKeyword::Iif,
        Atom::LeftParen,
        exp,
        Atom::Comma,
        exp,
        Atom::Comma,
        exp,
        Atom::RightParen,
    )
        .map(
            |(
                _iif,
                _lparen,
                test_exp,
                _first_comma,
                true_exp,
                _second_comma,
                false_exp,
                _rparen,
            )| Expression::from(IifNode::new(test_exp, true_exp, false_exp)),
        )
        .parse_next(input)
}

// TODO: rename to func_exp?
fn collection_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    (
        retsname,
        Atom::LeftParen,
        separated(0.., exp, Atom::Comma),
        Atom::RightParen,
    )
        .map(|(identifier, _, args, _): (_, _, Vec<Expression>, _)| {
            Expression::from(FunctionNode::new(identifier.name(), args))
        })
        .parse_next(input)
}

fn value_exp(input: &mut &[Token<'_>]) -> PResult<Expression> {
    alt((
        spec_value,
        fieldname,
        char_value,
        float_value,
        int_value,
        time_value,
    ))
    .parse_next(input)
}

fn fieldname(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fn just_name(input: &mut &[Token<'_>]) -> PResult<Expression> {
        retsname
            .map(|val| Expression::from(FieldNode::new(val.name())))
            .parse_next(input)
    }
    fn last_with_name(input: &mut &[Token<'_>]) -> PResult<Expression> {
        preceded(BareKeyword::Last, retsname)
            .map(|val| Expression::from(LastFieldNode::new(val.name())))
            .parse_next(input)
    }
    let bracketed_last_with_name = delimited(Atom::LeftBracket, last_with_name, Atom::RightBracket);
    let bracketed_name = delimited(Atom::LeftBracket, just_name, Atom::RightBracket);

    alt((
        last_with_name,
        just_name,
        bracketed_last_with_name,
        bracketed_name,
    ))
    .parse_next(input)
}

fn spec_value(input: &mut &[Token<'_>]) -> PResult<Expression> {
    alt((
        DottedKeyword::Empty.value(Expression::from(LiteralNode::new(serde_json::Value::Null))),
        DottedKeyword::True.value(Expression::from(LiteralNode::new(serde_json::Value::Bool(
            true,
        )))),
        DottedKeyword::False.value(Expression::from(LiteralNode::new(serde_json::Value::Bool(
            false,
        )))),
        DottedKeyword::Now.value(Expression::from(FunctionNode::new("NOW", []))),
        DottedKeyword::Today.value(Expression::from(FunctionNode::new("TODAY", []))),
    ))
    .parse_next(input)
}

fn char_value(input: &mut &[Token<'_>]) -> PResult<Expression> {
    any.verify_map(|token: Token<'_>| match token {
        Token::SingleQuotedTerm(t) => Some(Expression::from(LiteralNode::new(json!(t.value())))),
        Token::DoubleQuotedTerm(t) => Some(Expression::from(LiteralNode::new(json!(t.value())))),
        _ => None,
    })
    .parse_next(input)
}

fn time_value(input: &mut &[Token<'_>]) -> PResult<Expression> {
    fail.parse_next(input)
}

fn int_value(input: &mut &[Token<'_>]) -> PResult<Expression> {
    any.verify_map(|token: Token<'_>| match token {
        Token::Int(t) => t
            .as_str()
            .parse::<i64>()
            .ok()
            .map(|val| Expression::from(LiteralNode::new(json!(val)))),
        _ => None,
    })
    .parse_next(input)
}

fn float_value(input: &mut &[Token<'_>]) -> PResult<Expression> {
    any.verify_map(|token: Token<'_>| match token {
        Token::Float(t) => t
            .as_str()
            .parse::<f64>()
            .ok()
            .map(|val| Expression::from(LiteralNode::new(json!(val)))),
        _ => None,
    })
    .parse_next(input)
}

fn retsname<'a>(input: &mut &[Token<'a>]) -> PResult<Identifier<'a>> {
    let token = one_of(|token| matches!(token, Token::Identifier(_))).parse_next(input)?;
    match token {
        Token::Identifier(id) => Ok(id),
        _ => unreachable!(),
    }
}
