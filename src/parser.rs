use crate::{
    AndNode, Expression, ExpressionOp, FieldNode, FunctionNode, IifNode, LastFieldNode, ListNode,
    LiteralNode, NotNode, OpNode, OrNode,
};
use alloc::string::String;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of, one_of},
    combinator::{eof, fail, map, map_res, opt, recognize, value},
    multi::{many0, many0_count, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use serde_json::json;

// Note that this is largely structured to match the ANTLR4 specification attached to the
// Validation Expression language spec at https://github.com/darnjo/rcp019/blob/master/rcp019.g4

pub(crate) fn top(input: &str) -> IResult<&str, Expression> {
    terminated(delimited(multispace0, exp, multispace0), eof)(input)
}

fn exp(input: &str) -> IResult<&str, Expression> {
    or_exp(input)
}

fn or_exp(input: &str) -> IResult<&str, Expression> {
    let (rest, mut list) =
        separated_list1(tuple((multispace0, tag(".OR."), multispace0)), and_exp)(input)?;

    let expr = if list.len() == 1 {
        list.pop().unwrap()
    } else {
        Expression::from(OrNode::new(list))
    };
    Ok((rest, expr))
}

fn and_exp(input: &str) -> IResult<&str, Expression> {
    let (rest, mut list) =
        separated_list1(tuple((multispace0, tag(".AND."), multispace0)), not_exp)(input)?;

    let expr = if list.len() == 1 {
        list.pop().unwrap()
    } else {
        Expression::from(AndNode::new(list))
    };
    Ok((rest, expr))
}

fn not_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        preceded(
            pair(tag(".NOT."), multispace0),
            map(not_exp, |exp| Expression::from(NotNode::new(exp))),
        ),
        eq_exp,
    ))(input)
}

fn eq_exp(input: &str) -> IResult<&str, Expression> {
    fn first_case(input: &str) -> IResult<&str, Expression> {
        let op = alt((
            map(tag("!="), |_| ExpressionOp::Ne),
            map(tag("="), |_| ExpressionOp::Eq),
        ));
        let (rest, (l, _, op, _, r)) =
            tuple((cmp_exp, multispace0, op, multispace0, cmp_exp))(input)?;
        let exp = Expression::from(OpNode::new(l, op, r));
        Ok((rest, exp))
    }

    alt((first_case, cmp_exp))(input)
}

fn cmp_exp(input: &str) -> IResult<&str, Expression> {
    fn first_case(input: &str) -> IResult<&str, Expression> {
        let op = alt((
            map(tag("<="), |_| ExpressionOp::Lte),
            map(tag("<"), |_| ExpressionOp::Lt),
            map(tag(">="), |_| ExpressionOp::Gte),
            map(tag(">"), |_| ExpressionOp::Gt),
        ));
        let (rest, (l, _, op, _, r)) =
            tuple((cnt_exp, multispace0, op, multispace0, cnt_exp))(input)?;
        let exp = Expression::from(OpNode::new(l, op, r));
        Ok((rest, exp))
    }

    alt((first_case, cnt_exp))(input)
}

fn cnt_exp(input: &str) -> IResult<&str, Expression> {
    fn first_case(input: &str) -> IResult<&str, Expression> {
        let op = alt((
            map(tag(".CONTAINS."), |_| ExpressionOp::Contains),
            map(tag(".IN."), |_| ExpressionOp::In),
        ));
        let (rest, (l, _, op, _, r)) =
            tuple((sum_exp, multispace0, op, multispace0, sum_exp))(input)?;
        let exp = Expression::from(OpNode::new(l, op, r));
        Ok((rest, exp))
    }

    alt((first_case, sum_exp))(input)
}

fn sum_exp(input: &str) -> IResult<&str, Expression> {
    fn op(input: &str) -> IResult<&str, ExpressionOp> {
        alt((
            map(tag("+"), |_| ExpressionOp::Add),
            map(tag("-"), |_| ExpressionOp::Sub),
            map(tag("||"), |_| ExpressionOp::Concat),
        ))(input)
    }

    let (rest, (expr, exprs)) = pair(
        prod_exp,
        many0(pair(
            preceded(multispace0, op),
            preceded(multispace0, prod_exp),
        )),
    )(input)?;

    let expr = exprs.into_iter().fold(expr, |existing, (op, new)| {
        Expression::from(OpNode::new(existing, op, new))
    });

    Ok((rest, expr))
}

fn prod_exp(input: &str) -> IResult<&str, Expression> {
    fn op(input: &str) -> IResult<&str, ExpressionOp> {
        alt((
            map(tag("*"), |_| ExpressionOp::Mul),
            map(tag("/"), |_| ExpressionOp::Div),
        ))(input)
    }

    let (rest, (expr, exprs)) = pair(
        atom_exp,
        many0(pair(
            preceded(multispace0, op),
            preceded(multispace0, atom_exp),
        )),
    )(input)?;

    let expr = exprs.into_iter().fold(expr, |existing, (op, new)| {
        Expression::from(OpNode::new(existing, op, new))
    });

    Ok((rest, expr))
}

fn atom_exp(input: &str) -> IResult<&str, Expression> {
    fn parenthesized_exp(input: &str) -> IResult<&str, Expression> {
        map(
            tuple((tag("("), multispace0, exp, multispace0, tag(")"))),
            |tuple| tuple.2,
        )(input)
    }
    alt((func_exp, collection_exp, parenthesized_exp, list, value_exp))(input)
}

fn list(input: &str) -> IResult<&str, Expression> {
    let lparen = delimited(multispace0, tag("("), multispace0);
    let rparen = delimited(multispace0, tag(")"), multispace0);
    let args = separated_list0(delimited(multispace0, tag(","), multispace0), exp);

    let (rest, (_lparen, args, _rparen)) = tuple((lparen, args, rparen))(input)?;

    // TODO: Dedicated ListNode
    let expression = Expression::from(ListNode::new(args));

    Ok((rest, expression))
}

fn func_exp(input: &str) -> IResult<&str, Expression> {
    let (
        rest,
        (_iif, _lparen, test_exp, _first_comma, true_exp, _second_comma, false_exp, _rparen),
    ) = tuple((
        tag("IIF"),
        delimited(multispace0, tag("("), multispace0),
        exp,
        delimited(multispace0, tag(","), multispace0),
        exp,
        delimited(multispace0, tag(","), multispace0),
        exp,
        preceded(multispace0, tag(")")),
    ))(input)?;

    let exp = Expression::from(IifNode::new(test_exp, true_exp, false_exp));
    Ok((rest, exp))
}

fn collection_exp(input: &str) -> IResult<&str, Expression> {
    let lparen = delimited(multispace0, tag("("), multispace0);
    let rparen = delimited(multispace0, tag(")"), multispace0);
    let args = separated_list0(delimited(multispace0, tag(","), multispace0), exp);

    let (rest, (name, _lparen, args, _rparen)) = tuple((retsname, lparen, args, rparen))(input)?;

    let expression = Expression::from(FunctionNode::new(name, args));

    Ok((rest, expression))
}

fn value_exp(input: &str) -> IResult<&str, Expression> {
    alt((
        spec_value,
        fieldname,
        char_value,
        float_value,
        int_value,
        time_value,
    ))(input)
}

fn fieldname(input: &str) -> IResult<&str, Expression> {
    fn last_with_name(input: &str) -> IResult<&str, &str> {
        preceded(pair(tag("LAST"), multispace1), retsname)(input)
    }
    let brackeded_last_with_name = delimited(tag("["), last_with_name, tag("]"));
    let bracketed_name = delimited(tag("["), retsname, tag("]"));

    alt((
        map(last_with_name, |s| Expression::from(LastFieldNode::new(s))),
        map(retsname, |s| Expression::from(FieldNode::new(s))),
        map(brackeded_last_with_name, |s| {
            Expression::from(LastFieldNode::new(s))
        }),
        map(bracketed_name, |s| Expression::from(FieldNode::new(s))),
    ))(input)
}

fn spec_value(input: &str) -> IResult<&str, Expression> {
    let value = alt((
        map(tag("EMPTY"), |_| {
            Expression::from(LiteralNode::new(serde_json::Value::Null))
        }),
        map(tag("TRUE"), |_| {
            Expression::from(LiteralNode::new(serde_json::Value::Bool(true)))
        }),
        map(tag("FALSE"), |_| {
            Expression::from(LiteralNode::new(serde_json::Value::Bool(false)))
        }),
        map(tag("NOW"), |_| {
            Expression::from(FunctionNode::new("NOW", []))
        }),
        map(tag("TODAY"), |_| {
            Expression::from(FunctionNode::new("TODAY", []))
        }),
    ));

    delimited(tag("."), value, tag("."))(input)
}

fn char_value(input: &str) -> IResult<&str, Expression> {
    let double_quote_parser = escaped_transform(
        none_of("\"\\"),
        '\\',
        alt((
            value("\\", tag("\\")),
            value("\n", tag("n")),
            value("\t", tag("t")),
            value("\"", tag("\"")),
        )),
    );

    let single_quote_parser = escaped_transform(
        none_of("'\\"),
        '\\',
        alt((value("\\", tag("\\")), value("\'", tag("\'")))),
    );

    map(
        alt((
            map(tuple((tag("\""), tag("\""))), |_| String::new()),
            map(tuple((tag("'"), tag("'"))), |_| String::new()),
            delimited(tag("\""), double_quote_parser, tag("\"")),
            delimited(tag("'"), single_quote_parser, tag("'")),
        )),
        |s| Expression::from(LiteralNode::new(serde_json::Value::String(s))),
    )(input)
}

fn time_value(input: &str) -> IResult<&str, Expression> {
    // TODO
    fail(input)
}

fn int_value(input: &str) -> IResult<&str, Expression> {
    alt((
        map(nom::character::complete::u64, |v| {
            Expression::from(LiteralNode::new(json!(v)))
        }),
        map(nom::character::complete::i64, |v| {
            Expression::from(LiteralNode::new(json!(v)))
        }),
    ))(input)
}

fn float_value(input: &str) -> IResult<&str, Expression> {
    // From nom recipies
    fn float(input: &str) -> IResult<&str, &str> {
        alt((
            // Case one: .42
            recognize(tuple((
                char('.'),
                decimal,
                opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
            ))), // Case two: 42e42 and 42.42e42
            recognize(tuple((
                decimal,
                opt(preceded(char('.'), decimal)),
                one_of("eE"),
                opt(one_of("+-")),
                decimal,
            ))), // Case three: 42. and 42.42
            recognize(tuple((decimal, char('.'), opt(decimal)))),
        ))(input)
    }

    // From nom recipies
    fn decimal(input: &str) -> IResult<&str, &str> {
        recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
    }

    map_res(float, |s| {
        s.parse::<f64>()
            .map(|f| Expression::from(LiteralNode::new(json!(f))))
    })(input)
}

fn retsname(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}
