use super::{Expression, ExpressionOp};
use alloc::string::{String, ToString};
use core::fmt::Write;

impl Expression {
    /// Serialize the expression to a string
    pub fn serialize(&self) -> Result<String, core::fmt::Error> {
        let mut writer = String::new();
        self.format(&mut writer)?;
        Ok(writer)
    }

    fn format_precedence(&self) -> i32 {
        match self {
            Expression::Field(_) => 0,
            Expression::LastField(_) => 0,
            Expression::Literal(_) => 0,
            Expression::Function(_) => 1,
            Expression::Iif(_) => 1,
            Expression::List(_) => 1, // TODO: What's the right precedence here?
            Expression::Op(_) => 2,
            Expression::Not(_) => 3,
            Expression::And(_) => 4,
            Expression::Or(_) => 4,
        }
    }

    /// Serialize the expression to a writer
    pub fn format<W>(&self, writer: &mut W) -> Result<(), core::fmt::Error>
    where
        W: Write,
    {
        let current_precedence = self.format_precedence();
        let format_with_precedence =
            |writer: &mut W, expr: &Expression| -> Result<(), core::fmt::Error> {
                if expr.format_precedence() >= current_precedence {
                    writer.write_char('(')?;
                    expr.format(writer)?;
                    writer.write_char(')')?;
                    Ok(())
                } else {
                    expr.format(writer)
                }
            };

        match self {
            Expression::Field(ref node) => writer.write_str(node.name()),
            Expression::LastField(ref node) => {
                writer.write_str("LAST ")?;
                writer.write_str(node.name())?;
                Ok(())
            }
            Expression::And(ref node) => {
                for (idx, expr) in node.iter().enumerate() {
                    if idx > 0 {
                        writer.write_str(" .AND. ")?;
                    }
                    format_with_precedence(writer, expr)?;
                }
                Ok(())
            }
            Expression::Or(ref node) => {
                for (idx, expr) in node.iter().enumerate() {
                    if idx > 0 {
                        writer.write_str(" .OR. ")?;
                    }
                    format_with_precedence(writer, expr)?;
                }
                Ok(())
            }
            Expression::Not(ref node) => {
                writer.write_str(".NOT. ")?;
                format_with_precedence(writer, node.expression())?;
                Ok(())
            }
            Expression::Op(ref node) => {
                format_with_precedence(writer, node.left())?;
                writer.write_char(' ')?;
                writer.write_str(node.op().format())?;
                writer.write_char(' ')?;
                format_with_precedence(writer, node.right())?;
                Ok(())
            }
            Expression::Literal(ref node) => match node.value() {
                serde_json::Value::Null => writer.write_str(".EMPTY."),
                serde_json::Value::Bool(true) => writer.write_str(".TRUE."),
                serde_json::Value::Bool(false) => writer.write_str(".FALSE."),
                serde_json::Value::Number(n) => writer.write_str(&n.to_string()),
                serde_json::Value::String(_) => {
                    let string =
                        serde_json::to_string(node.value()).map_err(|_| core::fmt::Error)?;
                    writer.write_str(&string)?;
                    Ok(())
                }
                serde_json::Value::Array(_) => Err(core::fmt::Error),
                serde_json::Value::Object(_) => Err(core::fmt::Error),
            },
            Expression::Function(ref node) => {
                if node.name() == "NOW" {
                    return writer.write_str(".NOW.");
                }
                if node.name() == "TODAY" {
                    return writer.write_str(".TODAY.");
                }

                writer.write_str(node.name())?;
                writer.write_char('(')?;
                for (idx, expr) in node.iter().enumerate() {
                    if idx > 0 {
                        writer.write_str(", ")?;
                    }

                    expr.format(writer)?;
                }
                writer.write_char(')')?;
                Ok(())
            }
            Expression::Iif(ref node) => {
                writer.write_str("IIF(")?;
                node.test().format(writer)?;
                writer.write_str(", ")?;
                node.t().format(writer)?;
                writer.write_str(", ")?;
                node.f().format(writer)?;
                writer.write_char(')')?;
                Ok(())
            }
            Expression::List(ref node) => {
                writer.write_char('(')?;
                for (idx, expr) in node.iter().enumerate() {
                    if idx > 0 {
                        writer.write_str(", ")?;
                    }

                    expr.format(writer)?;
                }
                writer.write_char(')')?;
                Ok(())
            }
        }
    }
}

impl ExpressionOp {
    fn format(&self) -> &'static str {
        match self {
            ExpressionOp::Add => "+",
            ExpressionOp::Sub => "-",
            ExpressionOp::Mul => "*",
            ExpressionOp::Div => "/",
            ExpressionOp::Mod => ".MOD.",
            ExpressionOp::Concat => "||",
            ExpressionOp::Lt => "<",
            ExpressionOp::Lte => "<=",
            ExpressionOp::Gt => ">",
            ExpressionOp::Gte => ">=",
            ExpressionOp::Eq => "=",
            ExpressionOp::Ne => "!=",
            ExpressionOp::Contains => ".CONTAINS.",
            ExpressionOp::In => ".IN.",
        }
    }
}

impl core::fmt::Debug for Expression {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        struct InnerDebug<'a>(&'a Expression);

        impl<'a> core::fmt::Debug for InnerDebug<'a> {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                self.0.format(f)
            }
        }

        f.debug_tuple("Expression")
            .field(&InnerDebug(self))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_few_round_trips_to_make_sure_we_arent_adding_too_many_parentheses() {
        let exprs = [
            "Field + 3",
            "Field .MOD. 3",
            "Field > 1 .AND. Field < 100",
            "FieldA > 1 .AND. (FieldB < 20 .OR. FieldB = 42)",
            ".NOT. T",
            ".NOT. (T .OR. F)",
        ];

        for expr in exprs {
            let parsed = expr.parse::<Expression>().unwrap();
            let serialized = parsed.serialize().unwrap();
            assert_eq!(serialized, expr);
        }
    }
}
