//! An implementation of [RCP 19][rcp19] (RETS Validation Expressions) from the [RESO Transport
//! group][transport].
//!
//! [transport]: https://github.com/RESOStandards/transport
//! [rcp19]: https://github.com/RESOStandards/transport/blob/main/web-api-validation-expression.md
//!
//! ## Example
//!
//! ```
//! use rets_expression::{Expression, Engine, EvaluateContext};
//! use serde_json::json;
//!
//! // Parse an expression
//! let expression = "MlsStatus .IN. ('Active', 'Pending') .AND. (ListPrice >= 1 .OR. LAST MlsStatus = 'Incomplete')"
//!     .parse::<Expression>()
//!     .unwrap();
//!
//! // Create the property data to run the expression against
//! let value = json!({
//!     "MlsStatus": "Active",
//!     "ListPrice": 1000000
//! });
//! // Create the previous property data to run the expression against (for when the expression
//! // includes references to previous data, like `LAST FieldName`)
//! let previous_value = json!({
//!     "MlsStatus": "Incomplete",
//!     "ListPrice": 0
//! });
//!
//! // Create a default engine and a context in which to evaluate the expression
//! let engine = Engine::default();
//! let context = EvaluateContext::new(&engine, &value).with_previous(&previous_value);
//!
//! // Evaluate the expression!
//! let value = expression.apply(context).unwrap();
//! assert_eq!(value.into_owned(), json!(true));
//! ```
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs)]
extern crate alloc;

use alloc::{
    borrow::Cow,
    boxed::Box,
    collections::BTreeMap,
    format,
    string::{String, ToString},
    vec::Vec,
};
use chrono::{DateTime, NaiveDate};
pub use context::{Engine, EvaluateContext};
use core::str::FromStr;
use serde_json::Value;

mod context;
mod formatter;
pub mod function;
mod parser;

/// An expression that can be inspected or evaluated
#[derive(Clone, Eq, PartialEq)]
pub enum Expression {
    /// A node representing a reference to a field
    ///
    /// E.g. `MlsStatus`
    Field(FieldNode),
    /// A node representing a reference to a field from the previous value
    ///
    /// E.g. `LAST MlsStatus`
    LastField(LastFieldNode),
    /// A node representing the logical conjunction of two or more expressions
    ///
    /// E.g. `A .AND. B`
    And(AndNode),
    /// A node representing the logical disjunction of two or more expressions
    ///
    /// E.g. `A .OR. B`
    Or(OrNode),
    /// A node representing the logical negation of an expression
    ///
    /// E.g. `.NOT. A`
    Not(NotNode),
    /// A node representing a binary operation
    ///
    /// E.g. `A + B`
    Op(OpNode),
    /// A node representing a literal value
    ///
    /// E.g. `.TRUE.`
    Literal(LiteralNode),
    /// A node representing a function call
    ///
    /// E.g. `SUBSTR('A', 1, 2)`
    Function(FunctionNode),
    /// A node representing the special `IIF` conditional syntax
    ///
    /// E.g. `IIF(A, B, C)`
    Iif(IifNode),
    /// A node representing a list of items
    ///
    /// E.g. `(1, 2, 3)`
    List(ListNode),
}

impl Expression {
    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        loop {
            visitor.visit_expression_in(self);
            match self {
                Expression::Field(_) => {
                    visitor.visit_field_expression(self);
                    if let Some(node) = self.as_field_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                }
                Expression::LastField(_) => {
                    visitor.visit_last_field_expression(self);
                    if let Some(node) = self.as_last_field_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                }
                Expression::And(_) => {
                    visitor.visit_and_expression_in(self);
                    if let Some(node) = self.as_and_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_and_expression_out(self);
                }
                Expression::Or(_) => {
                    visitor.visit_or_expression_in(self);
                    if let Some(node) = self.as_or_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_or_expression_out(self);
                }
                Expression::Not(_) => {
                    visitor.visit_not_expression_in(self);
                    if let Some(node) = self.as_not_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_not_expression_out(self);
                }
                Expression::Op(_) => {
                    visitor.visit_op_expression_in(self);
                    if let Some(node) = self.as_op_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_op_expression_out(self);
                }
                Expression::Literal(_) => {
                    visitor.visit_literal_expression(self);
                    if let Some(node) = self.as_literal_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                }
                Expression::Function(_) => {
                    visitor.visit_function_expression_in(self);
                    if let Some(node) = self.as_function_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_function_expression_out(self);
                }
                Expression::Iif(_) => {
                    visitor.visit_iif_expression_in(self);
                    if let Some(node) = self.as_iif_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_iif_expression_out(self);
                }
                Expression::List(_) => {
                    visitor.visit_list_expression_in(self);
                    if let Some(node) = self.as_list_mut() {
                        node.accept(visitor);
                    } else {
                        continue;
                    }
                    visitor.visit_list_expression_out(self);
                }
            }
            visitor.visit_expression_out(self);
            break;
        }
    }
}

macro_rules! define_as_function {
    ($as_ref_name:ident, $as_mut_name:ident, $discriminant:ident, $node_type:ty) => {
        /// If the [Expression] is a X, returns a reference to the associated Y. Returns `None`
        /// otherwise.
        pub fn $as_ref_name(&self) -> Option<&$node_type> {
            match self {
                Expression::$discriminant(node) => Some(node),
                _ => None,
            }
        }

        /// If the [Expression] is a X, returns a mutable reference to the associated Y. Returns
        /// `None` otherwise.
        pub fn $as_mut_name(&mut self) -> Option<&mut $node_type> {
            match self {
                Expression::$discriminant(node) => Some(node),
                _ => None,
            }
        }
    };
}

impl Expression {
    define_as_function!(as_field, as_field_mut, Field, FieldNode);
    define_as_function!(as_last_field, as_last_field_mut, LastField, LastFieldNode);
    define_as_function!(as_and, as_and_mut, And, AndNode);
    define_as_function!(as_or, as_or_mut, Or, OrNode);
    define_as_function!(as_not, as_not_mut, Not, NotNode);
    define_as_function!(as_op, as_op_mut, Op, OpNode);
    define_as_function!(as_literal, as_literal_mut, Literal, LiteralNode);
    define_as_function!(as_function, as_function_mut, Function, FunctionNode);
    define_as_function!(as_iif, as_iif_mut, Iif, IifNode);
    define_as_function!(as_list, as_list_mut, List, ListNode);
}

#[derive(Clone, Eq, PartialEq)]
struct Span {
    start: usize,
    end: usize,
}

/// A node representing a reference to a field
///
/// E.g. `MlsStatus`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct FieldNode {
    /// The name of the field that is being referenced
    pub name: String,
    span: Option<Span>,
}

impl FieldNode {
    /// Create a new node
    pub fn new(name: impl ToString) -> Self {
        FieldNode {
            name: name.to_string(),
            span: None,
        }
    }

    /// The name of the field that is being referenced
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_field_node(self)
    }
}

impl From<FieldNode> for Expression {
    fn from(node: FieldNode) -> Expression {
        Expression::Field(node)
    }
}

impl<T> From<T> for FieldNode
where
    T: ToString,
{
    fn from(name: T) -> Self {
        FieldNode::new(name)
    }
}

/// A node representing a reference to a field from the previous value
///
/// E.g. `LAST MlsStatus`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct LastFieldNode {
    /// The name of the field that is being referenced
    pub name: String,
    span: Option<Span>,
}

impl LastFieldNode {
    /// Create a new node
    pub fn new(name: impl ToString) -> Self {
        LastFieldNode {
            name: name.to_string(),
            span: None,
        }
    }

    /// The name of the field that is being referenced
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_last_field_node(self)
    }
}

impl From<LastFieldNode> for Expression {
    fn from(node: LastFieldNode) -> Expression {
        Expression::LastField(node)
    }
}

impl<T> From<T> for LastFieldNode
where
    T: ToString,
{
    fn from(name: T) -> Self {
        LastFieldNode::new(name)
    }
}

/// A node representing the logical conjunction of two or more expressions
///
/// E.g. `A .AND. B`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct AndNode {
    /// The expressions that are and-ed together
    pub children: Vec<Expression>,
    span: Option<Span>,
}

impl AndNode {
    /// Create a new node
    pub fn new(children: impl Expressions) -> Self {
        AndNode {
            children: children.to_vec(),
            span: None,
        }
    }

    /// An iterator over the expressions
    pub fn iter(&self) -> impl Iterator<Item = &Expression> {
        self.children.iter()
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_and_node_in(self);
        for child in &mut self.children {
            child.accept(visitor)
        }
        visitor.visit_and_node_out(self);
    }
}

impl From<AndNode> for Expression {
    fn from(node: AndNode) -> Expression {
        Expression::And(node)
    }
}

impl<T> From<T> for AndNode
where
    T: Expressions,
{
    fn from(expressions: T) -> Self {
        AndNode::new(expressions)
    }
}

/// A node representing the logical disjunction of two or more expressions
///
/// E.g. `A .OR. B`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct OrNode {
    /// The expressions that are or-ed together
    pub children: Vec<Expression>,
    span: Option<Span>,
}

impl OrNode {
    /// Create a new node
    pub fn new(children: impl Expressions) -> Self {
        OrNode {
            children: children.to_vec(),
            span: None,
        }
    }

    /// An iterator over the expressions
    pub fn iter(&self) -> impl Iterator<Item = &Expression> {
        self.children.iter()
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_or_node_in(self);
        for child in &mut self.children {
            child.accept(visitor)
        }
        visitor.visit_or_node_out(self);
    }
}

impl From<OrNode> for Expression {
    fn from(node: OrNode) -> Expression {
        Expression::Or(node)
    }
}

impl<T> From<T> for OrNode
where
    T: Expressions,
{
    fn from(expressions: T) -> Self {
        OrNode::new(expressions)
    }
}

/// A node representing the logical negation of an expression
///
/// E.g. `.NOT. A`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct NotNode {
    /// The negated expression
    pub expression: Box<Expression>,
    span: Option<Span>,
}

impl NotNode {
    /// Create a new node
    pub fn new(expression: impl Into<Box<Expression>>) -> Self {
        NotNode {
            expression: expression.into(),
            span: None,
        }
    }

    /// The negated expression
    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_not_node_in(self);
        self.expression.accept(visitor);
        visitor.visit_not_node_out(self);
    }
}

impl From<NotNode> for Expression {
    fn from(node: NotNode) -> Expression {
        Expression::Not(node)
    }
}

/// A node representing a binary operation
///
/// E.g. `A + B`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct OpNode {
    /// The expression on the left of the operation
    pub left: Box<Expression>,
    /// The operation to perform
    pub op: ExpressionOp,
    /// The expression on the left of the operation
    pub right: Box<Expression>,
    op_span: Option<Span>,
}

impl OpNode {
    /// Create a new node
    pub fn new(
        left: impl Into<Box<Expression>>,
        op: ExpressionOp,
        right: impl Into<Box<Expression>>,
    ) -> Self {
        OpNode {
            left: left.into(),
            op,
            right: right.into(),
            op_span: None,
        }
    }

    /// The expression on the left of the operation
    pub fn left(&self) -> &Expression {
        &self.left
    }
    /// The expression on the left of the operation
    pub fn right(&self) -> &Expression {
        &self.right
    }
    /// The operation being performed
    pub fn op(&self) -> &ExpressionOp {
        &self.op
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_op_node_in(self);
        self.left.accept(visitor);
        self.right.accept(visitor);
        visitor.visit_op_node_out(self);
    }
}

macro_rules! convenience_op_node {
    ($ident:ident, $op:expr) => {
        /// Create a new node
        pub fn $ident(
            left: impl Into<Box<Expression>>,
            right: impl Into<Box<Expression>>,
        ) -> OpNode {
            OpNode::new(left, $op, right)
        }
    };
}

impl OpNode {
    convenience_op_node!(add, ExpressionOp::Add);
    convenience_op_node!(sub, ExpressionOp::Sub);
    convenience_op_node!(mul, ExpressionOp::Mul);
    convenience_op_node!(div, ExpressionOp::Div);
    convenience_op_node!(gt, ExpressionOp::Gt);
    convenience_op_node!(gte, ExpressionOp::Gte);
    convenience_op_node!(lt, ExpressionOp::Lt);
    convenience_op_node!(lte, ExpressionOp::Lte);
    convenience_op_node!(eq, ExpressionOp::Eq);
    convenience_op_node!(ne, ExpressionOp::Ne);
}

impl From<OpNode> for Expression {
    fn from(node: OpNode) -> Expression {
        Expression::Op(node)
    }
}

/// A node representing a literal value
///
/// E.g. `.TRUE.`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct LiteralNode {
    /// The literal value
    pub value: Value,
    span: Option<Span>,
}

impl LiteralNode {
    /// Create a new node
    pub fn new(value: Value) -> Self {
        LiteralNode { value, span: None }
    }

    /// The literal value
    pub fn value(&self) -> &Value {
        &self.value
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_literal_node(self);
    }
}

impl From<LiteralNode> for Expression {
    fn from(node: LiteralNode) -> Expression {
        Expression::Literal(node)
    }
}

/// A node representing a function call
///
/// E.g. `SUBSTR('A', 1, 2)`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct FunctionNode {
    /// The name of the function to execute
    pub name: String,
    /// The arguments to the function
    pub arguments: Vec<Expression>,
    name_span: Option<Span>,
    left_parens_span: Option<Span>,
    right_parens_span: Option<Span>,
}

impl FunctionNode {
    /// Create a new node
    pub fn new(name: impl ToString, arguments: impl Expressions) -> Self {
        FunctionNode {
            name: name.to_string(),
            arguments: arguments.to_vec(),
            name_span: None,
            left_parens_span: None,
            right_parens_span: None,
        }
    }

    /// The name of the function to execute
    pub fn name(&self) -> &str {
        &self.name
    }

    /// An iterator over the arguments of the function
    pub fn iter(&self) -> impl Iterator<Item = &Expression> {
        self.arguments.iter()
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_function_node_in(self);
        for argument in &mut self.arguments {
            argument.accept(visitor);
        }
        visitor.visit_function_node_out(self);
    }
}

impl From<FunctionNode> for Expression {
    fn from(node: FunctionNode) -> Expression {
        Expression::Function(node)
    }
}

/// A node representing the special `IIF` conditional syntax
///
/// E.g. `IIF(A, B, C)`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct IifNode {
    /// The expression that represents the test or the conditional
    pub test_expression: Box<Expression>,
    /// The expression to execute if the test returns true
    pub true_expression: Box<Expression>,
    /// The expression to execute if the test returns false
    pub false_expression: Box<Expression>,
    span: Option<Span>,
}

impl IifNode {
    /// Create a new node
    pub fn new(
        test: impl Into<Box<Expression>>,
        t: impl Into<Box<Expression>>,
        f: impl Into<Box<Expression>>,
    ) -> Self {
        IifNode {
            test_expression: test.into(),
            true_expression: t.into(),
            false_expression: f.into(),
            span: None,
        }
    }

    /// The expression that represents the test or the conditional
    pub fn test(&self) -> &Expression {
        &self.test_expression
    }

    /// The expression to execute if the test returns true
    pub fn t(&self) -> &Expression {
        &self.true_expression
    }

    /// The expression to execute if the test returns false
    pub fn f(&self) -> &Expression {
        &self.false_expression
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_iif_node_in(self);
        self.test_expression.accept(visitor);
        self.true_expression.accept(visitor);
        self.false_expression.accept(visitor);
        visitor.visit_iif_node_out(self);
    }
}

impl From<IifNode> for Expression {
    fn from(node: IifNode) -> Expression {
        Expression::Iif(node)
    }
}

/// A node representing a list of items
///
/// E.g. `(1, 2, 3)`
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct ListNode {
    /// The items in the list
    pub items: Vec<Expression>,
    span: Option<Span>,
}

impl ListNode {
    /// Create a new node
    pub fn new(items: impl Expressions) -> Self {
        ListNode {
            items: items.to_vec(),
            span: None,
        }
    }

    /// Iterate over the expressions in the list
    pub fn iter(&self) -> impl Iterator<Item = &Expression> {
        self.items.iter()
    }

    /// Visit this node with the provided visitor
    pub fn accept(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_list_node_in(self);
        for argument in &mut self.items {
            argument.accept(visitor);
        }
        visitor.visit_list_node_out(self);
    }
}

impl From<ListNode> for Expression {
    fn from(node: ListNode) -> Expression {
        Expression::List(node)
    }
}

impl FromStr for Expression {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::parse::parse(s) {
            Ok(expr) => Ok(expr),
            Err(s) => Err(s),
        }
    }
}

/// A binary operation in an [`OpNode`]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ExpressionOp {
    /// Add two expressions `A + B`
    Add,
    /// Subtract one expression from another `A - B`
    Sub,
    /// Multiple two expressions `A * B`
    Mul,
    /// Divide one expression by another `A / B`
    Div,
    /// Take the modulus of one number with another `A .MOD. B`
    Mod,
    /// Concatenate two strings `A || B`
    Concat,
    /// Compare two expressions using a less-than comparison `A < B`
    Lt,
    /// Compare two expressions using a less-than-or-equal-to comparison `A <= B`
    Lte,
    /// Compare two expressions using a greater-than comparison `A > B`
    Gt,
    /// Compare two expressions using a greater-than-or-equal-to comparison `A >= B`
    Gte,
    /// Compare two expressions using an equal-to comparison `A = B`
    Eq,
    /// Compare two expressions using a not-equal-to comparison `A != B`
    Ne,
    /// Determine if one expression contains another expression `A .CONTAINS. B`
    Contains,
    /// Determine if one expression is in another expression `A .IN. B`
    In,
}

impl ExpressionOp {
    /// Apply the operation to two values
    pub fn apply(self, left: &Value, right: &Value) -> Result<Value, Error> {
        fn boolean(input: bool) -> Result<Value, Error> {
            Ok(Value::Bool(input))
        }

        fn number<F, I, T1, T2>(
            left: &serde_json::Number,
            right: &serde_json::Number,
            f: F,
            i: I,
        ) -> Result<Value, Error>
        where
            F: Fn(f64, f64) -> T1,
            I: Fn(i64, i64) -> T2,
            T1: Into<serde_json::Value>,
            T2: Into<serde_json::Value>,
        {
            // If both are i64s, treat them call the function for i64s
            if let (Some(i_left), Some(i_right)) = (left.as_i64(), right.as_i64()) {
                return Ok(i(i_left, i_right).into());
            }

            let f_left = left.as_f64().ok_or(Error::InvalidNumber)?;
            let f_right = right.as_f64().ok_or(Error::InvalidNumber)?;
            Ok(f(f_left, f_right).into())
        }

        fn string(input: String) -> Result<Value, Error> {
            Ok(Value::String(input))
        }

        fn number_res<F, I, T1, T2>(
            left: &serde_json::Number,
            right: &serde_json::Number,
            f: F,
            i: I,
        ) -> Result<Value, Error>
        where
            F: Fn(f64, f64) -> Result<T1, Error>,
            I: Fn(i64, i64) -> Result<T2, Error>,
            T1: Into<serde_json::Value>,
            T2: Into<serde_json::Value>,
        {
            // If both are i64s, treat them call the function for i64s
            if let (Some(i_left), Some(i_right)) = (left.as_i64(), right.as_i64()) {
                return Ok(i(i_left, i_right)?.into());
            }

            let f_left = left.as_f64().ok_or(Error::InvalidNumber)?;
            let f_right = right.as_f64().ok_or(Error::InvalidNumber)?;
            Ok(f(f_left, f_right)?.into())
        }

        fn add_date_number(left: &str, right: &serde_json::Number) -> Result<Value, Error> {
            if let Ok(date) = NaiveDate::parse_from_str(left, "%Y-%m-%d") {
                let days = right.as_f64().ok_or(Error::InvalidNumber)? as u64;
                let new_date = date
                    .checked_add_days(chrono::Days::new(days))
                    .ok_or(Error::InvalidType)?;
                Ok(Value::String(new_date.format("%Y-%m-%d").to_string()))
            } else if let Ok(timestamp) = DateTime::parse_from_rfc3339(left) {
                let days = right.as_f64().ok_or(Error::InvalidNumber)?;
                let milliseconds = (days * 24.0 * 60.0 * 60.0 * 1000.0) as i64;
                let new_timestamp = timestamp
                    .checked_add_signed(chrono::Duration::milliseconds(milliseconds))
                    .ok_or(Error::InvalidType)?;
                Ok(Value::String(
                    new_timestamp.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
                ))
            } else {
                Err(Error::InvalidType)
            }
        }

        fn add_number_date(left: &serde_json::Number, right: &str) -> Result<Value, Error> {
            add_date_number(right, left)
        }

        fn subtract_date_number(left: &str, right: &serde_json::Number) -> Result<Value, Error> {
            if let Ok(date) = NaiveDate::parse_from_str(left, "%Y-%m-%d") {
                let days = right.as_f64().ok_or(Error::InvalidNumber)? as u64;
                let new_date = date
                    .checked_sub_days(chrono::Days::new(days))
                    .ok_or(Error::InvalidType)?;
                Ok(Value::String(new_date.format("%Y-%m-%d").to_string()))
            } else if let Ok(timestamp) = DateTime::parse_from_rfc3339(left) {
                let days = right.as_f64().ok_or(Error::InvalidNumber)?;
                let milliseconds = (days * 24.0 * 60.0 * 60.0 * 1000.0) as i64;
                let new_timestamp = timestamp
                    .checked_sub_signed(chrono::Duration::milliseconds(milliseconds))
                    .ok_or(Error::InvalidType)?;
                Ok(Value::String(
                    new_timestamp.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
                ))
            } else {
                Err(Error::InvalidType)
            }
        }

        fn subtract_date_date(left: &str, right: &str) -> Result<Value, Error> {
            if let (Ok(left), Ok(right)) = (
                NaiveDate::parse_from_str(left, "%Y-%m-%d"),
                NaiveDate::parse_from_str(right, "%Y-%m-%d"),
            ) {
                let duration = left.signed_duration_since(right);
                Ok(Value::Number(serde_json::Number::from(duration.num_days())))
            } else if let (Ok(left), Ok(right)) = (
                DateTime::parse_from_rfc3339(left),
                DateTime::parse_from_rfc3339(right),
            ) {
                let duration = left.signed_duration_since(right);
                let millis = duration.num_milliseconds();
                let days = millis as f64 / (1000.0 * 60.0 * 60.0 * 24.0);
                Ok(Value::Number(
                    serde_json::Number::from_f64(days).ok_or(Error::InvalidNumber)?,
                ))
            } else {
                Err(Error::InvalidType)
            }
        }

        match (self, left, right) {
            (Self::Eq, Value::Null, Value::Null) => boolean(true),
            (Self::Eq, Value::Null, _) => boolean(false),
            (Self::Eq, _, Value::Null) => boolean(false),
            (Self::Eq, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a == b),
            (Self::Eq, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a == b, |a, b| a == b)
            }
            (Self::Eq, Value::String(ref a), Value::String(ref b)) => boolean(a == b),
            (Self::Eq, _, _) => boolean(false),

            (Self::Ne, Value::Null, Value::Null) => boolean(false),
            (Self::Ne, Value::Null, _) => boolean(true),
            (Self::Ne, _, Value::Null) => boolean(true),
            (Self::Ne, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a != b),
            (Self::Ne, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a != b, |a, b| a != b)
            }
            (Self::Ne, Value::String(ref a), Value::String(ref b)) => boolean(a != b),
            (Self::Ne, _, _) => boolean(true),

            (Self::Lt, Value::Null, Value::Null) => boolean(false),
            (Self::Lt, Value::Null, _) => boolean(true),
            (Self::Lt, _, Value::Null) => boolean(false),
            (Self::Lt, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a < b),
            (Self::Lt, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a < b, |a, b| a < b)
            }
            (Self::Lt, Value::String(ref a), Value::String(ref b)) => boolean(a < b),
            (Self::Lt, _, _) => Err(Error::InvalidType),

            (Self::Lte, Value::Null, Value::Null) => boolean(true),
            (Self::Lte, Value::Null, _) => boolean(true),
            (Self::Lte, _, Value::Null) => boolean(false),
            (Self::Lte, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a <= b),
            (Self::Lte, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a <= b, |a, b| a <= b)
            }
            (Self::Lte, Value::String(ref a), Value::String(ref b)) => boolean(a <= b),
            (Self::Lte, _, _) => Err(Error::InvalidType),

            (Self::Gt, Value::Null, Value::Null) => boolean(false),
            (Self::Gt, Value::Null, _) => boolean(false),
            (Self::Gt, _, Value::Null) => boolean(true),
            (Self::Gt, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a > b),
            (Self::Gt, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a > b, |a, b| a > b)
            }
            (Self::Gt, Value::String(ref a), Value::String(ref b)) => boolean(a > b),
            (Self::Gt, _, _) => Err(Error::InvalidType),

            (Self::Gte, Value::Null, Value::Null) => boolean(true),
            (Self::Gte, Value::Null, _) => boolean(false),
            (Self::Gte, _, Value::Null) => boolean(true),
            (Self::Gte, Value::Bool(ref a), Value::Bool(ref b)) => boolean(a >= b),
            (Self::Gte, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a >= b, |a, b| a >= b)
            }
            (Self::Gte, Value::String(ref a), Value::String(ref b)) => boolean(a >= b),
            (Self::Gte, _, _) => Err(Error::InvalidType),

            (Self::Add, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a + b, |a, b| a + b)
            }
            (Self::Add, Value::String(ref s), Value::Number(ref t)) => add_date_number(s, t),
            (Self::Add, Value::Number(ref t), Value::String(ref s)) => add_number_date(t, s),
            (Self::Add, _, _) => Err(Error::InvalidType),

            (Self::Sub, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a - b, |a, b| a - b)
            }
            (Self::Sub, Value::String(ref s), Value::Number(ref t)) => subtract_date_number(s, t),
            (Self::Sub, Value::String(ref s), Value::String(ref t)) => subtract_date_date(s, t),
            (Self::Sub, _, _) => Err(Error::InvalidType),

            (Self::Mul, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a * b, |a, b| a * b)
            }
            (Self::Mul, _, _) => Err(Error::InvalidType),

            (Self::Div, Value::Number(ref a), Value::Number(ref b)) => number_res(
                a,
                b,
                |a, b| {
                    if b == 0.0 {
                        return Err(Error::DivideByZero);
                    }
                    Ok(a / b)
                },
                |a, b| {
                    if b == 0 {
                        return Err(Error::DivideByZero);
                    }
                    Ok(a / b)
                },
            ),
            (Self::Div, _, _) => Err(Error::InvalidType),

            (Self::Mod, Value::Number(ref a), Value::Number(ref b)) => {
                number(a, b, |a, b| a % b, |a, b| a % b)
            }
            (Self::Mod, _, _) => Err(Error::InvalidType),

            (Self::Concat, Value::String(ref a), Value::String(ref b)) => string(format!("{a}{b}")),
            (Self::Concat, _, _) => Err(Error::InvalidType),

            (Self::Contains, Value::Array(ref left), right) => {
                boolean(left.iter().any(|item| item == right))
            }
            (Self::Contains, _, _) => Err(Error::InvalidType),

            (Self::In, left, Value::Array(ref right)) => {
                boolean(right.iter().any(|item| item == left))
            }
            (Self::In, _, _) => Err(Error::InvalidType),
        }
    }
}

impl Expression {
    /// Run the expression to completion and return the result
    pub fn apply<'expr, 'val, T>(
        &'expr self,
        context: EvaluateContext<'_, 'val, T>,
    ) -> Result<Cow<'val, Value>, Error>
    where
        'expr: 'val,
    {
        self.apply_with_locals(context, &BTreeMap::default())
    }

    /// Run the expression to completion and return the result
    ///
    /// Locals can be used to augment or override the provided JSON values.
    pub fn apply_with_locals<'expr, 'val, T>(
        &'expr self,
        context: EvaluateContext<'_, 'val, T>,
        locals: &BTreeMap<&'expr str, Cow<'val, Value>>,
    ) -> Result<Cow<'val, Value>, Error>
    where
        'expr: 'val,
    {
        let mut context = context;
        self.apply_with_locals_inner(&mut context, locals)
    }

    fn apply_with_locals_inner<'expr, 'val, T>(
        &'expr self,
        context: &mut EvaluateContext<'_, 'val, T>,
        locals: &BTreeMap<&'expr str, Cow<'val, Value>>,
    ) -> Result<Cow<'val, Value>, Error>
    where
        'expr: 'val,
    {
        match self {
            Expression::Field(ref node) => {
                if let Some(local) = locals.get(node.name()) {
                    Ok(local.clone())
                } else {
                    Ok(context
                        .value()
                        .get(node.name())
                        .map(Cow::Borrowed)
                        .unwrap_or_else(|| Cow::Owned(Value::Null)))
                }
            }
            Expression::LastField(ref node) => {
                if let Some(previous_value) = context.previous_value() {
                    Ok(previous_value
                        .get(node.name())
                        .map(Cow::Borrowed)
                        .unwrap_or_else(|| Cow::Owned(Value::Null)))
                } else {
                    Err(Error::LastUsedWithoutPreviousValue)
                }
            }
            Expression::And(exprs) => {
                for expr in exprs.iter() {
                    let result = expr.apply_with_locals_inner(context, locals)?;
                    match result.as_ref() {
                        Value::Bool(false) => {
                            // Early return false
                            return Ok(Cow::Owned(Value::Bool(false)));
                        }
                        Value::Bool(true) => {
                            // Keep making sure all are true
                        }
                        _ => return Err(Error::InvalidType),
                    }
                }

                Ok(Cow::Owned(Value::Bool(true)))
            }
            Expression::Or(exprs) => {
                for expr in exprs.iter() {
                    let result = expr.apply_with_locals_inner(context, locals)?;
                    match result.as_ref() {
                        Value::Bool(true) => {
                            // Early return true
                            return Ok(Cow::Owned(Value::Bool(true)));
                        }
                        Value::Bool(false) => {
                            // Keep searching for a true
                        }
                        _ => return Err(Error::InvalidType),
                    }
                }

                Ok(Cow::Owned(Value::Bool(false)))
            }
            Expression::Iif(ref node) => {
                let value = node.test().apply_with_locals_inner(context, locals)?;
                match value.as_ref() {
                    Value::Bool(true) => node.t().apply_with_locals_inner(context, locals),
                    Value::Bool(false) => node.f().apply_with_locals_inner(context, locals),
                    _ => Err(Error::InvalidType),
                }
            }
            Expression::Op(node) => {
                let value1 = node.left().apply_with_locals_inner(context, locals)?;
                let value2 = node.right().apply_with_locals_inner(context, locals)?;
                node.op()
                    .apply(value1.as_ref(), value2.as_ref())
                    .map(Cow::Owned)
            }
            Expression::Literal(ref node) => Ok(Cow::Borrowed(node.value())),
            Expression::Not(ref node) => {
                let value = node.expression().apply_with_locals_inner(context, locals)?;
                match value.as_ref() {
                    Value::Bool(ref b) => Ok(Cow::Owned(Value::Bool(!b))),
                    _ => Err(Error::InvalidType),
                }
            }
            Expression::Function(ref node) => {
                let args = node
                    .iter()
                    .map(|expression| expression.apply_with_locals_inner(context, locals))
                    .collect::<Result<Vec<_>, _>>()?;

                let function = context
                    .engine()
                    .function(node.name())
                    .ok_or_else(|| Error::UnknownFunction(node.name().to_string()))?;
                function
                    .evaluate(context.function_context(), args)
                    .map_err(Error::Function)
            }
            Expression::List(ref node) => {
                let args = node
                    .iter()
                    .map(|expression| expression.apply_with_locals_inner(context, locals))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Cow::Owned(serde_json::json!(args)))
            }
        }
    }
}

/// A trait that represents a list of expressions that can be used when creating nodes like
/// [`AndNode`] or [`FunctionNode`].
pub trait Expressions {
    /// Create a list of expressions
    fn to_vec(self) -> Vec<Expression>;
}

impl Expressions for Vec<Expression> {
    fn to_vec(self) -> Vec<Expression> {
        self
    }
}
impl<const N: usize> Expressions for [Expression; N] {
    fn to_vec(self) -> Vec<Expression> {
        self.into_iter().collect()
    }
}

/// An error that occured while evaluating an expression
#[derive(Debug)]
pub enum Error {
    /// `LAST FieldName` was used but no previous value was provided
    LastUsedWithoutPreviousValue,
    /// The expression expected a certain type, but the value was not the expected type
    InvalidType,
    /// Some aspect of evaluation is not implemented
    NotImplemented,
    /// The expression did not result in a number that can be used
    InvalidNumber,
    /// The expression attempted to divide by zero
    DivideByZero,
    /// A function call was used to a function that does not exist
    UnknownFunction(String),
    /// A function call failed
    Function(function::FunctionError),
}

/// Visit nodes in the Expression
///
/// Use this to transform expressions or to inspect the structure of expressions without recursively
/// evaluating them.
///
/// ## Example
///
/// ```
/// use rets_expression::{Expression, Visitor, FunctionNode, LiteralNode};
/// let mut expression = "5 .IN. (1, 2, Three, LAST Four, Five)"
///     .parse::<Expression>()
///     .unwrap();
///
/// /// A visitor that rewrites native lists into `LIST` function calls.
/// struct RewriteNativeListVisitor;
///
/// impl Visitor for RewriteNativeListVisitor {
///     fn visit_list_expression_in(&mut self, expression: &mut Expression) {
///         let list_expression = std::mem::replace(
///             expression,
///             Expression::Literal(LiteralNode::new(serde_json::Value::Null)),
///         );
///         let Expression::List(list_node) = list_expression else {
///             unreachable!()
///         };
///         let function_node = FunctionNode::new("LIST", list_node.items);
///         let function_expression = Expression::from(function_node);
///         *expression = function_expression;
///     }
/// }
///
/// expression.accept(&mut RewriteNativeListVisitor);
///
/// let serialized = expression.serialize().unwrap();
/// assert_eq!(serialized, "5 .IN. LIST(1, 2, Three, LAST Four, Five)");
/// ```
#[allow(unused_variables)]
pub trait Visitor {
    /// Called when the visitor visits an expression before calling any of the other visit methods
    fn visit_expression_in(&mut self, expression: &mut Expression) {}
    /// Called then the visitor visits an expression after calling all of the other visit methods
    fn visit_expression_out(&mut self, expression: &mut Expression) {}

    /// Called when the visitor visits a [`FieldNode`]
    fn visit_field_node(&mut self, node: &mut FieldNode) {}
    /// Called when the visitor visits a [`LastFieldNode`]
    fn visit_last_field_node(&mut self, node: &mut LastFieldNode) {}
    /// Called when the visitor visits an [`AndNode`] before visiting its children
    fn visit_and_node_in(&mut self, node: &mut AndNode) {}
    /// Called when the visitor visits an [`AndNode`] after visiting its children
    fn visit_and_node_out(&mut self, node: &mut AndNode) {}
    /// Called when the visitor visits an [`OrNode`] before visiting its children
    fn visit_or_node_in(&mut self, node: &mut OrNode) {}
    /// Called when the visitor visits an [`OrNode`] after visiting its children
    fn visit_or_node_out(&mut self, node: &mut OrNode) {}
    /// Called when the visitor visits a [`NotNode`] before visiting its child
    fn visit_not_node_in(&mut self, node: &mut NotNode) {}
    /// Called when the visitor visits a [`NotNode`] after visiting its child
    fn visit_not_node_out(&mut self, node: &mut NotNode) {}
    /// Called when the visitor visits an [`OpNode`] before visiting its children
    fn visit_op_node_in(&mut self, node: &mut OpNode) {}
    /// Called when the visitor visits an [`OpNode`] after visiting its children
    fn visit_op_node_out(&mut self, node: &mut OpNode) {}
    /// Called when the visitor visits a [`LiteralNode`]
    fn visit_literal_node(&mut self, node: &mut LiteralNode) {}
    /// Called when the visitor visits a [`FunctionNode`] before visiting its children
    fn visit_function_node_in(&mut self, node: &mut FunctionNode) {}
    /// Called when the visitor visits a [`FunctionNode`] after visiting its children
    fn visit_function_node_out(&mut self, node: &mut FunctionNode) {}
    /// Called when the visitor visits a [`IifNode`] before visiting its children
    fn visit_iif_node_in(&mut self, node: &mut IifNode) {}
    /// Called when the visitor visits a [`IifNode`] after visiting its children
    fn visit_iif_node_out(&mut self, node: &mut IifNode) {}
    /// Called when the visitor visits a [`ListNode`] before visiting its children
    fn visit_list_node_in(&mut self, node: &mut ListNode) {}
    /// Called when the visitor visits a [`ListNode`] after visiting its children
    fn visit_list_node_out(&mut self, node: &mut ListNode) {}

    /// Called when the visitor visits an expression representing a [`FieldNode`]
    fn visit_field_expression(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`LastFieldNode`]
    fn visit_last_field_expression(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`AndNode`] before visiting its children
    fn visit_and_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`AndNode`] after visiting its children
    fn visit_and_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`OrNode`] before visiting its children
    fn visit_or_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`OrNode`] after visiting its children
    fn visit_or_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`NotNode`] before visiting its child
    fn visit_not_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`NotNode`] after visiting its child
    fn visit_not_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`OpNode`] before visiting its children
    fn visit_op_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits ann expression representing a [`OpNode`] after visiting its children
    fn visit_op_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`LiteralNode`]
    fn visit_literal_expression(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`FunctionNode`] before visiting its children
    fn visit_function_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`FunctionNode`] after visiting its children
    fn visit_function_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`IifNode`] before visiting its children
    fn visit_iif_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`IifNode`] after visiting its children
    fn visit_iif_expression_out(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`ListNode`] before visiting its children
    fn visit_list_expression_in(&mut self, expression: &mut Expression) {}
    /// Called when the visitor visits an expression representing a [`ListNode`] after visiting its children
    fn visit_list_expression_out(&mut self, expression: &mut Expression) {}
}

#[cfg(all(feature = "std", test))]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_basic() {
        let value = json!({
            "X": 1700,
            "Y": 400,
            "Z": 1700,
        });
        let previous_value = json!({
            "X": 1700,
            "Y": 300,
        });

        let expression = "X = LAST X .AND. Y = LAST Y .OR. X = Z"
            .parse::<Expression>()
            .unwrap();

        let engine = Engine::default();

        let context = EvaluateContext::new(&engine, &value).with_previous(&previous_value);
        let value = expression.apply(context).unwrap();
        assert_eq!(value.into_owned(), json!(true));
    }

    #[test]
    fn test_visitor() {
        let mut expression = "5 .IN. (1, 2, Three, LAST Four, Five)"
            .parse::<Expression>()
            .unwrap();

        struct RewriteNativeListVisitor;

        impl Visitor for RewriteNativeListVisitor {
            fn visit_list_expression_in(&mut self, expression: &mut Expression) {
                let list_expression = std::mem::replace(
                    expression,
                    Expression::Literal(LiteralNode::new(serde_json::Value::Null)),
                );
                let Expression::List(list_node) = list_expression else {
                    unreachable!()
                };
                let function_node = FunctionNode::new("LIST", list_node.items);
                let function_expression = Expression::from(function_node);
                *expression = function_expression;
            }
        }

        expression.accept(&mut RewriteNativeListVisitor);

        let serialized = expression.serialize().unwrap();
        assert_eq!(serialized, "5 .IN. LIST(1, 2, Three, LAST Four, Five)");
    }
}
