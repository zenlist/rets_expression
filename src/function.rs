//! Functions that can be used during the evaluation of an [`Expression`][super::Expression].
//!
//! An [`Engine`][super::Engine] holds all of the functions that can be executed by an `Expression`.
//! `Engine::default()` intializes all of the functions from the standard.
//!
//! If, however, you need to add new functions or change how existing functions work, that is also
//! possible.
//!
//! ## Defining new functions
//!
//! Definining a new function involves creating a struct that implements [`Function`] and then
//! adding it to an engine.
//!
//! ```
//! use std::borrow::Cow;
//! use serde_json::{json, Value};
//! use rets_expression::{Engine, Expression, EvaluateContext};
//! use rets_expression::function::{Function, FunctionError, FunctionContext};
//!
//! /// A function that takes the square root of a value.
//! struct Sqrt;
//!
//! impl<T> Function<T> for Sqrt {
//!     fn evaluate<'json>(
//!         &self,
//!         _context: FunctionContext<'_, T>,
//!         input: Vec<Cow<'json, Value>>,
//!     ) -> Result<Cow<'json, Value>, FunctionError> {
//!         if input.len() != 1 {
//!             return Err(FunctionError::UnexpectedNumberOfArguments);
//!         }
//!
//!         let Some(value) = input.get(0).and_then(|value| value.as_f64()) else {
//!             return Err(FunctionError::InvalidType);
//!         };
//!
//!         let sqrt = value.sqrt();
//!
//!         Ok(Cow::Owned(json!(sqrt)))
//!     }
//! }
//!
//! // Add the function to the engine
//! let mut engine = Engine::default();
//! engine.set_function("SQRT", Box::new(Sqrt));
//!
//! // Parse an expression that uses the function
//! let expression = "SQRT(9.0)".parse::<Expression>().unwrap();
//!
//! // And execute it
//! let value = json!({});
//! let context = EvaluateContext::new(&engine, &value);
//! let result = expression.apply(context).unwrap();
//!
//! assert_eq!(result.into_owned(), json!(3.0));
//! ```
//!
//! ## Defining new functions that use state
//!
//! In some cases, you may need to store state between expression evaluations, or provide state
//! between function calls.
//!
//! For these cases, [`EvaluateContext::new_with_state`][super::EvaluateContext::new_with_state] can
//! be used to seed the [`EvaluateContext`][super::EvaluateContext] with state, and
//! [`FunctionContext::state`]/[`FunctionContext::state_mut`] can be used to access state from
//! within a function.
//!
//! ```
//! use std::borrow::Cow;
//! use serde_json::{json, Value};
//! use rets_expression::{Engine, Expression, EvaluateContext};
//! use rets_expression::function::{Function, FunctionError, FunctionContext};
//!
//! /// The engine state used for the `TODAY` function call.
//! struct MyState {
//!     today: String,
//! }
//!
//! /// A new implementation of the `TODAY` function.
//! struct OverriddenToday;
//!
//! impl Function<MyState> for OverriddenToday {
//!     fn evaluate<'json>(
//!         &self,
//!         context: FunctionContext<'_, MyState>,
//!         input: Vec<Cow<'json, Value>>,
//!     ) -> Result<Cow<'json, Value>, FunctionError> {
//!         let state = context.state();
//!         let today = state.today.clone();
//!
//!         Ok(Cow::Owned(json!(today)))
//!     }
//! }
//!
//! // Add the function to the engine
//! let mut engine = Engine::default();
//! engine.set_function("TODAY", Box::new(OverriddenToday));
//!
//! // Parse an expression that uses the function
//! let expression = ".TODAY.".parse::<Expression>().unwrap();
//!
//! let value = json!({});
//!
//! // And execute it
//! let state = MyState { today: String::from("2023-04-21") };
//! let context = EvaluateContext::new_with_state(&engine, &value, state);
//! let result = expression.apply(context).unwrap();
//!
//! assert_eq!(result.into_owned(), json!("2023-04-21"));
//!
//! // And execute it with different state
//! let state = MyState { today: String::from("2023-07-31") };
//! let context = EvaluateContext::new_with_state(&engine, &value, state);
//! let result = expression.apply(context).unwrap();
//!
//! assert_eq!(result.into_owned(), json!("2023-07-31"));
//! ```

use alloc::{
    borrow::Cow,
    format,
    string::{String, ToString},
    vec::Vec,
};
use chrono::{DateTime, FixedOffset, NaiveDate};
use serde_json::Value;

/// The trait that defines the evaluation of a function
pub trait Function<T> {
    /// Evaluate the function with the provided context and input
    fn evaluate<'json>(
        &self,
        context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError>;
}

impl<F, T> Function<T> for F
where
    for<'json> F: Fn(Vec<Cow<'json, Value>>) -> Result<Cow<'json, Value>, FunctionError>,
{
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        self(input)
    }
}

/// The context that a function is run in
///
/// This provides access to the data stored in the `EvaluationContext` from within a function.
pub struct FunctionContext<'t, T> {
    state: &'t mut T,
}

impl<'t, T> FunctionContext<'t, T> {
    /// Create a new function context
    pub(crate) fn new(state: &'t mut T) -> Self {
        Self { state }
    }

    /// Get the state stored in the `EvaluationContext`
    pub fn state(&self) -> &T {
        self.state
    }

    /// Mutate the state stored in the `EvaluationContext`
    pub fn state_mut(&mut self) -> &mut T {
        self.state
    }
}

/// An error that can be thrown by a function
#[derive(Debug)]
pub enum FunctionError {
    /// One of the arguments to the function was not the type that was expected
    InvalidType,
    /// The function did not receive the number of arguments it expected
    UnexpectedNumberOfArguments,
    /// The function threw its own custom error
    Custom(Cow<'static, str>),
}

impl core::fmt::Display for FunctionError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            FunctionError::InvalidType => f.write_str("Invalid type"),
            FunctionError::UnexpectedNumberOfArguments => {
                f.write_str("Unexpected number of arguments")
            }
            FunctionError::Custom(err) => write!(f, "{err}"),
        }
    }
}

impl core::error::Error for FunctionError {}

/// Default implementation for the standard `LIST` function
pub struct ListFunction;

impl<T> Function<T> for ListFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        let owned_args = args.into_iter().map(|arg| arg.into_owned()).collect();
        let owned_array = serde_json::Value::Array(owned_args);
        Ok(Cow::Owned(owned_array))
    }
}

/// Default implementation for the standard `SET` function
pub struct SetFunction;

impl<T> Function<T> for SetFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        let owned_args: Vec<_> = args.into_iter().map(|arg| arg.into_owned()).collect();
        let mut new_set = Vec::with_capacity(owned_args.len());
        for arg in owned_args {
            if !new_set.contains(&arg) {
                new_set.push(arg);
            }
        }
        let owned_array = serde_json::Value::Array(new_set);
        Ok(Cow::Owned(owned_array))
    }
}

/// Default implementation for the standard `UNION` function
pub struct UnionFunction;

impl<T> Function<T> for UnionFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        mut args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        if args.len() == 1 && args[0].is_array() {
            return Ok(args.pop().unwrap());
        }

        let mut new_values: Vec<Value> = Vec::new();
        for arg in args {
            let Value::Array(array) = arg.into_owned() else {
                return Err(FunctionError::InvalidType);
            };
            for item in array {
                if new_values.iter().all(|existing| existing != &item) {
                    new_values.push(item);
                }
            }
        }

        let owned_array = serde_json::Value::Array(new_values);
        Ok(Cow::Owned(owned_array))
    }
}

/// Default implementation for the standard `DIFFERENCE` function
pub struct DifferenceFunction;

impl<T> Function<T> for DifferenceFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        mut args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        if args.len() != 2 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }
        let right = args.pop().unwrap();
        let left = args.pop().unwrap();

        let Value::Array(left) = left.into_owned() else {
            return Err(FunctionError::InvalidType);
        };
        let Value::Array(right) = right.into_owned() else {
            return Err(FunctionError::InvalidType);
        };

        let mut new_values = Vec::new();

        for item in &left {
            if !right.contains(item) {
                new_values.push(item.clone());
            }
        }
        for item in &right {
            if !left.contains(item) {
                new_values.push(item.clone());
            }
        }

        let owned_array = serde_json::Value::Array(new_values);
        Ok(Cow::Owned(owned_array))
    }
}

/// Default implementation for the standard `INTERSECTION` function
pub struct IntersectionFunction;

impl<T> Function<T> for IntersectionFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        mut args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        if args.is_empty() {
            return Ok(Cow::Owned(Value::Array(Vec::new())));
        }
        if args.len() == 1 && args[0].is_array() {
            return Ok(args.pop().unwrap());
        }

        fn intersect_two(left: Vec<Value>, right: Vec<Value>) -> Vec<Value> {
            let mut intersection = Vec::new();
            for item in right {
                if left.contains(&item) {
                    intersection.push(item);
                }
            }
            intersection
        }

        let mut intersection = None;
        for arg in args {
            let Value::Array(array) = arg.into_owned() else {
                return Err(FunctionError::InvalidType);
            };
            if let Some(previous) = intersection {
                intersection = Some(intersect_two(previous, array));
            } else {
                intersection = Some(array);
            }
        }

        let owned_array = serde_json::Value::Array(intersection.unwrap());
        Ok(Cow::Owned(owned_array))
    }
}

/// Default implementation for the standard `LENGTH` function
pub struct LengthFunction;

impl<T> Function<T> for LengthFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        if args.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }
        if !args[0].is_array() {
            return Err(FunctionError::InvalidType);
        }

        let len = args[0].as_array().unwrap().len();
        Ok(Cow::Owned(serde_json::Value::Number(
            serde_json::Number::from(len),
        )))
    }
}

#[cfg(feature = "match_function")]
/// Default implementation for the standard `MATCH` function
pub struct MatchFunction;

#[cfg(feature = "match_function")]
impl<T> Function<T> for MatchFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        if args.len() != 2 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }
        let Some(regex) = args[1].as_str() else {
            return Err(FunctionError::InvalidType);
        };
        if args[0].is_null() {
            return Ok(Cow::Owned(serde_json::Value::Bool(false)));
        }
        let Some(text) = args[0].as_str() else {
            return Err(FunctionError::InvalidType);
        };

        let regex = regex_lite::Regex::new(regex).map_err(|_| FunctionError::InvalidType)?;
        let result = regex.is_match(text);

        Ok(Cow::Owned(serde_json::Value::Bool(result)))
    }
}

#[cfg(feature = "std")]
/// Default implementation for the standard `NOW` function
pub struct NowFunction;

#[cfg(feature = "std")]
impl<T> Function<T> for NowFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        _args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        let now = chrono::Utc::now();
        let str = now.to_rfc3339_opts(chrono::SecondsFormat::Millis, true);
        Ok(Cow::Owned(serde_json::Value::String(str)))
    }
}

#[cfg(feature = "std")]
/// Default implementation for the standard `TODAY` function
///
/// Note that this function does not handle timezones. If you need a `TODAY` function that handles
/// timezones, you may want to implement your own.
pub struct TodayFunction;

#[cfg(feature = "std")]
impl<T> Function<T> for TodayFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        _args: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, FunctionError> {
        let now = chrono::Utc::now();
        let str = now.format("%Y-%m-%d").to_string();
        Ok(Cow::Owned(serde_json::Value::String(str)))
    }
}

/// Default implementation for the standard `BOOL` function
pub struct BoolFunction;

impl<T> Function<T> for BoolFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(_) => Ok(input),
            Value::Number(n) if n.as_i64() == Some(0) => Ok(Cow::Owned(Value::Bool(false))),
            Value::Number(_) => Ok(Cow::Owned(Value::Bool(true))),
            Value::String(str) => match str.to_ascii_lowercase().as_str() {
                "1" | "yes" | "true" => Ok(Cow::Owned(Value::Bool(true))),
                _ => Ok(Cow::Owned(Value::Bool(false))),
            },
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}

/// Default implementation for the standard `CHAR` function
pub struct CharFunction;

impl<T> Function<T> for CharFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(false) => Ok(Cow::Owned(Value::String(String::from("0")))),
            Value::Bool(true) => Ok(Cow::Owned(Value::String(String::from("1")))),
            Value::Number(n) => Ok(Cow::Owned(Value::String(n.to_string()))),
            Value::String(_) => Ok(input),
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}

/// Default implementation for the standard `CHARF` function
pub struct CharfFunction;

impl<T> Function<T> for CharfFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 2 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let mut iter = input.into_iter();
        let value = iter.next().unwrap();
        let precision = iter.next().unwrap();

        let Some(precision) = precision.as_u64() else {
            return Err(FunctionError::InvalidType);
        };
        let precision = precision as usize;

        match value.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(_) => Err(FunctionError::InvalidType),
            Value::Number(n) => {
                let f64 = if let Some(u64) = n.as_u64() {
                    u64 as f64
                } else if let Some(i64) = n.as_i64() {
                    i64 as f64
                } else if let Some(f64) = n.as_f64() {
                    f64
                } else {
                    return Err(FunctionError::InvalidType);
                };
                let str = format!("{f64:.precision$}");
                Ok(Cow::Owned(Value::String(str)))
            }
            Value::String(_) => Err(FunctionError::InvalidType),
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}

/// Default implementation for the standard `TIME` function
pub struct TimeFunction;

impl<T> Function<T> for TimeFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(_) => Err(FunctionError::InvalidType),
            Value::Number(_) => Err(FunctionError::InvalidType),
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
            Value::String(s) => {
                #[allow(clippy::if_same_then_else)]
                if NaiveDate::parse_from_str(s, "%Y-%m-%d").is_ok() {
                    // Valid date
                    Ok(input)
                } else if DateTime::parse_from_rfc3339(s).is_ok() {
                    // Valid time
                    Ok(input)
                } else {
                    Err(FunctionError::InvalidType)
                }
            }
        }
    }
}

/// Default implementation for the standard `DATE` function
pub struct DateFunction;

impl<T> Function<T> for DateFunction {
    fn evaluate<'json>(
        &self,
        context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        // DATE and TIME are synonyms and handle both
        TimeFunction.evaluate(context, input)
    }
}

/// Default implementation for the standard `INT` function
pub struct IntFunction;

impl<T> Function<T> for IntFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(false) => Ok(Cow::Owned(serde_json::Value::Number(
                serde_json::Number::from(0),
            ))),
            Value::Bool(true) => Ok(Cow::Owned(serde_json::Value::Number(
                serde_json::Number::from(1),
            ))),
            Value::Number(n) if n.is_i64() || n.is_u64() => Ok(input),
            Value::Number(n) => {
                if let Some(f64) = n.as_f64() {
                    let value = f64 as i64;
                    Ok(Cow::Owned(serde_json::Value::Number(
                        serde_json::Number::from(value),
                    )))
                } else {
                    Err(FunctionError::UnexpectedNumberOfArguments)
                }
            }
            Value::String(s) => {
                if let Ok(i64) = s.parse::<i64>() {
                    Ok(Cow::Owned(serde_json::Value::Number(
                        serde_json::Number::from(i64),
                    )))
                } else if let Ok(f64) = s.parse::<f64>() {
                    Ok(Cow::Owned(serde_json::Value::Number(
                        serde_json::Number::from(f64 as i64),
                    )))
                } else {
                    Err(FunctionError::InvalidType)
                }
            }
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}

/// Default implementation for the standard `FLOAT` function
pub struct FloatFunction;

impl<T> Function<T> for FloatFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(false) => Ok(Cow::Owned(serde_json::Value::Number(
                serde_json::Number::from(0),
            ))),
            Value::Bool(true) => Ok(Cow::Owned(serde_json::Value::Number(
                serde_json::Number::from(1),
            ))),
            Value::Number(_) => Ok(input),
            Value::String(s) => {
                if let Some(number) = s.parse::<f64>().ok().and_then(serde_json::Number::from_f64) {
                    Ok(Cow::Owned(serde_json::Value::Number(number)))
                } else {
                    Err(FunctionError::InvalidType)
                }
            }
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}

/// Default implementation for the standard `SUBSTR` function
pub struct SubstrFunction;

impl<T> Function<T> for SubstrFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 3 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let mut iter = input.into_iter();
        let value = iter.next().unwrap();
        let start = iter.next().unwrap();
        let end = iter.next().unwrap();

        let Some(str) = value.as_str() else {
            return Err(FunctionError::InvalidType);
        };

        let Some(start) = start.as_u64() else {
            return Err(FunctionError::InvalidType);
        };
        let start = start as usize;

        let Some(end) = end.as_u64() else {
            return Err(FunctionError::InvalidType);
        };
        let end = end as usize;

        // RETS is 1-based; Rust is 0-based.
        if start == 0 || end == 0 {
            return Err(FunctionError::InvalidType);
        }
        let start = start - 1;
        let end = end - 1;

        let start = core::cmp::min(start, str.len());
        let end = core::cmp::min(end, str.len());
        let str = &str[start..end];
        Ok(Cow::Owned(Value::String(str.to_string())))
    }
}

/// Default implementation for the standard `STRLEN` function
pub struct StrlenFunction;

impl<T> Function<T> for StrlenFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        let Some(str) = input.as_str() else {
            return Err(FunctionError::InvalidType);
        };

        Ok(Cow::Owned(Value::Number(serde_json::Number::from(
            str.len(),
        ))))
    }
}

/// Default implementation for the standard `LOWER` function
pub struct LowerFunction;

impl<T> Function<T> for LowerFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        let Some(str) = input.as_str() else {
            return Err(FunctionError::InvalidType);
        };

        Ok(Cow::Owned(Value::String(str.to_ascii_lowercase())))
    }
}

/// Default implementation for the standard `UPPER` function
pub struct UpperFunction;

impl<T> Function<T> for UpperFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        let Some(str) = input.as_str() else {
            return Err(FunctionError::InvalidType);
        };

        Ok(Cow::Owned(Value::String(str.to_ascii_uppercase())))
    }
}

/// Default implementation for the standard `YEAR` function
pub struct YearFunction;

impl<T> Function<T> for YearFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        date_time_helper(
            input,
            |date| chrono::Datelike::year(&date).into(),
            |time| chrono::Datelike::year(&time).into(),
        )
    }
}

/// Default implementation for the standard `MONTH` function
pub struct MonthFunction;

impl<T> Function<T> for MonthFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        date_time_helper(
            input,
            |date| chrono::Datelike::month(&date).into(),
            |time| chrono::Datelike::month(&time).into(),
        )
    }
}

/// Default implementation for the standard `DAY` function
pub struct DayFunction;

impl<T> Function<T> for DayFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        date_time_helper(
            input,
            |date| chrono::Datelike::day(&date).into(),
            |time| chrono::Datelike::day(&time).into(),
        )
    }
}

/// Default implementation for the standard `WEEKDAY` function
pub struct WeekdayFunction;

impl<T> Function<T> for WeekdayFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        fn weekday_to_number(weekday: chrono::Weekday) -> i64 {
            match weekday {
                chrono::Weekday::Sun => 1,
                chrono::Weekday::Mon => 2,
                chrono::Weekday::Tue => 3,
                chrono::Weekday::Wed => 4,
                chrono::Weekday::Thu => 5,
                chrono::Weekday::Fri => 6,
                chrono::Weekday::Sat => 7,
            }
        }
        date_time_helper(
            input,
            |date| weekday_to_number(chrono::Datelike::weekday(&date)),
            |time| weekday_to_number(chrono::Datelike::weekday(&time)),
        )
    }
}

fn date_time_helper<DateF, TimeF>(
    input: Vec<Cow<'_, Value>>,
    date_f: DateF,
    time_f: TimeF,
) -> Result<Cow<'_, Value>, FunctionError>
where
    DateF: Fn(NaiveDate) -> i64,
    TimeF: Fn(DateTime<FixedOffset>) -> i64,
{
    if input.len() != 1 {
        return Err(FunctionError::UnexpectedNumberOfArguments);
    }

    let input = input.into_iter().next().unwrap();
    let Some(str) = input.as_str() else {
        return Err(FunctionError::InvalidType);
    };

    let number = if let Ok(date) = NaiveDate::parse_from_str(str, "%Y-%m-%d") {
        date_f(date)
    } else if let Ok(time) = DateTime::parse_from_rfc3339(str) {
        time_f(time)
    } else {
        return Err(FunctionError::InvalidType);
    };

    Ok(Cow::Owned(Value::Number(serde_json::Number::from(number))))
}

/// Default implementation for the standard `TYPEOF` function
pub struct TypeofFunction;

impl<T> Function<T> for TypeofFunction {
    fn evaluate<'json>(
        &self,
        _context: FunctionContext<'_, T>,
        input: Vec<Cow<'json, Value>>,
    ) -> Result<Cow<'json, Value>, FunctionError> {
        if input.len() != 1 {
            return Err(FunctionError::UnexpectedNumberOfArguments);
        }

        let input = input.into_iter().next().unwrap();
        match input.as_ref() {
            Value::Null => Err(FunctionError::InvalidType),
            Value::Bool(_) => Ok(Cow::Owned(Value::String(String::from("BOOLEAN")))),
            Value::Number(n) if n.is_f64() => Ok(Cow::Owned(Value::String(String::from("FLOAT")))),
            Value::Number(_) => Ok(Cow::Owned(Value::String(String::from("INT")))),
            Value::String(s) if NaiveDate::parse_from_str(s, "%Y-%m-%d").is_ok() => {
                Ok(Cow::Owned(Value::String(String::from("TIME"))))
            }
            Value::String(s) if DateTime::parse_from_rfc3339(s).is_ok() => {
                Ok(Cow::Owned(Value::String(String::from("TIME"))))
            }
            Value::String(_) => Ok(Cow::Owned(Value::String(String::from("CHAR")))),
            Value::Array(_) => Err(FunctionError::InvalidType),
            Value::Object(_) => Err(FunctionError::InvalidType),
        }
    }
}
