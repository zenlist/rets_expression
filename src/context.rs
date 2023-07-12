use super::function;
use alloc::{
    boxed::Box,
    collections::BTreeMap,
    string::{String, ToString},
};
use serde_json::Value;

/// The information required to evaluate an expression
#[derive(Copy, Clone)]
pub struct EvaluateContext<'engine, 'json, T> {
    engine: &'engine Engine<T>,
    value: &'json Value,
    previous_value: Option<&'json Value>,
    state: T,
}

impl<'engine, 'json> EvaluateContext<'engine, 'json, ()> {
    /// Create a new evaluation context with the provided value
    pub fn new(engine: &'engine Engine<()>, value: &'json Value) -> Self {
        EvaluateContext {
            engine,
            value,
            previous_value: None,
            state: (),
        }
    }
}

impl<'engine, 'json, T> EvaluateContext<'engine, 'json, T> {
    /// Create a new evaluation context with the provided value and data
    pub fn new_with_state(engine: &'engine Engine<T>, value: &'json Value, state: T) -> Self {
        EvaluateContext {
            value,
            previous_value: None,
            engine,
            state,
        }
    }

    /// Add the previous data to use when evaluating
    ///
    /// Previous data is what gets used when an expression uses `LAST FieldName`.
    pub fn with_previous(self, previous_value: &'json Value) -> Self {
        self.set_previous(Some(previous_value))
    }

    /// Set the previous data to use when evaluating
    ///
    /// Previous data is what gets used when an expression uses `LAST FieldName`.
    pub fn set_previous(mut self, previous_value: Option<&'json Value>) -> Self {
        self.previous_value = previous_value;
        self
    }

    /// Get a reference to the value within this context
    pub fn value(&self) -> &'json Value {
        self.value
    }

    /// Get a reference to the previous value within this context, if available
    pub fn previous_value(&self) -> Option<&'json Value> {
        self.previous_value
    }

    /// Get a reference to the engine used when creating this context
    pub fn engine(&self) -> &'engine Engine<T> {
        self.engine
    }

    /// Get a reference to the state used when creating this context
    pub fn state(&self) -> &T {
        &self.state
    }

    /// Get a mutable reference to the state used when creating this context
    pub fn state_mut(&mut self) -> &mut T {
        &mut self.state
    }

    pub(crate) fn function_context(&mut self) -> function::FunctionContext<'_, T> {
        function::FunctionContext::new(&mut self.state)
    }
}

/// Global context for how to evaluate an expression
///
/// Mostly this holds all of the functions that can be called during evaluation.
///
/// This can typically be created once and used any time an expression evaluation needs to happen.
pub struct Engine<T> {
    functions: BTreeMap<String, Box<dyn function::Function<T>>>,
}

impl<T> Engine<T> {
    /// Create a new engine that does not contain any defined functions
    pub fn empty() -> Self {
        Self {
            functions: BTreeMap::default(),
        }
    }

    /// Get a function by name
    pub fn function(&self, name: &str) -> Option<&dyn function::Function<T>> {
        self.functions.get(name).map(|bx| bx.as_ref())
    }

    /// Add or override a function with the given name
    ///
    /// ```
    /// use rets_expression::{Engine, function::{ListFunction, SetFunction}};
    ///
    /// let mut engine = Engine::empty();
    /// # let mut engine: Engine<()> = engine; // Provide a type
    /// engine.set_function("LIST", Box::new(ListFunction));
    /// engine.set_function("SET", Box::new(SetFunction));
    /// ```
    pub fn set_function(&mut self, name: impl ToString, function: Box<dyn function::Function<T>>) {
        self.functions.insert(name.to_string(), function);
    }

    /// Add or override a function with the given name
    ///
    /// ```
    /// use rets_expression::{Engine, function::{ListFunction, SetFunction}};
    ///
    /// let engine = Engine::empty()
    ///     .with_function("LIST", Box::new(ListFunction))
    ///     .with_function("SET", Box::new(SetFunction));
    /// # let mut engine: Engine<()> = engine; // Provide a type
    /// ```
    pub fn with_function(
        mut self,
        name: impl ToString,
        function: Box<dyn function::Function<T>>,
    ) -> Self {
        self.set_function(name, function);
        self
    }
}

impl<T> Default for Engine<T> {
    fn default() -> Self {
        let mut engine = Engine::empty();
        engine.set_function("SET", Box::new(function::SetFunction));
        engine.set_function("UNION", Box::new(function::UnionFunction));
        engine.set_function("DIFFERENCE", Box::new(function::DifferenceFunction));
        engine.set_function("INTERSECTION", Box::new(function::IntersectionFunction));
        engine.set_function("LENGTH", Box::new(function::LengthFunction));
        engine.set_function("LIST", Box::new(function::ListFunction));

        engine.set_function("BOOL", Box::new(function::BoolFunction));
        engine.set_function("CHAR", Box::new(function::CharFunction));
        engine.set_function("CHARF", Box::new(function::CharfFunction));
        engine.set_function("TIME", Box::new(function::TimeFunction));
        engine.set_function("DATE", Box::new(function::DateFunction));
        engine.set_function("INT", Box::new(function::IntFunction));
        engine.set_function("FLOAT", Box::new(function::FloatFunction));
        engine.set_function("SUBSTR", Box::new(function::SubstrFunction));
        engine.set_function("STRLEN", Box::new(function::StrlenFunction));
        engine.set_function("LOWER", Box::new(function::LowerFunction));
        engine.set_function("UPPER", Box::new(function::UpperFunction));
        engine.set_function("YEAR", Box::new(function::YearFunction));
        engine.set_function("MONTH", Box::new(function::MonthFunction));
        engine.set_function("DAY", Box::new(function::DayFunction));
        engine.set_function("WEEKDAY", Box::new(function::WeekdayFunction));
        engine.set_function("TYPEOF", Box::new(function::TypeofFunction));

        #[cfg(feature = "match_function")]
        engine.set_function("MATCH", Box::new(function::MatchFunction));
        #[cfg(feature = "std")]
        engine.set_function("NOW", Box::new(function::NowFunction));
        #[cfg(feature = "std")]
        engine.set_function("TODAY", Box::new(function::TodayFunction));

        engine
    }
}

impl<T> core::fmt::Debug for Engine<T>
where
    T: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        struct FunctionList<'a, T>(&'a BTreeMap<String, Box<dyn function::Function<T>>>);

        impl<'a, T> core::fmt::Debug for FunctionList<'a, T> {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let mut list = f.debug_list();
                for name in self.0.keys() {
                    list.entry(name);
                }
                list.finish()
            }
        }

        f.debug_struct("Engine")
            .field("functions", &FunctionList(&self.functions))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use alloc::borrow::Cow;
    use serde_json::json;

    #[test]
    fn test_with_state() {
        struct Inc;
        impl function::Function<u32> for Inc {
            fn evaluate<'json>(
                &self,
                mut context: function::FunctionContext<'_, u32>,
                _input: Vec<Cow<'json, serde_json::Value>>,
            ) -> Result<Cow<'json, serde_json::Value>, function::FunctionError> {
                let value = *context.state();
                *context.state_mut() += 1;
                Ok(Cow::Owned(serde_json::Value::Number(
                    serde_json::Number::from(value),
                )))
            }
        }

        let engine = Engine::empty().with_function("INC", Box::new(Inc));
        let expression = "INC() + INC() + INC() + INC()"
            .parse::<Expression>()
            .unwrap();
        let value = json!({});

        let context = EvaluateContext::new_with_state(&engine, &value, 1);
        let value = expression.apply(context).unwrap().into_owned();
        assert_eq!(value.as_u64().unwrap(), 1 + 2 + 3 + 4);

        let context = EvaluateContext::new_with_state(&engine, &value, 10);
        let value = expression.apply(context).unwrap().into_owned();
        assert_eq!(value.as_u64().unwrap(), 10 + 11 + 12 + 13);
    }
}
