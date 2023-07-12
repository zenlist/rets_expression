use std::borrow::Cow;

use chrono::{DateTime, Utc};
use rets_expression::{Engine, EvaluateContext, Expression};
use serde_json::json;

fn main() {
    let mut tests_found = 0;
    let mut tests_passed = 0;

    let engine = Engine::default()
        .with_function("NOW", Box::new(Now))
        .with_function("TODAY", Box::new(Today));

    let iter = std::fs::read_dir("compliance-tests").unwrap();
    for result in iter {
        let file = result.unwrap();
        let filename = file
            .file_name()
            .into_string()
            .expect("Expected utf8 filename");

        if !filename.ends_with(".json") {
            continue;
        }

        let contents = std::fs::read(file.path()).unwrap();
        let expression_tests: Vec<ExpressionTest> = serde_json::from_slice(&contents).unwrap();

        for test in expression_tests {
            for check in &test.checks {
                tests_found += 1;
                let name = format!(
                    "tests/expression-tests/{} :: {} :: {}",
                    filename, test.name, check.expression
                );
                let expression = match check.expression.parse::<Expression>() {
                    Ok(expression) => expression,
                    Err(err) => {
                        println!("NOK {name}");
                        println!(
                            "  expected: {}",
                            serde_json::to_string(&check.expected).unwrap()
                        );
                        println!("  error: {err}");
                        continue;
                    }
                };
                let context = EvaluateContext::new_with_state(
                    &engine,
                    &test.context.value,
                    test.time_context(),
                )
                .set_previous(test.context.previous_value.as_ref());
                let actual = match expression.apply(context) {
                    Ok(result) => result,
                    Err(err) => {
                        println!("NOK {name}");
                        println!(
                            "  expected: {}",
                            serde_json::to_string(&check.expected).unwrap()
                        );
                        println!("  error: {err:?}");
                        continue;
                    }
                };
                if actual.as_ref() != &check.expected {
                    println!("NOK {name}");
                    println!(
                        "  expected: {}",
                        serde_json::to_string(&check.expected).unwrap()
                    );
                    println!("  actual: {}", serde_json::to_string(&actual).unwrap());
                } else {
                    tests_passed += 1;
                    println!("OK {name}");
                }
            }
        }
    }

    println!("Passed {tests_passed} of {tests_found} tests");
    if tests_passed < tests_found {
        std::process::exit(1)
    }
}

struct TimeContext {
    now: Option<DateTime<Utc>>,
    timezone: Option<chrono_tz::Tz>,
}

struct Now;

impl rets_expression::function::Function<TimeContext> for Now {
    fn evaluate<'json>(
        &self,
        context: rets_expression::function::FunctionContext<'_, TimeContext>,
        _input: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, rets_expression::function::FunctionError> {
        let now = context
            .state()
            .now
            .ok_or(rets_expression::function::FunctionError::InvalidType)?;

        Ok(Cow::Owned(json!(
            now.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        )))
    }
}

struct Today;

impl rets_expression::function::Function<TimeContext> for Today {
    fn evaluate<'json>(
        &self,
        context: rets_expression::function::FunctionContext<'_, TimeContext>,
        _input: Vec<Cow<'json, serde_json::Value>>,
    ) -> Result<Cow<'json, serde_json::Value>, rets_expression::function::FunctionError> {
        let now = context
            .state()
            .now
            .ok_or(rets_expression::function::FunctionError::InvalidType)?;
        let tz = context
            .state()
            .timezone
            .ok_or(rets_expression::function::FunctionError::InvalidType)?;

        let now = now.with_timezone(&tz);
        let value = now.format("%Y-%m-%d").to_string();

        Ok(Cow::Owned(json!(value)))
    }
}

#[derive(serde::Deserialize)]
struct ExpressionTest {
    name: String,
    context: Context,
    checks: Vec<Check>,
}

impl ExpressionTest {
    pub fn time_context(&self) -> TimeContext {
        TimeContext {
            now: self.context.now,
            timezone: self.context.timezone,
        }
    }
}

#[derive(serde::Deserialize)]
struct Context {
    value: serde_json::Value,
    #[serde(rename = "previousValue")]
    previous_value: Option<serde_json::Value>,
    now: Option<DateTime<Utc>>,
    timezone: Option<chrono_tz::Tz>,
}

#[derive(serde::Deserialize)]
struct Check {
    #[serde(rename = "expr")]
    expression: String,
    expected: serde_json::Value,
}
