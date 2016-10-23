#![cfg(test)]

extern crate easter;
extern crate esprit;
extern crate estree;
extern crate joker;
extern crate serde_json;
extern crate test;
extern crate unjson;

use easter::expr::Expr;
use easter::patt::{AssignTarget, Patt};
use easter::prog::Script;
use easter::stmt::{Stmt, StmtListItem};
use esprit::script;
use estree::IntoScript;
use joker::track::Untrack;
use std::{thread, env};
use test::{TestDesc, TestDescAndFn, TestName, TestFn, test_main};
use test::ShouldPanic::No;
use unjson::{Unjson, ExtractField};
use unjson::ty::Array;

struct ParserTest {
    pub filename: Option<String>,
    pub source: String,
    pub expected: Option<Result<Script, String>>
}

fn deserialize_parser_tests(src: &str) -> Vec<ParserTest> {
    let arr: Array = serde_json::from_str(src).unwrap();
    arr.into_iter().map(|v| {
        let mut obj = v.into_object().unwrap();
        ParserTest {
            filename: obj.extract_string("filename").ok(),
            source: obj.extract_string("source").unwrap(),
            expected: obj.extract_object_opt("expected").unwrap().map(|obj| obj.into_script().map_err(|err| {
                format!("failed to deserialize script: {}", err)
            }))
        }
    }).collect()
}

/*
fn deserialize_parser_integration_tests(modpath: &str) -> () {

}
*/

fn add_test<F: FnOnce() + Send + 'static>(tests: &mut Vec<TestDescAndFn>, name: String, f: F) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(name),
            ignore: false,
            should_panic: No
        },
        testfn: TestFn::dyn_test_fn(f)
    });
}

fn as_ref_test(tests: &mut Vec<TestDescAndFn>) {
    add_test(tests, "reference test".to_string(), || {
        let mut ast = script("foobar = 17;").unwrap();
        ast.untrack();
        match ast.body.first().unwrap() {
            &StmtListItem::Stmt(Stmt::Expr(_, Expr::Assign(_, Patt::Simple(AssignTarget::Id(ref id)), _), _)) => {
                assert_eq!(id.name.as_ref(), "foobar");
            }
            _ => { panic!("unexpected AST structure"); }
        }
    });
}

fn unit_tests(target: &mut Vec<TestDescAndFn>) {
    let tests = deserialize_parser_tests(include_str!("../tests/build/unit.json"));
    for ParserTest { source, expected, .. } in tests {
        add_test(target, source.clone(), move || {
            let expected = expected.map(|ast| ast.unwrap());
            let result = script(&source[..]);
            match (result, expected) {
                (Ok(mut actual_ast), Some(expected_ast)) => {
                    actual_ast.untrack();
                    // if actual_ast != expected_ast {
                    //     println!("expected AST: {:#?}", expected_ast);
                    //     println!("actual AST:   {:#?}", actual_ast);
                    // }
                    assert!(actual_ast == expected_ast, "unit test got wrong result\n\
                    expected AST: {:#?}\n\
                    actual AST: {:#?}", expected_ast, actual_ast);
                }
                (Err(_), None) => { }
                (Ok(mut actual_ast), None) => {
                    actual_ast.untrack();
                    panic!("expected error, got AST:\n{:#?}", actual_ast);
                }
                (Err(actual_err), Some(expected_ast)) => {
                    panic!("expected AST, got error\n\
                    expected AST: {:#?}\n\
                    actual error: {:#?}", expected_ast, actual_err);
                }
            }
        });
    }
}

const DEFAULT_MB: usize = 4;

fn read_envvar() -> Option<usize> {
    match env::var("ESTREE_STACK_SIZE_MB") {
        Ok(s) => {
            match s.parse() {
                Ok(x) => Some(x),
                Err(_) => None
            }
        }
        Err(env::VarError::NotPresent) => Some(DEFAULT_MB),
        Err(_) => None
    }
}

fn stack_size() -> usize {
    let mb = match read_envvar() {
        Some(x) => x,
        None => {
            println!("warning: invalid ESTREE_STACK_SIZE_MB value; defaulting to 4MB");
            DEFAULT_MB
        }
    };
    mb * 1024 * 1024
}

fn integration_tests(target: &mut Vec<TestDescAndFn>) {
    let child = thread::Builder::new().stack_size(stack_size()).spawn(|| {
        deserialize_parser_tests(include_str!("../tests/build/integration.json"))
    }).unwrap();
    let tests = child.join().unwrap();
    for ParserTest { filename, source, expected } in tests {
        add_test(target, filename.unwrap(), move || {
            let expected_ast = expected.unwrap().unwrap();
            match script(&source[..]) {
                Ok(mut actual_ast) => {
                    actual_ast.untrack();
                    assert!(actual_ast == expected_ast, "integration test got wrong result\n\
                    expected AST: {:#?}\n\
                    actual AST: {:#?}", expected_ast, actual_ast);
                }
                Err(actual_err) => {
                    panic!("integration test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    as_ref_test(&mut tests);
    unit_tests(&mut tests);
    integration_tests(&mut tests);
    test_main(&args, tests);
}
