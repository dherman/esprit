#![cfg(test)]

extern crate easter;
extern crate esprit;
extern crate estree;
extern crate glob;
extern crate joker;
extern crate serde_json;
extern crate test;
extern crate unjson;

use easter::expr::Expr;
use easter::patt::{AssignTarget, Patt};
use easter::stmt::{Stmt, StmtListItem};
use esprit::script;
use estree::IntoScript;
use glob::glob;
use joker::track::Untrack;
use serde_json::value::Value;
use std::ffi::OsStr;
use std::fs::{File, read_dir};
use std::io::Read;
use std::path::Path;
use std::{thread, env};
use test::{TestDesc, TestDescAndFn, TestName, TestFn, test_main};
use test::ShouldPanic::No;
use unjson::Unjson;

fn add_test<F: FnOnce() + Send + 'static>(tests: &mut Vec<TestDescAndFn>, name: String, ignore: bool, f: F) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(name),
            ignore: ignore,
            should_panic: No
        },
        testfn: TestFn::dyn_test_fn(f)
    });
}

fn as_ref_test(tests: &mut Vec<TestDescAndFn>) {
    add_test(tests, "reference test".to_string(), false, || {
        let mut ast = script("foobar = 17;").unwrap();
        ast.untrack();
        match ast.items.first().unwrap() {
            &StmtListItem::Stmt(Stmt::Expr(_, Expr::Assign(_, Patt::Simple(AssignTarget::Id(ref id)), _), _)) => {
                assert_eq!(id.name.as_ref(), "foobar");
            }
            _ => { panic!("unexpected AST structure"); }
        }
    });
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

fn integration_tests(target: &mut Vec<TestDescAndFn>, ignore: bool) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let fixtures = {
        let mut fixtures = root.to_path_buf();
        fixtures.push("tests");
        fixtures.push("esprima");
        fixtures.push("test");
        fixtures.push("3rdparty");
        fixtures
    };

    let syntax = fixtures.join("syntax");

    let files =
        read_dir(syntax).unwrap()
        .map(|dir| dir.unwrap().path())
        .filter(|path| path.extension() == Some(OsStr::new("json")))
        .map(|tree_path| {
            let mut source_path_buf = fixtures.join(tree_path.file_name().unwrap());
            source_path_buf.set_extension("js");
            (tree_path, source_path_buf)
        });

    for (tree_path, source_path) in files {
        add_test(target, source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string(), ignore, move || {
            let expected_ast = thread::Builder::new().stack_size(stack_size()).spawn(|| {
                let v: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
                v.into_object().unwrap().into_script().map_err(|err| {
                    format!("failed to deserialize script: {}", err)
                }).unwrap()
            }).unwrap().join().unwrap();
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match script(&source[..]) {
                Ok(mut actual_ast) => {
                    actual_ast.untrack();
                    assert!(actual_ast == expected_ast, "integration test got wrong result");
                }
                Err(actual_err) => {
                    panic!("integration test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn unit_tests(target: &mut Vec<TestDescAndFn>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let fixtures = {
        let mut fixtures = root.to_path_buf();
        fixtures.push("tests");
        fixtures.push("esprima");
        fixtures.push("test");
        fixtures.push("fixtures");
        fixtures
    };

    let testignore: Vec<_> =
        include_str!(".testignore")
        .lines()
        .filter(|s| !s.is_empty() && !s.starts_with("#"))
        .map(|s| glob::Pattern::new(s).unwrap())
        .collect();

    let files =
        read_dir(fixtures.as_path()).unwrap()
        .flat_map(|dir| glob(&format!("{}/**/*.tree.json", dir.unwrap().path().to_str().unwrap())).unwrap())
        .filter_map(|entry| {
            let tree_path = entry.unwrap();
            let source_path = {
                let tree_file_name = tree_path.file_name().unwrap().to_str().unwrap();
                let source_file_name = tree_file_name[..tree_file_name.len() - 9].to_string() + "js";
                tree_path.with_file_name(source_file_name)
            };
            if source_path.exists() {
                let ignore = {
                    let local_tree_path = tree_path.strip_prefix(&fixtures).unwrap();
                    testignore.iter().any(|ignore| ignore.matches_path(&local_tree_path))
                };
                Some((tree_path, source_path, ignore))
            } else {
                None
            }
        });

    for (tree_path, source_path, ignore) in files {
        add_test(target, source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string(), ignore, move || {
            let v: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
            let expected_ast = v.into_object().unwrap().into_script().map_err(|err| {
                format!("failed to deserialize script: {}", err)
            }).unwrap();
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match script(&source[..]) {
                Ok(mut actual_ast) => {
                    actual_ast.untrack();
                    assert!(actual_ast == expected_ast, "unit test got wrong result\n\
                    expected AST: {:#?}\n\
                    actual AST: {:#?}", expected_ast, actual_ast);
                }
                Err(actual_err) => {
                    panic!("unit test failed to parse:\n{:#?}", actual_err);
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
    let ignore_integration_tests = env::var_os("ESTREE_INTEGRATION_TESTS") == None;
    if ignore_integration_tests {
        println!("note: Run with `ESTREE_INTEGRATION_TESTS=1` to run with integration tests (much slower).");
    }
    integration_tests(&mut tests, ignore_integration_tests);
    test_main(&args, tests);
}
