#![cfg(test)]

extern crate easter;
extern crate esprit;
extern crate estree;
extern crate glob;
extern crate joker;
extern crate serde_json;

// From 'rustc-test' crate.
// Mirrors Rust's internal 'libtest'.
// https://doc.rust-lang.org/1.1.0/test/index.html
extern crate test;

extern crate unjson;

use easter::expr::Expr;
use easter::patt::{AssignTarget, Patt};
use easter::stmt::{Stmt, StmtListItem};
use esprit::{program, script, Program};
use estree::{IntoModule, IntoScript};
use glob::glob;
use joker::track::Untrack;
use serde_json::value::Value;
use std::fs::{File, read_dir};
use std::io::Read;
use std::path::Path;
use std::env;
use test::{TestDesc, TestDescAndFn, TestName, TestFn, test_main};
use test::ShouldPanic::No;
use unjson::{ExtractField, Unjson};

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
                    let local_test_path = source_path.strip_prefix(&fixtures).unwrap().with_extension("");
                    testignore.iter().any(|ignore| ignore.matches_path(&local_test_path))
                };
                Some((tree_path, source_path, ignore))
            } else {
                None
            }
        });

    for (tree_path, source_path, ignore) in files {
        add_test(target, source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string(), ignore, move || {
            let v: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
            let mut obj = v.into_object().unwrap();
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();

            match program(&source[..]) {
                Ok(Program::Module(mut module)) => {
                    module.untrack();
                    let expected = obj.into_module().expect("failed to deserialize module");
                    assert!(module == expected, "unit test got wrong result\n\
                    expected: {:#?}\n\
                    actual AST: {:#?}", expected, module);
                }
                Ok(Program::Ambiguous(_, mut script)) => {
                    script.untrack();
                    let expected = obj.into_script().expect("failed to deserialize script");
                    assert!(script == expected, "unit test got wrong result\n\
                    expected: {:#?}\n\
                    actual AST: {:#?}", expected, script);
                }
                Err(error) => {
                    match obj.extract_array("errors") {
                        Ok(_) => {},
                        Err(unjson::error::Error::MissingField(_)) => {
                            panic!("unit test failed to parse:\n{:#?}", error);
                        },
                        Err(error) => {
                            panic!("failed to deserialize errors:\n{:#?}", error);
                        }
                    }
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
    test_main(&args, tests);
}
