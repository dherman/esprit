#![cfg(test)]

extern crate esprit;
extern crate estree;
extern crate joker;
extern crate serde_json;
extern crate test;
extern crate unjson;

use esprit::script;
use estree::IntoScript;
use joker::track::Untrack;
use serde_json::value::Value;
use std::ffi::OsStr;
use std::fs::{File, read_dir};
use std::io::{Read, Write, stdout};
use std::path::Path;
use std::{thread, env};
use test::{TestDesc, TestDescAndFn, TestName, TestFn, Bencher, TDynBenchFn, test_main};
use test::ShouldPanic::No;
use unjson::Unjson;

struct DynBenchFn<F> {
    run: F
}

impl<F: Fn(&mut Bencher) + Send + 'static> TDynBenchFn for DynBenchFn<F> {
    fn run(&self, harness: &mut Bencher) {
        (self.run)(harness);
    }
}

fn add_bench<F: Fn(&mut Bencher) + Send + 'static>(tests: &mut Vec<TestDescAndFn>, name: String, ignore: bool, f: F) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(name),
            ignore: ignore,
            should_panic: No
        },
        testfn: TestFn::DynBenchFn(Box::new(DynBenchFn { run: f }))
    });
}

const DEFAULT_MB: usize = 4;

fn stack_size() -> usize {
    match env::var("ESTREE_STACK_SIZE_MB") {
        Ok(s) => match s.parse() {
            Ok(x) => Some(x),
            Err(_) => None
        },
        Err(env::VarError::NotPresent) => Some(DEFAULT_MB),
        Err(_) => None
    }.unwrap_or_else(|| {
        println!("warning: invalid ESTREE_STACK_SIZE_MB value; defaulting to {}MB", DEFAULT_MB);
        DEFAULT_MB
    }) * 1024 * 1024
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
        let name = source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string();
        if !ignore {
            let mut source = String::new();
            File::open(source_path.clone()).unwrap().read_to_string(&mut source).unwrap();
            print!("Parsing JSON {}...", tree_path.strip_prefix(&root).unwrap().to_str().unwrap());
            stdout().flush().unwrap();
            let expected_ast = {
                let v: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
                v.into_object().unwrap().into_script().map_err(|err| {
                    format!("failed to deserialize script: {}", err)
                }).unwrap()
            };
            println!(" done");
            add_bench(target, name, ignore, move |mut bench| {
                let mut result = None;
                bench.iter(|| {
                    result = Some(script(&source[..]))
                });
                match result.unwrap() {
                    Ok(mut actual_ast) => {
                        actual_ast.untrack();
                        assert!(actual_ast == expected_ast, "integration test got wrong result");
                    }
                    Err(actual_err) => {
                        panic!("integration test failed to parse:\n{:#?}", actual_err);
                    }
                }
            });
        } else {
            add_bench(target, name, ignore, |_| {});
        }
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let bench = args.contains(&"--bench".to_string());
    let ignore_integration_tests = !bench && env::var_os("ESTREE_INTEGRATION_TESTS") == None;
    if ignore_integration_tests {
        println!("note: Run with `ESTREE_INTEGRATION_TESTS=1` to run with integration tests (much slower).");
    }
    thread::Builder::new().stack_size(stack_size()).spawn(move || {
        let mut tests = Vec::new();
        integration_tests(&mut tests, ignore_integration_tests);
        test_main(&args, tests);
    }).unwrap().join().unwrap();
}
