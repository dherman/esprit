#![cfg(test)]

extern crate esprit;
extern crate estree;
extern crate joker;
extern crate serde_json;

// From 'rustc-test' crate.
// Mirrors Rust's internal 'libtest'.
// https://doc.rust-lang.org/1.1.0/test/index.html
extern crate test;

extern crate unjson;

use esprit::script;
use estree::IntoScript;
use joker::track::Untrack;
use serde_json::value::Value;
use std::ffi::OsStr;
use std::fs::{File, read_dir};
use std::io::Read;
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

fn integration_tests(target: &mut Vec<TestDescAndFn>, ignore: bool, stack_size: usize) {
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
            let mut source_path = fixtures.join(tree_path.file_name().unwrap());
            source_path.set_extension("js");
            let name = source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string();

            (tree_path, source_path, name)
        });

    if ignore {
        // Fast path for ignored tests needs only their names
        for (_, _, name) in files {
            add_bench(target, name, true, |_| {});
        }
        return;
    }

    let tests =
        files
        .map(|(tree_path, source_path, name)| {
            let tree_name = tree_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string();

            // Read & parse each JSON in dedicated thread
            let thread =
                thread::Builder::new()
                .name(tree_name.clone())
                .stack_size(stack_size)
                .spawn(move || {
                    let mut source = String::new();
                    File::open(source_path).unwrap().read_to_string(&mut source).unwrap();

                    println!("Parsing JSON {}...", tree_name);
                    let expected_ast = {
                        let v: Value = serde_json::de::from_reader(File::open(&tree_path).unwrap()).unwrap();
                        v.into_object().unwrap().into_script().map_err(|err| {
                            format!("failed to deserialize script: {}", err)
                        }).unwrap()
                    };
                    println!("Done parsing {}", tree_name);
                    (source, expected_ast)
                })
                .unwrap();

            (name, thread)
        })
        // First collect to evaluate iterator and spawn threads
        .collect::<Vec<_>>()
        .into_iter()
        .map(|(name, thread)| {
            let (source, expected_ast) = thread.join().unwrap();
            (name, source, expected_ast)
        })
         // Then collect results from threads so that they don't affect benchmark numbers
        .collect::<Vec<_>>();

    for (name, source, expected_ast) in tests {
        add_bench(target, name, false, move |mut bench| {
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
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let bench = args.contains(&"--bench".to_string());
    let ignore_integration_tests = !bench && env::var_os("ESTREE_INTEGRATION_TESTS") == None;
    if ignore_integration_tests {
        println!("note: Run with `ESTREE_INTEGRATION_TESTS=1` to run with integration tests (much slower).");
    }
    let stack_size = std::cmp::max(
        // standard Rust env variable
        env::var("RUST_MIN_STACK")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or_default(),
        // ...but with custom minimal value
        4 * 1024 * 1024
    );
    thread::Builder::new()
    .name("bench".to_string())
    .stack_size(stack_size)
    .spawn(move || {
        let mut tests = Vec::new();
        integration_tests(&mut tests, ignore_integration_tests, stack_size);
        test_main(&args, tests);
    }).unwrap().join().unwrap();
}
