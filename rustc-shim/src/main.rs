// Frankenstein rustc shim
//
// Invokes rustc's frontend to get MIR, serializes to JSON on stdout.
// Must be built with nightly + rustc-dev component.

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;

use rustc_driver::{Callbacks, Compilation};
use rustc_middle::ty::TyCtxt;
use std::env;

struct FrankensteinCallbacks;

impl Callbacks for FrankensteinCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        dump_mir(tcx);
        Compilation::Stop
    }
}

fn dump_mir(tcx: TyCtxt<'_>) {
    print!("[");
    let mut first = true;

    for local_def_id in tcx.hir_body_owners() {
        let def_id: rustc_span::def_id::DefId = local_def_id.into();
        if !tcx.is_mir_available(def_id) {
            continue;
        }
        let body = tcx.optimized_mir(def_id);
        let name = tcx.def_path_str(def_id);

        if !first { print!(","); }
        first = false;

        print!("{{\"name\":{},", json_str(&name));
        print!("\"arg_count\":{},", body.arg_count);

        // Local declarations
        print!("\"locals\":[");
        for (i, local) in body.local_decls.iter().enumerate() {
            if i > 0 { print!(","); }
            print!("{{\"idx\":{},\"ty\":{},\"mut\":{}}}",
                i,
                json_str(&format!("{:?}", local.ty)),
                if local.mutability == rustc_middle::mir::Mutability::Mut { "true" } else { "false" }
            );
        }
        print!("],");

        // Basic blocks
        print!("\"blocks\":[");
        for (i, bb) in body.basic_blocks.iter().enumerate() {
            if i > 0 { print!(","); }
            print!("{{\"idx\":{},\"stmts\":[", i);
            for (j, stmt) in bb.statements.iter().enumerate() {
                if j > 0 { print!(","); }
                print!("{}", json_str(&format!("{:?}", stmt.kind)));
            }
            print!("],\"term\":");
            match &bb.terminator {
                Some(term) => print!("{}", json_str(&format!("{:?}", term.kind))),
                None => print!("null"),
            }
            print!("}}");
        }
        print!("]");

        print!("}}");
    }
    println!("]");
}

fn json_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c < '\x20' => {
                use std::fmt::Write;
                write!(out, "\\u{:04x}", c as u32).unwrap();
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: rustc-mir-dump <input.rs>");
        std::process::exit(1);
    }

    let rustc_args: Vec<String> = vec![
        "rustc-mir-dump".to_string(),
        args[1].clone(),
        "--edition=2021".to_string(),
    ];

    let mut callbacks = FrankensteinCallbacks;
    rustc_driver::run_compiler(&rustc_args, &mut callbacks);
}
