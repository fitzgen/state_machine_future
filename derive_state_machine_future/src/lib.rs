//! The compiler for `derive(StateMachineFuture)`.

#![recursion_limit = "128"]

#[macro_use]
extern crate darling;
extern crate heck;
extern crate petgraph;
extern crate proc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

mod ast;
mod codegen;
mod phases;

use ast::StateMachine;
use darling::FromDeriveInput;
use phases::Pass;
use proc_macro::TokenStream;
use quote::ToTokens;

#[proc_macro_derive(StateMachineFuture, attributes(state_machine_future))]
pub fn derive_state_machine_future(tokens: TokenStream) -> TokenStream {
    let source = tokens.to_string();

    let derive_input =
        syn::parse_derive_input(&source).expect("should parse source into derive input");

    let machine = match StateMachine::<phases::Parsed>::from_derive_input(&derive_input) {
        Ok(sm) => sm,
        Err(e) => panic!("error in derive(StateMachineFuture): {}", e),
    };

    let machine = phases::StartReadyError::pass(machine);
    let machine = phases::ValidTransitionEdges::pass(machine);
    let machine = phases::ValidPaths::pass(machine);
    let machine = phases::ReadyForCodegen::pass(machine);

    let mut tokens = quote!();
    machine.to_tokens(&mut tokens);

    tokens
        .parse()
        .expect("should parse expanded output source into tokens")
}
