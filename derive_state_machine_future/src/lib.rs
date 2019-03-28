//! The compiler for `derive(StateMachineFuture)`.

#![recursion_limit = "128"]

#[macro_use]
extern crate darling;
extern crate heck;
extern crate petgraph;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

mod ast;
mod codegen;
mod phases;

use darling::FromDeriveInput;
use proc_macro::TokenStream;

use ast::StateMachine;
use phases::Pass;

#[proc_macro_derive(StateMachineFuture, attributes(state_machine_future))]
pub fn derive_state_machine_future(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as syn::DeriveInput);

    let machine = match StateMachine::<phases::Parsed>::from_derive_input(&derive_input) {
        Ok(sm) => sm,
        Err(e) => panic!("error in derive(StateMachineFuture): {}", e),
    };

    let machine = phases::StartReadyError::pass(machine);
    let machine = phases::ValidTransitionEdges::pass(machine);
    let machine = phases::ValidPaths::pass(machine);
    let machine = phases::StateGenerics::pass(machine);
    let machine = phases::AfterStateGenerics::pass(machine);
    let machine = phases::ReadyForCodegen::pass(machine);

    quote!(#machine).into()
}
