//! Final AST -> tokens code generation.

use ast::{State, StateMachine};
use darling;
use heck::SnakeCase;
use phases;
use quote::{self, ToTokens};
use syn;

fn doc_string<S: AsRef<str>>(s: S) -> quote::Tokens {
    let s = s.as_ref();

    let mut doc = String::with_capacity(s.len() + "\n\n/// \n".len());
    doc.push_str("\n\n/// ");
    doc.push_str(s);
    doc.push('\n');

    let mut tokens = quote!{};
    quote::Ident::new(doc).to_tokens(&mut tokens);
    tokens
}

fn to_var<S: AsRef<str>>(s: S) -> quote::Ident {
    let s = s.as_ref().to_snake_case();
    match s.as_str() {
        "abstract" | "alignof" | "as" | "become" | "box" | "break" | "const" | "continue"
        | "crate" | "do" | "else" | "enum" | "extern" | "false" | "final" | "fn" | "for" | "if"
        | "impl" | "in" | "let" | "loop" | "macro" | "match" | "mod" | "move" | "mut"
        | "offsetof" | "override" | "priv" | "proc" | "pub" | "pure" | "ref" | "return"
        | "Self" | "self" | "sizeof" | "static" | "struct" | "super" | "trait" | "true"
        | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" | "while"
        | "yield" | "bool" | "_" => {
            let mut var = String::from("var_");
            var.push_str(&s);
            quote::Ident::new(var)
        }
        _ => quote::Ident::new(s),
    }
}

impl ToTokens for StateMachine<phases::ReadyForCodegen> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        if cfg!(features = "debug_code_generation") {
            println!("StateMachine::to_tokens: self = {:#?}", self);
        }

        let vis = &self.vis;
        let generics = &self.generics;
        let wheres = &self.generics.where_clause;
        let states = self.states();

        let derive = if self.derive.is_empty() {
            quote!{}
        } else {
            let derive = &*self.derive;
            quote! {
                #[derive( #( #derive ),* )]
            }
        };

        let state_idents: Vec<_> = states.iter().map(|s| &s.ident).collect();
        let state_idents = &state_idents[..];

        let states_variants: Vec<_> = state_idents
            .iter()
            .map(|s| {
                quote! {
                    #s(#s #generics)
                }
            })
            .collect();

        let start = &states[self.extra.start];
        let start_state_ident = &start.ident;

        let ready = &states[self.extra.ready];
        let future_item = &ready.data.fields[0];

        let error = &states[self.extra.error];
        let future_error = &error.data.fields[0];

        let state_machine_attrs = &self.attrs;

        let state_machine_description_name = self.ident.to_string();

        let mut state_machine_name = state_machine_description_name.clone();
        state_machine_name.push_str("Future");

        let mut module_name = state_machine_name.clone();
        module_name.push_str("Module");
        let module_ident = to_var(module_name);

        let ident = &self.ident;
        let state_machine_ident = quote::Ident::new(state_machine_name.as_str());
        let states_enum = &*self.extra.states_enum;

        let start_params = match start.data.style {
            darling::ast::Style::Unit => vec![],
            darling::ast::Style::Tuple => start
                .data
                .fields
                .iter()
                .cloned()
                .enumerate()
                .map(|(i, mut f)| {
                    f.ident = Some(syn::Ident::new(format!("arg{}", i)));
                    f
                })
                .collect(),
            darling::ast::Style::Struct => start.data.fields.clone(),
        };
        let start_params = &start_params;

        let start_value = match start.data.style {
            darling::ast::Style::Unit => quote! {
                #start_state_ident
            },
            darling::ast::Style::Tuple => {
                let args = start_params.iter().map(|f| &f.ident);
                quote! {
                    #start_state_ident( #( #args ),* )
                }
            }
            darling::ast::Style::Struct => {
                let args = start_params.iter().map(|f| &f.ident);
                quote! {
                    #start_state_ident { #( #args ),* }
                }
            }
        };

        let poll_match_arms: Vec<_> = states
            .iter()
            .map(|state| state.future_poll_match_arm())
            .collect();

        let poll_trait = &*self.extra.poll_trait;
        let poll_trait_methods: Vec<_> = states
            .iter()
            .filter(|s| !s.ready && !s.error)
            .map(|state| state.poll_trait_method())
            .collect();

        let start_doc = doc_string(format!(
            "Start executing the `{}` state machine. This constructing its `Future` \
             representation in its initial start state and returns it.",
            state_machine_name
        ));

        tokens.append(quote! {
            mod #module_ident {
                #![allow(unreachable_code, unused_imports)]

                extern crate futures;
                extern crate state_machine_future;
                use super::*;

                #( #states )*

                #derive
                enum #states_enum #generics #wheres {
                    #( #states_variants ),*
                }

                #( #state_machine_attrs )*
                #derive
                #[must_use = "futures do nothing unless polled"]
                pub struct #state_machine_ident #generics(
                    Option<#states_enum #generics>
                ) #wheres;

                impl #generics self::futures::Future for #state_machine_ident #generics #wheres {
                    type Item = #future_item;
                    type Error = #future_error;

                    fn poll(&mut self) -> self::futures::Poll<Self::Item, Self::Error> {
                        loop {
                            let state = match self.0.take() {
                                Some(state) => state,
                                None => return Ok(self::futures::Async::NotReady),
                            };
                            self.0 = match state {
                                #( #poll_match_arms )*
                            };
                        }
                    }
                }

                impl #generics self::state_machine_future::StateMachineFuture
                    for #ident #generics #wheres
                {
                    type Future = #state_machine_ident #generics;
                }

                pub trait #poll_trait #generics : self::state_machine_future::StateMachineFuture
                    #wheres
                {
                    #( #poll_trait_methods )*
                }

                impl #generics #ident #generics #wheres {
                    #start_doc
                    pub fn start( #( #start_params ),* ) -> #state_machine_ident #generics {
                        #state_machine_ident(
                            Some(
                                #states_enum::#start_state_ident(
                                    #start_value
                                )
                            )
                        )
                    }
                }
            }
            #[allow(unused_imports)]
            #vis use self::#module_ident::*;
        });

        if cfg!(feature = "debug_code_generation") {
            use std::io::Write;
            use std::process;

            println!();
            println!("=================================================================");
            println!();

            let mut child = process::Command::new("rustup")
                .args(&["run", "nightly", "rustfmt"])
                .stdin(process::Stdio::piped())
                .spawn()
                .unwrap();
            {
                let mut stdin = child.stdin.take().unwrap();
                stdin.write_all(tokens.to_string().as_bytes()).unwrap();
            }
            child.wait().unwrap();

            println!();
            println!("=================================================================");
            println!();
        }
    }
}

impl State<phases::ReadyForCodegen> {
    fn future_poll_match_arm(&self) -> quote::Tokens {
        let ident = &self.ident;
        let ident_string = ident.to_string();
        let var = to_var(&ident_string);
        let states_enum = &*self.extra.states_enum;
        let poll_trait = &*self.extra.poll_trait;

        if self.ready {
            return quote! {
                #states_enum::#ident(#ident(#var)) => {
                    return Ok(self::futures::Async::Ready(#var));
                }
            };
        }

        let error_ident = &*self.extra.error_ident;
        let error_var = to_var(error_ident.to_string());

        if self.error {
            return quote!{
                #states_enum::#error_ident(#error_ident(#error_var)) => {
                    return Err(#error_var);
                }
            };
        }

        let after = &self.extra.after;
        let poll_method = &self.extra.poll_method;
        let description_ident = &*self.extra.description_ident;
        let generics = &*self.extra.generics;

        let ready = self.transitions.iter().map(|t| {
            let t_var = to_var(t.to_string());
            quote! {
                Ok(self::futures::Async::Ready(#after::#t(#t_var))) => {
                    Some(#states_enum::#t(#t_var))
                }
            }
        });

        quote! {
            #states_enum::#ident(#var) => {
                let (#var, result) =
                    self::state_machine_future::RentToOwn::with(
                        #var,
                        <#description_ident #generics as #poll_trait #generics>::#poll_method
                    );
                match result {
                    Err(e) => {
                        Some(#states_enum::#error_ident(#error_ident(e)))
                    }
                    Ok(self::futures::Async::NotReady) => {
                        self.0 = #var.map(#states_enum::#ident);
                        return Ok(self::futures::Async::NotReady);
                    }
                    #( #ready )*
                }
            }
        }
    }

    fn poll_doc_string(&self) -> quote::Tokens {
        doc_string(format!(
            "Poll the future when it is in the `{}` state and see if it is ready \
             to transition to a new state. If the future is ready to transition \
             into a new state, return `Ok(Async::Ready({}))`. If the future is \
             not ready to transition into a new state, return \
             `Ok(Async::NotReady)`. If an error is encountered, return `Err({})`. \
             The `RentToOwn` wrapper allows you to choose whether to take \
             ownership of the current state or not.",
            self.ident.to_string(),
            self.extra.after,
            {
                let mut t = quote!{};
                self.extra.error_type.to_tokens(&mut t);
                t.to_string()
            },
        ))
    }

    fn poll_trait_method(&self) -> quote::Tokens {
        assert!(!self.ready && !self.error);

        let poll_method = &self.extra.poll_method;
        let poll_method_doc = self.poll_doc_string();
        let me = &self.ident;
        let after = &self.extra.after;
        let generics = &*self.extra.generics;
        let error_type = &*self.extra.error_type;
        quote! {
            #poll_method_doc
            fn #poll_method<'a>(
                &'a mut self::state_machine_future::RentToOwn<'a, #me #generics>
            ) -> self::futures::Poll<#after #generics, #error_type>;
        }
    }
}

impl ToTokens for State<phases::ReadyForCodegen> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        let ident_name = self.ident.to_string();
        let ident = &self.ident;
        let attrs = &self.attrs;
        let generics = &*self.extra.generics;
        let wheres = &self.extra.generics.where_clause;

        let derive = if self.extra.derive.is_empty() {
            quote!{}
        } else {
            let derive = &**self.extra.derive;
            quote! {
                #[derive( #( #derive ),* )]
            }
        };

        let fields: Vec<_> = self.data
            .fields
            .iter()
            .map(|f| {
                let mut f = f.clone();
                f.vis = syn::Visibility::Public;
                f
            })
            .collect();

        tokens.append(quote::Ident::new("\n\n"));
        tokens.append(match self.data.style {
            darling::ast::Style::Unit => quote! {
                #( #attrs )*
                #derive
                pub struct #ident;
            },
            darling::ast::Style::Tuple => quote! {
                #( #attrs )*
                #derive
                pub struct #ident #generics( #( #fields ),* ) #wheres;
            },
            darling::ast::Style::Struct => quote! {
                #( #attrs )*
                #derive
                pub struct #ident #generics #wheres {
                    #( #fields ),*
                }
            },
        });

        if self.ready || self.error {
            return;
        }

        let after_ident = &self.extra.after;

        let after_variants: Vec<_> = self.transitions
            .iter()
            .map(|s| {
                let doc = doc_string(format!(
                    "A transition from the `{}` state to the `{}` state.",
                    ident_name,
                    s
                ));
                quote! {
                    #doc
                    #s(#s #generics)
                }
            })
            .collect();

        let after_froms: Vec<_> = self.transitions
            .iter()
            .map(|s| {
                let s_var = to_var(s.to_string());
                quote! {
                    impl #generics From<#s #generics> for #after_ident #generics #wheres {
                        fn from(#s_var: #s #generics) -> Self {
                            #after_ident::#s(#s_var)
                        }
                    }
                }
            })
            .collect();

        let after_doc = doc_string(format!(
            "The states that the `{}` state can transition to.",
            ident_name
        ));

        tokens.append(quote! {
            #after_doc
            pub enum #after_ident #generics #wheres {
                #( #after_variants ),*
            }
            #( #after_froms )*
        });
    }
}
