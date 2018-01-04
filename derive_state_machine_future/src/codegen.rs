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
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let states = self.states();

        let derive = if self.derive.is_empty() {
            quote!{}
        } else {
            let derive = &*self.derive;
            quote! {
                #[derive( #( #derive ),* )]
            }
        };

        let states_variants: Vec<_> = states
            .iter()
            .map(|s| {
                let ty_generics = s.extra.generics.split_for_impl().1;
                let ident = &s.ident;

                quote! {
                    #ident(#ident #ty_generics)
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
            .map(|state| state.future_poll_match_arm(&ty_generics))
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

        let futures_crate = &*self.extra.futures_crate;
        let smf_crate = &*self.extra.smf_crate;

        let mut quiet = "__smf_quiet_warnings_for_".to_string();
        quiet += &state_machine_name.to_snake_case();
        let quiet = quote::Ident::new(quiet);

        let quiet_constructions: Vec<_> = self.states()
            .iter()
            .map(|s| {
                let s_ident = &s.ident;
                match s.data.style {
                    darling::ast::Style::Unit => quote! {
                        let _ = ::std::mem::replace(xxx, #ident::#s_ident);
                    },
                    darling::ast::Style::Tuple => {
                        let fields = s.data.fields.iter().map(|_| {
                            quote! {
                                conjure()
                            }
                        });
                        quote! {
                            let _ = ::std::mem::replace(
                                xxx,
                                #ident::#s_ident( #( #fields ),* ),
                            );
                        }
                    }
                    darling::ast::Style::Struct => {
                        let fields = s.data.fields.iter().map(|f| {
                            let f = &f.ident;
                            quote! {
                                #f: conjure()
                            }
                        });
                        quote! {
                            let _ = ::std::mem::replace(
                                xxx,
                                #ident::#s_ident { #( #fields ),* },
                            );
                        }
                    }
                }
            })
            .collect();

        tokens.append(quote! {
            extern crate futures as #futures_crate;
            extern crate state_machine_future as #smf_crate;

            #( #states )*

            #derive
            #[allow(dead_code)]
            enum #states_enum #impl_generics #where_clause {
                #( #states_variants ),*
            }

            #( #state_machine_attrs )*
            #derive
            #[must_use = "futures do nothing unless polled"]
            #vis struct #state_machine_ident #impl_generics(
                Option<#states_enum #ty_generics>
            ) #where_clause;

            impl #impl_generics #futures_crate::Future
                for #state_machine_ident #ty_generics #where_clause {
                type Item = #future_item;
                type Error = #future_error;

                #[allow(unreachable_code)]
                fn poll(&mut self) -> #futures_crate::Poll<Self::Item, Self::Error> {
                    loop {
                        let state = match self.0.take() {
                            Some(state) => state,
                            None => return Ok(#futures_crate::Async::NotReady),
                        };
                        self.0 = match state {
                            #( #poll_match_arms )*
                        };
                    }
                }
            }

            impl #impl_generics #smf_crate::StateMachineFuture
                for #ident #ty_generics #where_clause
            {
                type Future = #state_machine_ident #ty_generics;
            }

            #vis trait #poll_trait #impl_generics
                : #smf_crate::StateMachineFuture
                #where_clause
            {
                #( #poll_trait_methods )*
            }

            impl #impl_generics #ident #ty_generics #where_clause {
                #start_doc
                #[allow(dead_code)]
                #vis fn start( #( #start_params ),* ) -> #state_machine_ident #ty_generics {
                    #state_machine_ident(
                        Some(
                            #states_enum::#start_state_ident(
                                #start_value
                            )
                        )
                    )
                }
            }

            #[allow(warnings)]
            fn #quiet #impl_generics (xxx: &mut #ident #ty_generics) #where_clause {
                fn conjure<SmfAnyType>() -> SmfAnyType {
                    unreachable!()
                }
                #(
                    #quiet_constructions;
                )*
            }
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
                .unwrap_or_else(|_| {
                    process::Command::new("rustfmt")
                        .stdin(process::Stdio::piped())
                        .spawn()
                        .unwrap()
                });
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
    fn future_poll_match_arm(&self, ty_generics: &syn::TyGenerics) -> quote::Tokens {
        let ident = &self.ident;
        let ident_string = ident.to_string();
        let var = to_var(&ident_string);
        let states_enum = &*self.extra.states_enum;
        let poll_trait = &*self.extra.poll_trait;
        let futures_crate = &*self.extra.futures_crate;
        let smf_crate = &*self.extra.smf_crate;

        if self.ready {
            return quote! {
                #states_enum::#ident(#ident(#var)) => {
                    return Ok(#futures_crate::Async::Ready(#var));
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

        let ready = self.transitions.iter().map(|t| {
            let t_var = to_var(t.to_string());
            quote! {
                Ok(#futures_crate::Async::Ready(#after::#t(#t_var))) => {
                    Some(#states_enum::#t(#t_var))
                }
            }
        });

        quote! {
            #states_enum::#ident(#var) => {
                let (#var, result) =
                    #smf_crate::RentToOwn::with(
                        #var,
                        <#description_ident #ty_generics as #poll_trait #ty_generics>::#poll_method
                    );
                match result {
                    Err(e) => {
                        Some(#states_enum::#error_ident(#error_ident(e)))
                    }
                    Ok(#futures_crate::Async::NotReady) => {
                        self.0 = #var.map(#states_enum::#ident);
                        return Ok(#futures_crate::Async::NotReady);
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
        let ty_generics = self.extra.generics.split_for_impl().1;
        let (_, after_ty_generics, _) = self.extra.after_state_generics.split_for_impl();
        let error_type = &*self.extra.error_type;
        let futures_crate = &*self.extra.futures_crate;
        let smf_crate = &*self.extra.smf_crate;

        quote! {
            #poll_method_doc
            fn #poll_method<'smf_poll>(
                &'smf_poll mut #smf_crate::RentToOwn<'smf_poll, #me #ty_generics>
            ) -> #futures_crate::Poll<#after #after_ty_generics, #error_type>;
        }
    }
}

impl ToTokens for State<phases::ReadyForCodegen> {
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        let vis = &*self.extra.vis;
        let ident_name = self.ident.to_string();
        let ident = &self.ident;
        let attrs = &self.attrs;
        let (impl_generics, _, where_clause) = self.extra.generics.split_for_impl();
        let (after_impl_generics, after_ty_generics, after_where_clause) =
            self.extra.after_state_generics.split_for_impl();

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
                #vis struct #ident;
            },
            darling::ast::Style::Tuple => quote! {
                #( #attrs )*
                #derive
                #vis struct #ident #impl_generics( #( #fields ),* ) #where_clause;
            },
            darling::ast::Style::Struct => quote! {
                #( #attrs )*
                #derive
                #vis struct #ident #impl_generics #where_clause {
                    #( #fields ),*
                }
            },
        });

        if self.ready || self.error {
            return;
        }

        let after_ident = &self.extra.after;

        let after_variants: Vec<_> = self.extra
            .transition_state_generics
            .iter()
            .map(|(s, g)| {
                let doc = doc_string(format!(
                    "A transition from the `{}` state to the `{}` state.",
                    ident_name, s
                ));
                let ty_generics = g.split_for_impl().1;
                quote! {
                    #doc
                    #s(#s #ty_generics)
                }
            })
            .collect();

        let after_froms: Vec<_> = self.extra
            .transition_state_generics
            .iter()
            .map(|(s, g)| {
                let s_var = to_var(s.to_string());
                let trans_ty_generics = g.split_for_impl().1;

                quote! {
                    impl #after_impl_generics From<#s #trans_ty_generics>
                        for #after_ident #after_ty_generics #after_where_clause {
                        fn from(#s_var: #s #trans_ty_generics) -> Self {
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
            #vis enum #after_ident #after_impl_generics #after_where_clause {
                #( #after_variants ),*
            }
            #( #after_froms )*
        });
    }
}
