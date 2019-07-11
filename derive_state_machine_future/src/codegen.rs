//! Final AST -> tokens code generation.

use darling;
use heck::SnakeCase;
use proc_macro2::{self, Ident, Span};
use quote::{ToTokens, TokenStreamExt};
use syn;

use super::ast::{State, StateMachine};
use super::phases;

fn doc_string<S: AsRef<str>>(s: S) -> proc_macro2::TokenStream {
    let s = s.as_ref();

    let meta = syn::Meta::NameValue(syn::MetaNameValue {
        ident: Ident::new("doc", Span::call_site()),
        eq_token: <Token![=]>::default(),
        lit: syn::Lit::Str(syn::LitStr::new(s, Span::call_site())),
    });

    quote!(#[#meta])
}

fn to_var<S: AsRef<str>>(s: S) -> Ident {
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
            Ident::new(&var, Span::call_site())
        }
        _ => Ident::new(&s, Span::call_site()),
    }
}

impl ToTokens for StateMachine<phases::ReadyForCodegen> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if cfg!(feature = "debug_code_generation") {
            println!("StateMachine::to_tokens: self = {:#?}", self);
        }

        let vis = &self.vis;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let states = self.states();
        let total_states = states.len();

        let derive = if self.derive.is_empty() {
            quote! {}
        } else {
            let derive = &*self.derive;
            quote! {
                #[derive( #( #derive ),* )]
            }
        };

        let mut states_variants: Vec<_> = states
            .iter()
            .map(|s| {
                let ty_generics = s.extra.generics.split_for_impl().1;
                let ident = &s.ident;

                quote! {
                    #ident(#ident #ty_generics)
                }
            })
            .collect();
        states_variants.push(quote! {
            Working__
        });
        let states_variants = states_variants;

        let start = &states[self.extra.start];
        let start_state_ident = &start.ident;

        let ready = &states[self.extra.ready];
        let ready_ident = &ready.ident;
        let ready_var = to_var(&ready_ident.to_string());
        let future_item = &ready.fields.fields[0];

        let error = &states[self.extra.error];
        let error_ident = &error.ident;
        let error_var = to_var(&error_ident.to_string());
        let future_error = &error.fields.fields[0];

        let state_machine_attrs = &self.attrs;

        let state_machine_description_name = self.ident.to_string();

        let mut state_machine_name = state_machine_description_name.clone();
        state_machine_name.push_str("Future");

        let ident = &self.ident;
        let state_machine_ident = Ident::new(state_machine_name.as_str(), Span::call_site());
        let states_enum = &*self.extra.states_enum;

        let start_params = match start.fields.style {
            darling::ast::Style::Unit => vec![],
            darling::ast::Style::Tuple => start
                .fields
                .fields
                .iter()
                .cloned()
                .enumerate()
                .map(|(i, mut f)| {
                    f.ident = Some(syn::Ident::new(&format!("arg{}", i), Span::call_site()));
                    f
                })
                .collect(),
            darling::ast::Style::Struct => start.fields.fields.clone(),
        };
        let start_params = &start_params;

        let start_value = match start.fields.style {
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
            .map(|state| state.future_poll_match_arm(&ty_generics, self.context.as_ref()))
            .collect();
        let mut error_msg = state_machine_name.clone();
        error_msg.push_str(" future has been polled while already completed");
        let working_match_arms = quote! {
            #states_enum::Working__ => {
                unreachable!(#error_msg);
            }
        };

        let future_trait = &*self.extra.future_trait;
        let poll_trait_methods: Vec<_> = states
            .iter()
            .filter(|s| !s.ready && !s.error)
            .map(|state| state.poll_trait_method(&ty_generics, self.context.as_ref()))
            .collect();

        let start_doc = doc_string(format!(
            "Start executing the `{}` state machine. This constructing its `Future` \
             representation in its initial start state and returns it.",
            state_machine_name
        ));

        let smf_crate = &*self.extra.smf_crate;

        let mut quiet = "__smf_quiet_warnings_for_".to_string();
        quiet += &state_machine_name.to_snake_case();
        let quiet = Ident::new(&quiet, Span::call_site());

        let quiet_constructions: Vec<_> = self
            .states()
            .iter()
            .map(|s| {
                let s_ident = &s.ident;
                match s.fields.style {
                    darling::ast::Style::Unit => quote! {
                        let _ = ::std::mem::replace(xxx, #ident::#s_ident);
                    },
                    darling::ast::Style::Tuple => {
                        let fields = s.fields.fields.iter().map(|_| {
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
                        let fields = s.fields.fields.iter().map(|f| {
                            let f = &f.ident;
                            quote! {
                                #f: conjure()
                            }
                        });

                        let match_fields = s.fields.fields.iter().map(|f| {
                            let f = &f.ident;
                            quote! {
                                ref #f
                            }
                        });

                        quote! {
                            let _ = ::std::mem::replace(
                                xxx,
                                #ident::#s_ident { #( #fields ),* },
                            );

                            match *xxx {
                                #ident::#s_ident { #( #match_fields ),* } => { unimplemented!() },
                                _ => { unimplemented!() },
                            }
                        }
                    }
                }
            })
            .collect();

        let has_no_start_parameters = start_params.len() == 0;

        let context_field = match self.context {
            Some(ref ident) => quote! {
                , context: Option<#ident #ty_generics>
            },
            None => quote! {},
        };

        let context_start_arg_decl = match self.context {
            Some(ref ident) if has_no_start_parameters => quote! {
                context: #ident #ty_generics
            },
            Some(ref ident) => quote! {
                , context: #ident #ty_generics
            },
            None => quote! {},
        };

        let context_start_in_arg_decl = match self.context {
            Some(ref ident) => quote! {
                , context: #ident #ty_generics
            },
            None => quote! {},
        };

        let context_start_arg = match self.context {
            Some(_) => quote! {
                , context: Some(context)
            },
            None => quote! {},
        };

        let extract_context = match self.context {
            Some(_) => match total_states {
                // If there is only 1 state it is irrefutable that we are in the ready state.
                1 => quote! {
                    let context = match self.context.take() {
                        Some(context) => context,
                        None => {
                            let #states_enum::#ready_ident(#ready_ident(#ready_var)) = state;
                            return Ok(#smf_crate::export::Async::Ready(#ready_var))
                        }
                    };
                },
                _ => quote! {
                    let context = match self.context.take() {
                        Some(context) => context,
                        None => {
                            return match state {
                                #states_enum::#ready_ident(#ready_ident(#ready_var)) => Ok(#smf_crate::export::Async::Ready(#ready_var)),
                                #states_enum::#error_ident(#error_ident(#error_var)) => Err(#error_var),
                                _ => Ok(#smf_crate::export::Async::NotReady)
                            }
                        }
                    };
                },
            },
            None => quote! {},
        };

        tokens.append_all(quote! {
            use #smf_crate::SMPoll;
            use #smf_crate::SMAsync;

            #( #states )*

            #derive
            #[allow(dead_code)]
            #vis enum #states_enum #impl_generics #where_clause {
                #( #states_variants ),*
            }

            #( #state_machine_attrs )*
            #[must_use = "futures do nothing unless polled"]
            #vis struct #state_machine_ident #impl_generics #where_clause {
                current_state: #states_enum #ty_generics
                #context_field
            }

            impl #impl_generics #smf_crate::export::Future
                for #state_machine_ident #ty_generics #where_clause {
                type Item = #future_item;
                type Error = #future_error;

                #[allow(unreachable_code)]
                fn poll(&mut self) -> #smf_crate::export::Poll<Self::Item, Self::Error> {
                    loop {
                        #extract_context // TODO(baloo): need to workout this
                        match #smf_crate::export::replace(&mut self.current_state, #states_enum::Working__) {
                            #( #poll_match_arms )*
                            #working_match_arms
                        };
                    }
                }
            }

            impl #impl_generics #smf_crate::StateMachineFuture
                for #ident #ty_generics #where_clause
            {
                type Future = #state_machine_ident #ty_generics;
            }

            #vis trait #future_trait #impl_generics
                : #smf_crate::StateMachineFuture
                #where_clause
            {
                #( #poll_trait_methods )*
            }

            impl #impl_generics #ident #ty_generics #where_clause {
                #start_doc
                #[allow(dead_code)]
                #vis fn start( #( #start_params ),* #context_start_arg_decl ) -> #state_machine_ident #ty_generics {
                    #state_machine_ident {
                        current_state: #states_enum::#start_state_ident(
                            #start_value
                        )
                        #context_start_arg
                    }
                }

                #vis fn start_in<STATE: Into<#states_enum #ty_generics>>( state: STATE #context_start_in_arg_decl ) -> #state_machine_ident #ty_generics {
                    #state_machine_ident {
                        current_state: state.into()
                        #context_start_arg
                    }
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

        let state_froms: Vec<_> = states
            .iter()
            .map(|s| {
                let state_ident = &s.ident;
                let state_ident_var = to_var(state_ident.to_string());
                let state_generics = s.extra.generics.split_for_impl().1;

                quote! {
                    impl #impl_generics From<#state_ident #state_generics>
                        for #states_enum #ty_generics #where_clause {
                        fn from(#state_ident_var: #state_ident #state_generics) -> Self {
                            #states_enum::#state_ident(#state_ident_var)
                        }
                    }
                }
            })
            .collect();

        tokens.append_all(quote! {
            #( #state_froms )*
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
    fn future_poll_match_arm(
        &self,
        ty_generics: &syn::TypeGenerics,
        context: Option<&syn::Ident>,
    ) -> proc_macro2::TokenStream {
        let ident = &self.ident;
        let ident_string = ident.to_string();
        let var = to_var(&ident_string);
        let states_enum = &*self.extra.states_enum;
        let future_trait = &*self.extra.future_trait;
        let smf_crate = &*self.extra.smf_crate;

        if self.ready {
            return quote! {
                #states_enum::#ident(#ident(#var)) => {
                    return Ok(#smf_crate::export::Async::Ready(#var));
                }
            };
        }

        let error_ident = &*self.extra.error_ident;
        let error_var = to_var(error_ident.to_string());

        if self.error {
            return quote! {
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
                Ok(#smf_crate::SMAsync::Ready(#after::#t(#t_var))) => {
                    #smf_crate::export::replace(&mut self.current_state, #states_enum::#t(#t_var));
                }
            }
        });

        let poll_method_call = match context {
            Some(_) => quote! {
                |state| {
                    let (context, result) =
                        #smf_crate::RentToOwn::with(
                            context,
                            move |context| {
                                <#description_ident #ty_generics as #future_trait #ty_generics>::#poll_method(state, context)
                            }
                        );

                    self.context = context;

                    result
                }
            },
            None => quote! {
                <#description_ident #ty_generics as #future_trait #ty_generics>::#poll_method
            },
        };

        quote! {
            #states_enum::#ident(#var) => {
                let out = #poll_method_call(#var);
                match out {
                    Err(e) => {
                        // We replaced the state, poll will now loop and poll on
                        // the new state
                        #smf_crate::export::replace(&mut self.current_state, #states_enum::#error_ident(#error_ident(e)));
                    },
                    Ok(#smf_crate::SMAsync::NotReady(not_ready)) => {
                        #smf_crate::export::replace(&mut self.current_state, #states_enum::#ident(not_ready));
                        return Ok(#smf_crate::export::Async::NotReady);
                    }
                    #( #ready )*
                }
            }
        }
    }

    fn poll_doc_string(&self) -> proc_macro2::TokenStream {
        // TODO(baloo): fix that
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
                let mut t = quote! {};
                self.extra.error_type.to_tokens(&mut t);
                t.to_string()
            },
        ))
    }

    fn poll_trait_method(
        &self,
        sm_ty_generics: &syn::TypeGenerics,
        context: Option<&syn::Ident>,
    ) -> proc_macro2::TokenStream {
        assert!(!self.ready && !self.error);

        let poll_method = &self.extra.poll_method;
        let poll_method_doc = self.poll_doc_string();
        let me = &self.ident;
        let after = &self.extra.after;
        let ty_generics = self.extra.generics.split_for_impl().1;
        let (_, after_ty_generics, _) = self.extra.after_state_generics.split_for_impl();
        let error_type = &*self.extra.error_type;
        let smf_crate = &*self.extra.smf_crate;

        let context_param = match context {
            Some(ident) => quote! {
                , _: &'smf_poll_context mut #smf_crate::RentToOwn<'smf_poll_context, #ident #sm_ty_generics>
            },
            None => quote! {},
        };

        quote! {
            #poll_method_doc
            fn #poll_method(
                _: #me #ty_generics
                #context_param
            ) -> #smf_crate::SMPoll<#after #after_ty_generics, #me, #error_type>;
        }
    }
}

impl ToTokens for State<phases::ReadyForCodegen> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let vis = &*self.extra.vis;
        let ident_name = self.ident.to_string();
        let ident = &self.ident;
        let attrs = &self.attrs;
        let (impl_generics, _, where_clause) = self.extra.generics.split_for_impl();
        let (after_impl_generics, after_ty_generics, after_where_clause) =
            self.extra.after_state_generics.split_for_impl();

        let derive = if self.extra.derive.is_empty() {
            quote! {}
        } else {
            let derive = &**self.extra.derive;
            quote! {
                #[derive( #( #derive ),* )]
            }
        };

        let fields: Vec<_> = self
            .fields
            .fields
            .iter()
            .map(|f| {
                let mut f = f.clone();
                f.vis = syn::VisPublic {
                    pub_token: <Token![pub]>::default(),
                }
                .into();
                f
            })
            .collect();

        tokens.append_all(match self.fields.style {
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

        let after_variants: Vec<_> = self
            .extra
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

        let after_froms: Vec<_> = self
            .extra
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

        tokens.append_all(quote! {
            #after_doc
            #vis enum #after_ident #after_impl_generics #after_where_clause {
                #( #after_variants ),*
            }
            #( #after_froms )*
        });
    }
}

