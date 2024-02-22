use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{__private::Span, quote};
use syn::{parse_macro_input, parse_quote, ItemFn, ItemImpl, ItemStruct};

#[proc_macro_attribute]
pub fn monoruby_builtin(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(item as ItemFn);
    let base_name = ast.sig.ident.clone();
    let func_name = format!("__{}", base_name);
    let wrapped = Ident::new(&func_name, Span::call_site());
    ast.sig.ident = wrapped.clone();

    let ast2: ItemFn = parse_quote!(
        #[allow(improper_ctypes_definitions)]
        pub extern "C" fn #base_name(
            vm: &mut Executor,
            globals: &mut Globals,
            lfp: Lfp,
        ) -> Option<Value> {
            match #wrapped(vm, globals, lfp) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            }
        }
    );
    let gen = quote! {
        #ast

        #ast2
    };

    gen.into()
}

#[proc_macro_attribute]
pub fn monoruby_object(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as ItemStruct);
    let base = ast.ident.clone();
    let inner = Ident::new(&format!("{base}Inner"), Span::call_site());
    let as_ref = Ident::new(
        &format!("as_{}", base.to_string().to_lowercase()),
        Span::call_site(),
    );
    let as_ref_mut = Ident::new(
        &format!("as_{}_mut", base.to_string().to_lowercase()),
        Span::call_site(),
    );
    let objkind = Ident::new(&base.to_string().to_uppercase(), Span::call_site());

    let auto_deref: ItemImpl = parse_quote!(
        impl std::ops::Deref for #base {
            type Target = #inner;
            fn deref(&self) -> &Self::Target {
                self.0.#as_ref()
            }
        }
    );

    let auto_deref_mut: ItemImpl = parse_quote!(
        impl std::ops::DerefMut for #base {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.0.#as_ref_mut()
            }
        }
    );

    let auto_from: ItemImpl = parse_quote!(
        impl std::convert::From<Value> for #base {
            fn from(v: Value) -> Self {
                assert_eq!(ObjKind::#objkind, v.rvalue().ty());
                Self(v)
            }
        }

    );

    let gen = quote! {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy)]
        #ast

        #auto_deref
        #auto_deref_mut
        #auto_from

        impl std::convert::Into<Value> for #base {
            fn into(self) -> Value {
                self.0
            }
        }

        impl alloc::GC<RValue> for #base {
            fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
                self.0.mark(alloc)
            }
        }

        impl #base {
            pub fn as_ptr(self) -> *mut RValue {
                self.0.id() as _
            }
        }
    };

    gen.into()
}
