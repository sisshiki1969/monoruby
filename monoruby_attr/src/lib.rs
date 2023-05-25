use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{__private::Span, quote};
use syn::{parse_macro_input, parse_quote, ItemFn};

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
            lfp: LFP,
            arg: Arg,
            len: usize,
        ) -> Option<Value> {
            match #wrapped(vm, globals, lfp, arg, len) {
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
