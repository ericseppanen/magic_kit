use heck::ToSnakeCase;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token;
use syn::{
    parse2, parse_macro_input, Field, Fields, FieldsNamed, GenericArgument, ItemStruct,
    PathArguments, TraitBound, Type, TypeParamBound, TypePath,
};

// Given "HashMap", return "hash_map"
fn snake_ident(ident: &Ident) -> Ident {
    let ident = ident.to_string();
    let snake_ident = ident.to_snake_case();
    Ident::new(&snake_ident, Span::call_site())
}

// Given a Type, extract its last path component and then snake_case it.
// E.g. std::collections::HashMap -> hash_map
// Will panic if the type is not a well-formed TypePath.
fn make_snake_ident(ty: &Type) -> Ident {
    match ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = &path.segments.last().expect("TypePath with 0 segments");
            snake_ident(&segment.ident)
        }
        _ => panic!("gen_field failed extracting TypePath"),
    }
}

// from Ident{A} to "a: FreezeBox<Arc<A>>"
fn gen_field(ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);
    quote! {
        #field_name: ::freezebox::FreezeBox<::std::sync::Arc<#ty>>,
    }
}

// from Ident{A} to "a: <Arc<A>"
fn gen_field_arc(ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);
    quote! {
        #field_name: ::std::sync::Arc<#ty>,
    }
}

// from Ident{A} to "a: Default::default(),"
fn gen_default_init(ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);
    quote! {
        #field_name: Default::default(),
    }
}

// Emit the field's ident with a trailing comma.
// This is for doing struct init, i.e.
// MyStruct { a, b, }
fn gen_user_init(field: &Field) -> TokenStream {
    let ident = &field.ident;
    quote! {
        #ident,
    }
}

// from Ident{A} to "a: KitGet::<A>::kitget(__kit),"
fn gen_from_init(ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);
    quote! {
        #field_name: ::magic_kit::KitGet::<#ty>::kitget(__kit),
    }
}

// Emit the field's name and type, for defining a struct.
// This will be used to construct a function signature,
// so don't emit a trailing comma.
fn gen_user_param(field: &Field) -> TokenStream {
    let ident = &field.ident;
    let ty = &field.ty;
    quote! {
        #ident : #ty
    }
}

// Emit an imple of KitGet<T> for a lazy struct.
fn gen_kitget(name: &Ident, ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);

    quote! {
        #[automatically_derived]
        impl ::magic_kit::KitGet<#ty> for #name
        {
            fn kitget(&self) -> ::std::sync::Arc<#ty> {
                if !self.#field_name.is_initialized() {
                    let val = ::magic_kit::Initializer::<#name>::create(&self);
                    let val = ::std::sync::Arc::new(val);
                    self.#field_name.lazy_init(val);
                }
                ::std::sync::Arc::clone(&self.#field_name)
            }
        }
    }
}

// Emit an impl of KitGet<T> for a concrete struct.
fn gen_kitget_arc(name: &Ident, ty: &Type) -> TokenStream {
    let field_name = make_snake_ident(ty);

    quote! {
        #[automatically_derived]
        impl ::magic_kit::KitGet<#ty> for #name
        {
            fn kitget(&self) -> ::std::sync::Arc<#ty> {
                ::std::sync::Arc::clone(&self.#field_name)
            }
        }
    }
}

// Emit an impl of KitGet<T> for a user field in a lazy struct.
fn gen_kitget_user_arc(name: &Ident, field_name: &Ident, ty: &Type) -> TokenStream {
    quote! {
        #[automatically_derived]
        impl ::magic_kit::KitGet<#ty> for #name
        {
            fn kitget(&self) -> ::std::sync::Arc<#ty> {
                ::std::sync::Arc::clone(&self.#field_name)
            }
        }
    }
}

fn gen_kitgen_bound(name: &AttrList) -> Punctuated<TypeParamBound, token::Add> {
    let iter = name.iter().map(|ty| {
        let s = quote! { ::magic_kit::KitGet<#ty> };
        let bound: TraitBound = parse2(s).unwrap();
        TypeParamBound::Trait(bound)
    });

    Punctuated::from_iter(iter)
}

// If a Type starts with "Arc" then return the inner type.
// Otherwise return None.
fn strip_arc(ty: &Type) -> Option<&Type> {
    let path = match ty {
        Type::Path(TypePath { path, .. }) => path,
        _ => return None,
    };
    let segment = match path.segments.first() {
        Some(segment) => segment,
        _ => return None,
    };
    let ident = &segment.ident;
    if ident != "Arc" {
        return None;
    }
    let angle_args = match &segment.arguments {
        PathArguments::AngleBracketed(angle_args) => angle_args,
        _ => return None,
    };
    if angle_args.args.len() != 1 {
        return None;
    }
    match angle_args.args.first() {
        Some(GenericArgument::Type(ty)) => Some(ty),
        _ => None,
    }
}

// If the input is a field with an ident and a type that's
// Arc<T>, return its (ident, T).
fn arc_field(f: &Field) -> Option<(&Ident, &Type)> {
    match &f.ident {
        Some(ident) => strip_arc(&f.ty).map(|ty| (ident, ty)),
        None => None,
    }
}

// This is the type that macro attributes are parsed into.
type AttrList = Punctuated<Type, token::Comma>;

fn parse_attr(attr: proc_macro::TokenStream) -> AttrList {
    use syn::parse::Parser;
    let attr_parser = AttrList::parse_terminated;
    attr_parser.parse(attr).expect("attr parser failed")
}

#[proc_macro_attribute]
pub fn lazy_kit(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    //eprintln!("ATTR: {:#?}", attr);
    let generated_names = parse_attr(attr);
    let item: ItemStruct = parse_macro_input!(item);
    //eprintln!("ITEM: {:#?}", item);

    let struct_name = &item.ident;
    let vis = &item.vis;

    let generated_fields = generated_names.iter().map(gen_field);
    let generated_inits = generated_names.iter().map(gen_default_init);

    let user_fields = match &item.fields {
        Fields::Named(FieldsNamed { named, .. }) => named,
        _ => panic!("failed to extract named fields"),
    };
    let user_params = user_fields.iter().map(gen_user_param);
    let user_inits = user_fields.iter().map(gen_user_init);
    // User fields "Arc<T>" represented as (name, T)
    let user_arc_fields = user_fields.iter().filter_map(arc_field);

    let generated_kitget = generated_names.iter().map(|ty| gen_kitget(struct_name, ty));
    let user_arc_kitget =
        user_arc_fields.map(|(name, ty)| gen_kitget_user_arc(struct_name, name, ty));

    let expanded = quote! {
        #[automatically_derived]
        #vis struct #struct_name {
            #(#generated_fields)*
            #user_fields
        }

        #[automatically_derived]
        impl #struct_name {
            pub fn new( #(#user_params),* ) -> Self {
                Self {
                    #(#generated_inits)*
                    #(#user_inits)*
                }
            }

            fn get<T>(&self) -> ::std::sync::Arc<T>
            where
                Self: ::magic_kit::KitGet<T>,
            {
                ::magic_kit::KitGet::<T>::kitget(self)
            }
        }

        #(#generated_kitget)*
        #(#user_arc_kitget)*
    };
    expanded.into()
}

#[proc_macro_attribute]
pub fn magic_kit(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    //eprintln!("ATTR: {:#?}", attr);
    let generated_names = parse_attr(attr);
    let item: ItemStruct = parse_macro_input!(item);
    //eprintln!("ITEM: {:#?}", item);

    let struct_name = &item.ident;
    let vis = &item.vis;

    let generated_fields = generated_names.iter().map(gen_field_arc);
    //let generated_inits = generated_names.iter().map(|i| gen_default_init(i));

    let user_fields = match &item.fields {
        Fields::Named(FieldsNamed { named, .. }) => named,
        _ => panic!("failed to extract named fields"),
    };
    //let user_params = user_fields.iter().map(|f| gen_user_param(f));
    //let user_inits = user_fields.iter().map(|f| gen_user_init(f));

    let generated_kitget = generated_names
        .iter()
        .map(|ty| gen_kitget_arc(struct_name, ty));

    let generated_bound = gen_kitgen_bound(&generated_names);
    let from_inits = generated_names.iter().map(gen_from_init);

    let expanded = quote! {
        #[automatically_derived]
        #vis struct #struct_name {
            #(#generated_fields)*
            // FIXME: user fields won't work here if we want to implement From
            #user_fields
        }

        #[automatically_derived]
        impl #struct_name {
            fn get<T>(&self) -> ::std::sync::Arc<T>
            where
                Self: ::magic_kit::KitGet<T>,
            {
                ::magic_kit::KitGet::<T>::kitget(self)
            }
        }

        /*#[automatically_derived]
        impl #struct_name {
            pub fn new( #(#user_params),* ) -> Self {
                Self {
                    #(#generated_inits)*
                    #(#user_inits)*
                }
            }
        }*/

        #(#generated_kitget)*

        impl<__K> From<&__K> for #struct_name
        where
            __K: #generated_bound,
        {
            fn from(__kit: &__K) -> Self {
                Self {
                    #(#from_inits)*
                }
            }
        }

    };
    expanded.into()
}
