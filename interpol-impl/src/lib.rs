extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Ident, Span, TokenTree};
use proc_macro_hack::proc_macro_hack;
use quote::quote;
use std::iter::FromIterator;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, parse_quote, parse_str, Expr, LitStr, Token};

fn rewrite_site(e: proc_macro2::TokenStream, span: Span) -> proc_macro2::TokenStream {
    proc_macro2::TokenStream::from_iter(e.into_iter().map(|tt| match tt {
        TokenTree::Ident(ident) => TokenTree::Ident(Ident::new(&ident.to_string(), span)),
        TokenTree::Group(group) => TokenTree::Group(Group::new(
            group.delimiter(),
            rewrite_site(group.stream(), span),
        )),
        tt => tt,
    }))
}

fn gen(macro_call: impl Fn(String, Vec<Expr>) -> TokenStream, s: LitStr) -> TokenStream {
    let call_site = s.span();
    let s = s.value();

    let mut fmt_string = String::new();

    let mut args: Vec<Expr> = vec![];

    let s: Vec<char> = s.chars().collect();
    let mut s = &s[0..];

    while !s.is_empty() {
        if s[0] == '}' {
            if s.len() >= 2 && s[1] == '}' {
                fmt_string.push_str("}}");
                s = &s[2..];
                continue;
            } else {
                panic!("incorrect occurence of `}`");
            }
        }

        if s[0] != '{' {
            fmt_string.push(s[0]);
            s = &s[1..];
            continue;
        }

        // escaping ("{{")
        if s.len() >= 2 && s[1] == '{' {
            fmt_string.push_str("{{");
            s = &s[2..];
            continue;
        }

        // process interpolation
        s = &s[1..];

        let mut expr = vec![];
        let mut level = 1;

        // find corresponding '}'
        while !s.is_empty() {
            let c = s[0];
            s = &s[1..];

            if c == '}' {
                level -= 1;
                if level == 0 {
                    break;
                }
            } else if c == '{' {
                level += 1;
            }

            expr.push(c);
        }

        let mut manip_ix = None;

        // find last ':' except a part of "::"
        for i in 0..expr.len() {
            if !(i >= 1 && expr[i - 1] == ':')
                && expr[i] == ':'
                && !(i + 1 < expr.len() && expr[i + 1] == ':')
            {
                manip_ix = Some(i);
            }
        }

        let (expr, manip) = if let Some(manip_ix) = manip_ix {
            (&expr[..manip_ix], &expr[manip_ix..])
        } else {
            (&expr[..], &[] as &[char])
        };

        let expr: String = expr.iter().collect();
        let manip: String = manip.iter().collect();

        fmt_string.push_str(&format!("{{{}}}", manip));

        let expr: Expr = parse_str(&expr).expect(&format!("Failed to parse: `{}`", &expr));

        let expr = rewrite_site(quote! { #expr }, call_site);
        let expr: Expr = parse_quote! { #expr };

        args.push(expr);
    }

    macro_call(fmt_string, args)
}

#[proc_macro_hack]
pub fn format(input: TokenStream) -> TokenStream {
    gen(
        |fmt_string, args| quote!(std::format!(#fmt_string #(, #args )*)).into(),
        parse_macro_input!(input as LitStr),
    )
}

#[proc_macro_hack]
pub fn print(input: TokenStream) -> TokenStream {
    gen(
        |fmt_string, args| quote!(std::print!(#fmt_string #(, #args )*)).into(),
        parse_macro_input!(input as LitStr),
    )
}

#[proc_macro_hack]
pub fn println(input: TokenStream) -> TokenStream {
    gen(
        |fmt_string, args| quote!(std::println!(#fmt_string #(, #args )*)).into(),
        parse_macro_input!(input as LitStr),
    )
}

#[proc_macro_hack]
pub fn eprint(input: TokenStream) -> TokenStream {
    gen(
        |fmt_string, args| quote!(std::eprint!(#fmt_string #(, #args )*)).into(),
        parse_macro_input!(input as LitStr),
    )
}

#[proc_macro_hack]
pub fn eprintln(input: TokenStream) -> TokenStream {
    gen(
        |fmt_string, args| quote!(std::eprintln!(#fmt_string #(, #args )*)).into(),
        parse_macro_input!(input as LitStr),
    )
}

fn parse_write_arg(input: TokenStream) -> (Expr, LitStr) {
    let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
    let args = parser.parse(input).unwrap();
    if args.len() != 2 {
        panic!("too many arguments");
    }
    let mut it = args.iter();
    let dst = it.next().unwrap();
    let fmt = it.next().unwrap();
    (dst.clone(), parse_quote! { #fmt })
}

#[proc_macro_hack]
pub fn write(input: TokenStream) -> TokenStream {
    let (dst, fmt) = parse_write_arg(input);
    gen(
        |fmt_string, args| quote!(std::write!(#dst, #fmt_string #(, #args )*)).into(),
        fmt,
    )
}

#[proc_macro_hack]
pub fn writeln(input: TokenStream) -> TokenStream {
    let (dst, fmt) = parse_write_arg(input);
    gen(
        |fmt_string, args| quote!(std::writeln!(#dst, #fmt_string #(, #args )*)).into(),
        fmt,
    )
}
