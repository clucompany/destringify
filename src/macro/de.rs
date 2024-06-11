
use alloc::{format, string::ToString};
use proc_macro2::{Delimiter, Group, TokenStream as TokenStream2, TokenTree as TokenTree2};
use crate::{trees::{sq_err, result::TreeResult}, exprs::literal::ExprLit};

pub fn macro_rule_destringify(
	group: &'_ Group,
) -> TreeResult<TokenTree2> {
	let group = {
		let all_streams = group.stream();
		let mut iter = all_streams.into_iter();

		let expr0 = iter.next();
		if let Some(unk) = iter.next() {
			sq_err! {
				return [unk.span()]: "Unsupported syntax."
			}
		}

		match expr0 {
			Some(TokenTree2::Group(a)) => a,
			_ => sq_err! {
				return [group.span()]: "Unsupported syntax."
			},
		}
	};
	let group0 = {
		let all_streams = group.stream();
		let mut iter = all_streams.into_iter();

		let expr0 = iter.next();
		if let Some(unk) = iter.next() {
			sq_err! {
				return [unk.span()]: "Unsupported syntax."
			}
		}

		expr0
	};

	let span = group.span();
	match group0 {
		None => TreeResult::Ok({ // empty_stream
			let mut ngroup = Group::new(
				Delimiter::None,
				TokenStream2::new(),
			);
			ngroup.set_span(span);

			TokenTree2::Group(ngroup)
		}),
		Some(TokenTree2::Literal(lit)) => ExprLit::try_new_with_fns(
			&lit.to_string(),
			|literal| {
				let expr = match syn::parse_str::<TokenStream2>(literal.as_str()) {
					Ok(a) => a,
					Err(e) => {
						let err = format!("{:?}", e);
						sq_err! {
							return [span]: "Error parsing string: `", #err, "`"
						}
					}
				};
				let mut group = Group::new(
					Delimiter::None,
					expr
				);
				group.set_span(span);

				TreeResult::Ok(TokenTree2::Group(group))
			},
			|e| e.into_tt_err(span).into()
		),
		_ => sq_err! {
			return [span]: "Unsupported syntax."
		},
	}
}
