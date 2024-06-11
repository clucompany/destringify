
use core::{hint::unreachable_unchecked, ops::Range, panic, str::FromStr};

use alloc::{format, string::{String, ToString}, vec::Vec};
use proc_macro2::{Delimiter, Group, Literal, Span, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::ToTokens;
use core::str::Chars;
use crate::{exprs::literal::{str_to_literal, ExprLit}, trees::{null::make_null_ttree, result::TreeResult, sq_err, ttry}};

#[derive(Debug)]
enum SelectRule {
	StrOrSymbol(StrOrSymbol),
	RangeStrOrSymbol(StrOrSymbol, StrOrSymbol),
}

impl SelectRule {
	#[inline]
	fn into(self) -> (StrOrSymbol, Option<StrOrSymbol>) {
		match self {
			Self::StrOrSymbol(s) => (s, None),
			Self::RangeStrOrSymbol(a, b) => (a, b.into()),
		}
	}
}

impl From<char> for SelectRule {
	#[inline]
	fn from(value: char) -> Self {
		Self::StrOrSymbol(value.into())
	}
}

impl From<StrOrSymbol> for SelectRule {
	#[inline]
	fn from(value: StrOrSymbol) -> Self {
		Self::StrOrSymbol(value)
	}
}

impl From<String> for SelectRule {
	#[inline]
	fn from(value: String) -> Self {
		Self::StrOrSymbol(value.into())
	}
}

impl From<Range<String>> for SelectRule {
	#[inline]
	fn from(value: Range<String>) -> Self {
		Self::RangeStrOrSymbol(value.start.into(), value.end.into())
	}
}

impl From<Range<StrOrSymbol>> for SelectRule {
	#[inline]
	fn from(value: Range<StrOrSymbol>) -> Self {
		Self::RangeStrOrSymbol(value.start, value.end)
	}
}

impl From<Range<char>> for SelectRule {
	#[inline]
	fn from(value: Range<char>) -> Self {
		Self::RangeStrOrSymbol(value.start.into(), value.end.into())
	}
}

impl From<(StrOrSymbol, StrOrSymbol)> for SelectRule {
	#[inline]
	fn from((a, b): (StrOrSymbol, StrOrSymbol)) -> Self {
		Self::RangeStrOrSymbol(a, b)
	}
}

#[derive(Debug)]
enum StrOrSymbol {
	Str(String),
	OneSymbol(char),
}

#[cfg(test)]
#[test]
fn test_str_or_symbol() {
	{
		let en_rule0 = 'A';

		let mut end = 0;
		assert!(StrOrSymbol::from('A').search_char(
			en_rule0,
			|a| end += a.len_utf8(),
			|_a, _a2| {}, // skip

			|| true,
			|| false
		));
		assert_eq!(end, 1);
	}
	{
		let ru_rule1 = "АБСДА";

		let mut end = 0;
		assert!(StrOrSymbol::from('А').search_in_str(
			ru_rule1,
			|a| end += a.len_utf8(),
			|_a, _a2| {}, // skip

			|| true,
			|| false
		));
		assert_eq!(end, 2);

		end = 0;
		assert!(StrOrSymbol::from("БС".to_string()).search_in_str(
			ru_rule1,
			|a| end += a.len_utf8(),
			|_a, _a2| {}, // skip

			|| true,
			|| false
		));
		assert_eq!(end, 6);
	}
}

impl StrOrSymbol {
	#[inline]
	pub /*const*/ fn len(&self) -> usize {
		match self {
			Self::Str(a) => a.len(),
			Self::OneSymbol(..) => 1,
		}
	}

	#[inline]
	fn search_in_str<R>(
		&self,

		str: &str,
		mut counter: impl FnMut(char),
		novalid_next_inspect: impl FnMut(char, Option<char>),

		exists: impl FnOnce() -> R,
		noexists: impl FnOnce() -> R
	) -> R {
		let mut iter = str.chars();

		self.search(
			|| iter.next().inspect(|a| counter(*a)),
			novalid_next_inspect,

			exists,
			noexists
		)
	}

	#[inline]
	fn search_char<R>(
		&self,

		a: char,
		mut counter: impl FnMut(char),
		novalid_next_inspect: impl FnMut(char, Option<char>),

		exists: impl FnOnce() -> R,
		noexists: impl FnOnce() -> R
	) -> R {
		let mut a = Some(a);

		self.search(
			|| a.take().inspect(|a| counter(*a)),
			novalid_next_inspect,

			exists,
			noexists
		)
	}

	#[inline]
	fn search<R>(
		&self,

		mut next: impl FnMut() -> Option<char>,
		mut novalid_next_inspect: impl FnMut(char, Option<char>),

		exists: impl FnOnce() -> R,
		noexists: impl FnOnce() -> R
	) -> R {
		'begin: while let Some(ca) = next() {
			match self {
				Self::OneSymbol(a2) if &ca == a2 => return exists(),
				Self::OneSymbol(a2) => {
					novalid_next_inspect(ca, (*a2).into());
					continue 'begin;
				}, // skip
				Self::Str(ref astr) => {
					let mut astriter = astr.chars();

					match astriter.next() {
						Some(a) if a == ca => { // continue!
							if a.len_utf8() == astr.len() { // one symbol
								return exists();
							}
							for ca in astriter { // two/.. symbols
								let ca2 = next();

								if Some(ca) != ca2 { // double some, that's how it should be
								novalid_next_inspect(ca, ca2);
									continue 'begin;
								}
							}
							return exists();
						},
						ca2 @ (Some(..) | None) => {
							novalid_next_inspect(ca, ca2);
							continue 'begin;
						}, // skip
					}
				}
			}
		}

		noexists()
	}
}

impl From<char> for StrOrSymbol {
	#[inline]
	fn from(value: char) -> Self {
		Self::OneSymbol(value)
	}
}

impl From<String> for StrOrSymbol {
	#[inline]
	fn from(value: String) -> Self {
		Self::Str(value)
	}
}

fn decode_select_rules<R>(
	select0: &'_ Group,

	empty_end: impl FnOnce() -> TreeResult<R>,
	noempty_end: impl FnOnce() -> TreeResult<R>,

	mut next_rule: impl FnMut(SelectRule) -> TreeResult<()>,
) -> TreeResult<R> {
	let mut is_empty = true;
	let stream = select0.stream();
	let iter = stream.into_iter();

	let mut prev_value: Option<StrOrSymbol> = None;
	let mut exp_comma = false;
	let mut cpoint: Option<(Span, usize)> = None;
	for a in iter {
		let span = a.span();
		let value: StrOrSymbol = match a {
			TokenTree2::Group(group) => sq_err! {
				return [group.span()]: "Unsupported syntax."
			},
			TokenTree2::Ident(ident) => sq_err! {
				return [ident.span()]: "Unsupported syntax."
			},
			TokenTree2::Punct(a) if a.as_char() == '.' => {
				exp_comma = false;
				match cpoint {
					None => {
						cpoint = Some((span, 0));
					},
					Some((_, 1)) => sq_err! {
						return [span]: "Unsupported syntax."
					},
					Some((ref mut pspan, ref mut pcount)) => {
						*pspan = span;
						*pcount += 1
					},
				}
				continue;
			},
			TokenTree2::Punct(a) if cpoint.is_none() && a.as_char() == ',' => {
				if !exp_comma {
					sq_err! {
						return [a.span()]: "A single comma was expected."
					}
				}
				exp_comma = false;
				continue;
			},
			TokenTree2::Punct(a) if !exp_comma => {
				exp_comma = true;
				let value = a.as_char().into();

				match cpoint.take() {
					None => value,
					Some((cspan, ccount)) => match ccount {
						1 => {
							let value0 = match prev_value.take() {
								Some(a) => a,
								None => sq_err! {
									return [span]: "Unsupported syntax."
								},
							};

							is_empty = false;
							ttry!(next_rule((value0, value).into()));
							continue;
						},
						_c => sq_err! {
							return [cspan]: "Unsupported syntax."
						},
					}
				}
			},
			TokenTree2::Literal(lit) if !exp_comma => {
				exp_comma = true;
				let value = ttry!(ExprLit::try_new_with_fns(
					&lit.to_string(),

					|a| TreeResult::Ok(a.to_string()), // TODO, FIX DOUBLE_TO_STRING
					|e| e.into_tt_err(span).into()
				)).into();

				match cpoint.take() {
					None => value,
					Some((cspan, ccount)) => match ccount {
						1 => {
							let value0 = match prev_value.take() {
								Some(a) => a,
								None => sq_err! {
									return [span]: "Unsupported syntax."
								},
							};

							is_empty = false;
							ttry!(next_rule((value0, value).into()));
							continue;
						},
						_c => sq_err! {
							return [cspan]: "Unsupported syntax."
						},
					}
				}
			},
			TokenTree2::Punct(p) => sq_err! {
				return [p.span()]: "A comma was expected."
			},
			TokenTree2::Literal(lit) => sq_err! {
				return [lit.span()]: "A comma was expected."
			},
		};
		if let Some(value) = core::mem::replace(&mut prev_value, Some(value)) {
			is_empty = false;
			ttry!(next_rule(value.into()));
		}
	}
	if let Some(value) = prev_value {
		is_empty = false;
		ttry!(next_rule(value.into()));
	}
	if let Some((pspan, ..)) = cpoint {
		sq_err! {
			return [pspan]: "Unsupported syntax."
		}
	}

	match is_empty {
		true => empty_end(),
		false => noempty_end(),
	}
}


#[derive(Debug, Default)]
enum SegmenStrOrTreeVec<T> {
	#[default]
	Empty,

	V(Vec<T>),
	A(T),
}

impl<T> SegmenStrOrTreeVec<T> {
	#[inline]
	pub const fn empty() -> Self {
		Self::Empty
	}

	#[inline]
	pub const fn new(a: T) -> Self {
		Self::A(a)
	}

	pub fn push(&mut self, a2: T) {
		match self {
			Self::Empty => *self = Self::A(a2),
			Self::A(..) => {
				let a = match core::mem::replace(self, Self::Empty) {
					Self::A(a) => a,
					_ => unsafe { unreachable_unchecked() }
				};

				let mut v = Vec::with_capacity(2);
				v.push(a);
				v.push(a2);

				*self = Self::V(v);
			},
			Self::V(ref mut v) => {
				v.push(a2);
			},
		}
	}
}

/*enum SegmentTT<'a> {
	Str(&'a str),
	TT(TokenTree2)
}

impl<'a> Default for SegmentTT<'a> {
	fn default() -> Self {
		const EMPTY: &'static str = "";

		Self::Str(EMPTY)
	}
}*/

impl<'a> SegmenStrOrTreeVec<StrOrSymbol> {
	pub fn len(&self) -> usize {
		match self {
			SegmenStrOrTreeVec::Empty => 0,
			SegmenStrOrTreeVec::A(a) => a.len(),
			SegmenStrOrTreeVec::V(vec) => {
				let mut c = 0;
				for a in vec.iter() {
					c += a.len();
				}

				c
			}
		}
	}
}

pub fn macro_rule_select(
	group: &'_ Group,
) -> TreeResult<TokenTree2> {
	let (select0, expr0) = {
		let all_streams = group.stream();
		let mut iter = all_streams.into_iter();

		let select_group = {
			let select = match iter.next() {
				Some(TokenTree2::Group(a)) => a,
				_ => sq_err! {
					return [group.span()]: "Unsupported syntax."
				}
			};
			match iter.next() {
				Some(TokenTree2::Punct(a)) if a.as_char() == ':' => {},
				Some(TokenTree2::Punct(a)) => sq_err! {
					return [group.span()]: "Unsupported syntax."
				},
				_ => sq_err! {
					return [group.span()]: "Unsupported syntax."
				}
			}

			select
		};
		let expr0 = match iter.next() {
			Some(TokenTree2::Group(group)) => {
				let all_streams = group.stream();
				let mut iter = all_streams.into_iter();

				let expr0 = iter.next();
				if let Some(unk) = iter.next() {
					sq_err! {
						return [unk.span()]: "Unsupported syntax."
					}
				}

				expr0
			},
			_ => sq_err! {
				return [group.span()]: "Unsupported syntax."
			},
		};
		if let Some(unk) = iter.next() {
			sq_err! {
				return [unk.span()]: "Unsupported syntax."
			}
		}

		(select_group, expr0)
	};

	let span = group.span();
	let expr0 = match expr0 {
		Some(TokenTree2::Literal(lit)) => ttry!(ExprLit::try_new_with_fns(
			&lit.to_string(),
			|literal| TreeResult::Ok(literal.to_string()), // TODO, FIX DOUBLE_TO_STRING
			|e| e.into_tt_err(span).into()
		)),
		Some(a) => sq_err! {
			return [a.span()]: "Unsupported syntax."
		},
		None => return TreeResult::Ok(make_null_ttree(span)), // empty_stream
	};

	let mut tree: Vec<TokenStream2> = Vec::new();
	let result = decode_select_rules(
		&select0,
		|| TreeResult::Ok(false), // empty_stream
		|| TreeResult::Ok(true),
		|rule| {
			let (rule_start, rule_end) = rule.into();
			let mut iter = expr0.chars();

			let mut start = 0;
			let mut end = 0;
			'begin: loop {
				start = end;
				if !rule_start.search(
					|| iter.next().inspect(|a| end += a.len_utf8()),
					|_a, _a2| {}, // skip
					|| true,
					|| false
				) {
					break 'begin;
				}

				let rule_len = rule_start.len();
				let endpos = end-rule_len;
				let start = TokenStream2::from(TokenTree2::Literal(
					ttry!(str_to_literal(
						&expr0[start..endpos],
						span
					))
				));
				tree.push(start);
				{ // rule
					let expr = match syn::parse_str::<TokenStream2>(&expr0[end - rule_len..end]) {
						Ok(a) => a,
						Err(e) => {
							let err = format!("{:?}", e);
							sq_err! {
								return [span]: "Error parsing token_string: `", #err, "`"
							}
						}
					};
					tree.push(expr);
				}
			}
			//panic!("{:?}", rule);
			TreeResult::Ok(())
		}
	);
	drop(select0);

	match result {
		TreeResult::Ok(false) => TreeResult::Ok(make_null_ttree(span)),
		TreeResult::Ok(true) => { // noempty
			let mut group = Group::new(
				Delimiter::None,
				TokenStream2::from_iter(tree)
			);
			group.set_span(span);

			TreeResult::Ok(TokenTree2::Group(group))
		},
		TreeResult::Err(e) => TreeResult::Err(e)
	}

	/*match expr0 {
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
	}*/
}
