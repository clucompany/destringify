//Copyright 2024 #UlinProject Denis Kotlyarov (Денис Котляров)

//-----------------------------------------------------------------------------
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at

//	   http://www.apache.org/licenses/LICENSE-2.0

//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
// limitations under the License.
//-----------------------------------------------------------------------------

// or

//-----------------------------------------------------------------------------
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:

//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

#![allow(clippy::blocks_in_conditions)]
#![allow(clippy::to_string_trait_impl)]
#![allow(clippy::redundant_static_lifetimes)]
//#![no_std]

extern crate proc_macro;
extern crate alloc;

use alloc::string::ToString;
use proc_macro::TokenStream;
use proc_macro2::{Group, TokenStream as TokenStream2, TokenTree as TokenTree2};
use crate::{r#macro::{de::macro_rule_destringify, select::macro_rule_select}, trees::{null::make_null_ttree, replace::{support_replace_tree_in_group, support_replace_tree_in_stream}, result::TreeResult, search::SearchGroup, sq_err, ttry}};
use core::slice::IterMut;

/// Separate syntactic expressions of trees.
pub (crate) mod exprs {
	pub mod literal;
}

/// Code component of macros.
pub (crate) mod r#macro {
	pub mod de;
	pub mod select;
}

mod trees {
	#[macro_use]
	pub mod result;
	#[allow(clippy::single_component_path_imports)]
	pub (crate) use ttry;
	pub mod null;
	pub mod replace;
	pub mod search;

	#[macro_use]
	mod msq_err;
	#[allow(clippy::single_component_path_imports)]
	pub (crate) use sq_err;
}

/// The task of the function is to find a group with the desired macro
/// and perform useful work specific to the selected macro.
///
/// The design of this feature has been adapted to search for attachments.
fn search_destringify_and_replacegroup(
	iter: &'_ mut IterMut<'_, TokenTree2>,
) -> SearchGroup {
	'sbegin: while let Some(m_punct) = iter.next() {
		match m_punct {
			TokenTree2::Punct(punct) => {
				if punct.as_char() == '#' {
					if let Some(m_ident) = iter.next() {
						if let TokenTree2::Ident(ident) = m_ident {
							let (add_auto_break, macro_fn): (bool, fn(&'_ Group) -> TreeResult<TokenTree2>) = {
								let str_ident = ident.to_string();

								match str_ident.as_str() {
									"tt" => (false, macro_rule_destringify),
									"tt_and_break" => (true, macro_rule_destringify),
									"select" => (false, macro_rule_select),
									"select_and_break" => (true, macro_rule_select),
									"break" | "break_search_macro" => {
										/*
											Stop indexing after the given keyword. This saves resources.
										*/
										if let Some(m_punct2) = iter.next() {
											if let TokenTree2::Punct(punct2) = m_punct2 {
												if punct2.as_char() == ';' {
													*m_ident = make_null_ttree(m_ident.span());
													*m_punct = make_null_ttree(m_punct.span());
													*m_punct2 = make_null_ttree(m_punct2.span());

													return SearchGroup::Break;
												}
											}
										}

										sq_err! {
											return [ident.span()]: "`;` was expected."
										}
									},

									_ => sq_err! {
										return [ident.span()]: "Unknown macro, expected `tt`, `tt_and_break`, `select`, `select_and_break`, `break`, `break_search_macro`."
									},
								}
							};

							if let Some(m_punct2) = iter.next() {
								if let TokenTree2::Punct(punct2) = m_punct2 {
									if punct2.as_char() == '!' {
										if let Some(m_group) = iter.next() {
											if let TokenTree2::Group(group) = m_group {
												let result = ttry!( macro_fn(group) );

												*m_ident = make_null_ttree(m_ident.span());
												*m_punct = make_null_ttree(m_punct.span());
												*m_punct2 = make_null_ttree(m_punct2.span());
												*m_group = result;

												if add_auto_break {
													return SearchGroup::Break;
												}

												continue 'sbegin;
											}
										}
									}
									// autoskip
								}
							}

							sq_err! {
								return [ident.span()]: "Unknown macro, expected #de!(\"test\"), #de_and_break!(\"test\"), #break_search_macro;"
							}
						}
					}
				}
			}
			// If this is a group, then you need to go down inside the
			// group and look for the necessary macros there.
			TokenTree2::Group(group) => match support_replace_tree_in_group(
				group,
				|mut iter| search_destringify_and_replacegroup(&mut iter),
			) {
				SearchGroup::Break => continue 'sbegin,
				result @ SearchGroup::Error(..) => return result,
			},
			_ => {},
		}
	}

	SearchGroup::Break
}

#[proc_macro]
pub fn destringify(input: TokenStream) -> TokenStream {
	let mut tt: TokenStream2 = input.into();

	match support_replace_tree_in_stream(
		&mut tt,
		|mut iter| search_destringify_and_replacegroup(&mut iter)
	) {
		SearchGroup::Error(e) => e.into(),
		SearchGroup::Break => tt.into(),
	}
}
