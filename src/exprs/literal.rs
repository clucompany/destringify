
use core::{borrow::Borrow, fmt::Debug, ops::Deref, str::FromStr};
use alloc::{borrow::ToOwned, format, string::{String, ToString}};
use proc_macro2::{LexError, Literal, Span, TokenStream as TokenStream2};
use crate::{sq_err, trees::result::TreeResult};

/// The actual literal expression, written as "./test".
#[repr(transparent)]
pub struct ExprLit {
	data: str
}

impl PartialEq<ExprLit> for ExprLit {
	#[inline(always)]
	fn eq(&self, other: &ExprLit) -> bool {
		PartialEq::eq(self.as_str(), other.as_str())
	}
}

impl PartialEq<str> for ExprLit {
	#[inline(always)]
	fn eq(&self, other: &str) -> bool {
		PartialEq::eq(self.as_str(), other)
	}
}

/// Errors received in case of a
/// literal expression parsing error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprLitTryNewErr {
	/// More characters were expected to be parsed.
	ExpLen {
		current: usize,
		exp: usize,
	},

	/// A double closing of quotes was expected.
	ExpQuotes,
}

impl ExprLitTryNewErr {
	/// Convert an error to a syntax tree.
	#[inline]
	pub fn into_tt_err(self, span: Span) -> TokenStream2 {
		match self {
			Self::ExpLen { current, exp } => sq_err! {
				[span]: "More char expected, current: ", #current, "exp: {}", #exp, "."
			},
			Self::ExpQuotes => sq_err! {
				[span]: "Double quotes were expected."
			}
		}
	}
}

impl Deref for ExprLit {
	type Target = str;

	#[inline(always)]
	fn deref(&self) -> &Self::Target {
		self.as_str()
	}
}

impl Debug for ExprLit {
	#[inline(always)]
	fn fmt(&self, f: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
		Debug::fmt(self as &str, f)
	}
}

impl ToOwned for ExprLit {
	type Owned = String;

	#[inline]
	fn to_owned(&self) -> Self::Owned {
		self.as_str().to_owned()
	}
}

impl Borrow<ExprLit> for String {
	#[inline]
	fn borrow(&self) -> &ExprLit {
		unsafe { ExprLit::unchecked(self.as_str()) } // TODO
	}
}

impl ToString for ExprLit {
	#[inline]
	fn to_string(&self) -> String {
		self.as_str().to_string()
	}
}

impl ExprLit {
	/// Creating `ExprLit` without clipping.
	#[inline]
	const fn __new(a: &str) -> &ExprLit {
		// It is safe, there is no other way than to "transmute", "box"
		// to create a dimensionless structure.
		unsafe { &*(a as *const _ as *const ExprLit) }
	}

	/// Creating `ExprLit` without clipping.
	#[inline(always)]
	pub const unsafe fn unchecked(a: &str) -> &Self {
		Self::__new(a)
	}

	/// Create an `ExprLit` from the expression `"test"` and return it.
	#[allow(dead_code)]
	#[inline]
	pub fn try_new(a: &str) -> Result<&ExprLit, ExprLitTryNewErr> {
		Self::try_new_with_fns(
			a,
			Ok,
			Err
		)
	}

	/// Create an `ExprLit` from the expression `"test"` and return it.
	pub fn try_new_with_fns<'a, R>(a: &'a str, next: impl FnOnce(&'a ExprLit) -> R, err: impl FnOnce(ExprLitTryNewErr) -> R) -> R {
		let a_array = a.as_bytes();

		let len = a_array.len();
		if len < 2 {
			return err(ExprLitTryNewErr::ExpLen {
				current: len,
				exp: 2,
			});
		}
		debug_assert!({
			#[allow(clippy::get_first)] // why?, this is done to be completely analogous to an unsafe function.
			a_array.get(0).is_some()
		});
		debug_assert!({
			#[allow(clippy::get_first)] // why?, this is done to be completely analogous to an unsafe function.
			a_array.get(len -1).is_some()
		});
		/*
			This is safe, the extra necessary checks are done in a separate `if` above.
		*/
		match unsafe { (a_array.get_unchecked(0), a_array.get_unchecked(len -1)) } {
			(b'"', b'"') => /* line */ {}, // GOOD,
			(b'\'', b'\'') if len != 3 => { /* one_symbol */
				/*
					We exclude the possibility of using `'` as more
					than one character.
				*/
				return err(
					ExprLitTryNewErr::ExpLen {
						current: len,
						exp: 3
					}
				);
			},
			(b'\'', b'\'') => /* line */ {}, // GOOD,
			_ => return err(ExprLitTryNewErr::ExpQuotes),
		}

		next({
			debug_assert!(a.get(1..len-1).is_some());

			// It's safe, checks are done above (above `debug_assert`).
			let str = unsafe {
				a.get_unchecked(1..len-1)
			};
			Self::__new(str)
		})
	}

	#[inline(always)]
	/// Returns `true` if self has a length of zero bytes.
	pub const fn is_empty(&self) -> bool {
		self.data.is_empty()
	}

	/// Getting a string of actual data.
	#[inline(always)]
	pub const fn as_str(&self) -> &str {
		&self.data
	}
}

/// Converts a string to a literal.
///
/// # Arguments
///
/// * `str` - The string to convert.
/// * `span` - The span of the string in the source code.
///
/// # Returns
///
/// A `TreeResult` containing the created literal, or an error if the string cannot be parsed as a literal.
pub fn str_to_literal(istr: &str, span: Span) -> TreeResult<Literal> {
	let mut str = String::with_capacity(istr.len() + 2);
	str.push('\"');
	str.push_str(istr);
	str.push('\"');

	match Literal::from_str(&str) {
		Ok(mut a) => {
			a.set_span(span);

			TreeResult::Ok(a)
		},
		Err(e) => {
			let err = format!("{:?}", e);

			sq_err! {
				[span]: "Unsupported syntax, err: '", #err, "'"
			}
		}
	}
}

#[cfg(test)]
#[test]
fn test_literal() {
	/*
		Checking the correct operation of ExprLit.
	*/
	assert_eq!(
		ExprLit::try_new(""),
		Err(ExprLitTryNewErr::ExpLen { current: 0, exp: 2 })
	);
	assert_eq!(
		ExprLit::try_new("\""),
		Err(ExprLitTryNewErr::ExpLen { current: 1, exp: 2 })
	);
	assert_eq!(
		ExprLit::try_new("\"\""),
		Ok(ExprLit::__new("")),
	);
	assert_eq!(
		ExprLit::try_new("'\\'"),
		Ok(ExprLit::__new("\\")),
	);
}
