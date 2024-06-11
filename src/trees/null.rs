
use proc_macro2::{Delimiter, Group, Span, TokenStream as TokenStream2, TokenTree as TokenTree2};

/// Create an empty tree that does not affect the target tree.
///
/// Typically used to avoid deleting tree elements and replacing them with voids.
#[inline]
pub /*const*/ fn make_null_ttree(span: Span) -> TokenTree2 {
	let mut ngroup = Group::new(
		Delimiter::None,
		TokenStream2::new(),
	);
	ngroup.set_span(span);

	TokenTree2::Group(ngroup)
}
