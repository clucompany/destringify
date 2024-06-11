
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

/// Events for the `search_destringify_and_replacegroup` function.
pub (crate) enum SearchGroup {
	/// Abort the group search 
	/// and display an error.
	Error(TokenStream2),
	
	/// The trees are over, 
	/// we need to stop the search.
	Break,
}

impl From<TokenStream2> for SearchGroup {
	#[inline(always)]
	fn from(e: TokenStream2) -> Self {
		Self::Error(e)
	}
}

impl From<TokenStream> for SearchGroup {
	#[inline(always)]
	fn from(e: TokenStream) -> Self {
		TokenStream2::from(e).into()
	}
}