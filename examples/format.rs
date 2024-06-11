use std::fmt::Write;
macro_rules! format2 {
	// {} - Display
	[ @$str:ident [ {} $($next:tt)* ][ $arg_name: expr $(,$($args:tt)+)? $(,)?] ] => {
		write!($str, "{}", $arg_name).unwrap();

		format2! {
			@$str[ $($next)* ] [ $($args)* ]
		}
	};
	// {} - Display
	[ @$str:ident [ {} $($next:tt)* ][ $(,)? ] ] => {
		compile_error!("No arguments found");
	};
	// {:?} - Debug
	[ @$str:ident [ {:?} $($next:tt)* ][ $arg_name: expr $(,$($args:tt)+)? $(,)? ] ] => {
		write!($str, "{:?}", $arg_name).unwrap();

		format2! {
			@$str[ $($next)* ] [ $($args)* ]
		}
	};
	// {:?} - Debug
	[ @$str:ident [ {:?} $($next:tt)* ][ $(,)? ] ] => {
		compile_error!("No arguments found");
	};
	// {{}} - Push '{}'
	[ @$str:ident [ {{}} $($next:tt)* ][ $($args:tt)* ] ] => {
		$str.push_str("{}");

		format2! {
			@$str[ $($next)* ] [ $($args)* ]
		}
	};
	// .to_string() - Push 'your_text'
	[ @$str:ident [ $e: tt $($next:tt)* ][ $($args:tt)* ] ] => {
		$str.push_str(&$e.to_string());

		format2! {
			@$str[ $($next)* ] [ $($args)* ]
		}
	};
	// Unk next
	[ @$str:ident [ $c:tt $($next:tt)* ][ $($args:tt)* ] ] => {
		compile_error!(concat!("Unsupported block, '", stringify!($c), "'"));
	};
	// Exp argument
	[ @$str:ident [ $($next:tt)+ ][ /* args */ ] ] => {
		compile_error!("Each inclusion requires its own argument.");
	};

	[ @$str:ident [ $($next:tt)+ ][ $($all:tt)* ] ] => { // Unk
		compile_error!(concat!(stringify!($($next)*), "~", stringify!($($all)*)));
	};
	[ @$str:ident [][] ] => {}; // end

	// START_POINT
	[ $str: expr $(, $($all:tt)+)? $(,)? ] => {{
		let mut out = String::new();
		destringify::destringify! {
			format2! {
				@out[
					//#select!(["{:?}", "{}", '{'..":?}", '{'..'}']: $str)
					#select!(['{'..'}']: $str)
				] [ $($($all)+)? ]
			}
		}

		out
	}}
}

fn main() {
	let str = "test".to_string();
	let format = format2!("Test {} {{}}", str);
	assert_eq!(format, "Test test");
}
