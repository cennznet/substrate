#[macro_export]
macro_rules! impl_outer_error {
	(
		$(#[$attr:meta])*
		pub enum $name:ident for $runtime:ident {
			$( $module:ident $( < $module_instance:ident > )? ),* $(,)?
		}
	) => {
		$crate::impl_outer_error! {
			$(#[$attr])*
			pub enum $name for $runtime where system = system {
				$( $module $( < $module_instance > )?, )*
			}
		}
	};
	(
		$(#[$attr:meta])*
		pub enum $name:ident for $runtime:ident where system = $system:ident {
			$( $module:ident $( < $module_instance:ident > )? ),* $(,)?
		}
	) => {
		$crate::impl_outer_error!(
			$( #[$attr] )*;
			$name;
			$runtime;
			$system;
			$( $module $( < $module_instance > )?, )*
		);
	};

	// The main macro expansion that actually renders the Error enum.

	(
		$(#[$attr:meta])*;
		$name:ident;
		$runtime:ident;
		$system:ident;
		$( $module:ident $( < $module_instance:ident > )? ,)*
	) => {
		$crate::paste::item! {
		// Workaround for https://github.com/rust-lang/rust/issues/26925 . Remove when sorted.
		#[derive(Clone, PartialEq, Eq, $crate::codec::Encode)]
		#[cfg_attr(feature = "std", derive(Debug))]
		$(#[$attr])*
		#[allow(non_camel_case_types)]
		pub enum $name {
			system($system::Error),
			$(
				[< $module $(_ $module_instance )? >]  ($module::Error),
			)*
		}
		}

		impl From<$system::Error> for $name {
			fn from(err: $system::Error) -> Self {
				$name::system(err)
			}
		}

		impl From<&'static str> for $name {
			fn from(err: &'static str) -> Self {
				$name::system($system::Error::Other(err))
			}
		}

		impl $crate::Printable for $name {
			fn print(self) {
				$crate::print("Error:");
				let err = Into::<$crate::runtime_primitives::DispatchError>::into(self);
				$crate::print(err.module as u64);
				$crate::print(err.error as u64);
				if let Some(msg) = err.message {
					$crate::print(msg);
				}
			}
		}

		impl $crate::rstd::convert::TryInto<$system::Error> for $name {
			type Error = Self;
			fn try_into(self) -> $crate::dispatch::result::Result<$system::Error, Self::Error> {
				if let $name::system(err) = self {
					Ok(err)
				} else {
					Err(self)
				}
			}
		}

		impl Into<$crate::runtime_primitives::DispatchError> for $name {
			fn into(self) -> $crate::runtime_primitives::DispatchError {
				match self {
					$name::system(ref err) => match err {
						$system::Error::Other(msg) =>
							$crate::runtime_primitives::DispatchError {
								module: 0,
								error: 0,
								message: Some(msg),
							},
						_ => $crate::runtime_primitives::DispatchError {
								module: 0,
								error: Into::<u8>::into(err),
								message: None,
							},
					},
					$(
						$name::$module(ref err) => match err {
							$module::Error::Other(msg) =>
								$crate::runtime_primitives::DispatchError {
									module: $crate::codec::Encode::using_encoded(&self, |s| s[0]),
									error: 0,
									message: Some(msg),
								},
							_ => $crate::runtime_primitives::DispatchError {
									module: $crate::codec::Encode::using_encoded(&self, |s| s[0]),
									error: Into::<u8>::into(err),
									message: None,
								},
						},
					)*
				}
			}
		}

		$(
			impl From<$module::Error> for $name {
				fn from(err: $module::Error) -> Self {
					$name::$module(err)
				}
			}

			impl $crate::rstd::convert::TryInto<$module::Error> for $name {
				type Error = Self;
				fn try_into(self) -> $crate::dispatch::result::Result<$module::Error, Self::Error> {
					if let $name::$module(err) = self {
						Ok(err)
					} else {
						Err(self)
					}
				}
			}
		)*
	}
}


#[macro_export]
macro_rules! decl_error {
	(
		$(#[$attr:meta])*
		pub enum Error {
			$(
				$(#[$variant_attr:meta])*
				$name:ident
			),*
			$(,)?
		}
	) => {
		// Workaround for https://github.com/rust-lang/rust/issues/26925 . Remove when sorted.
		#[derive(Clone, PartialEq, Eq, $crate::codec::Encode)]
		#[cfg_attr(feature = "std", derive(Debug))]
		$(#[$attr])*
		#[allow(non_camel_case_types)]
		pub enum Error {
			Other(&'static str),
			$(
				$(#[$variant_attr])*
				$name
			),*
		}

		impl From<&Error> for u8 {
			fn from(err: &Error) -> u8 {
				match err {
					Error::Other(_) => 0,
					_ => $crate::codec::Encode::using_encoded(err, |s| s[0]),
				}
			}
		}

		impl From<&'static str> for Error {
			fn from(val: &'static str) -> Error {
				Error::Other(val)
			}
		}
	}
}
