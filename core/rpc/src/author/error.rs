// Copyright 2017-2019 Parity Technologies (UK) Ltd.
// This file is part of Substrate.

// Substrate is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Substrate is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Substrate.  If not, see <http://www.gnu.org/licenses/>.

//! Authoring RPC module errors.

use client;
use transaction_pool::txpool;
use crate::rpc;

use crate::errors;

/// Author RPC Result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Author RPC errors.
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum Error {
	/// Client error.
	Client(client::error::Error),
	/// Transaction pool error,
	Pool(txpool::error::Error),
	/// Incorrect extrinsic format.
	#[display(fmt="Invalid extrinsic format")]
	BadFormat,
	/// Verification error
	#[display(fmt="Extrinsic verification error: {}", "_0.description()")]
	Verification(Box<::std::error::Error + Send>),
}

impl std::error::Error for Error {}

/// Base code for all authorship errors.
const BASE_ERROR: i64 = 1000;
/// Extrinsic has an invalid format.
const BAD_FORMAT: i64 = BASE_ERROR + 1;
/// Error during transaction verification in runtime.
const VERIFICATION_ERROR: i64 = BASE_ERROR + 2;

/// Pool rejected the transaction as invalid
const POOL_INVALID_TX: i64 = BASE_ERROR + 10;
/// Cannot determine transaction validity.
const POOL_UNKNOWN_VALIDITY: i64 = POOL_INVALID_TX + 1;
/// The transaction is temporarily banned.
const POOL_TEMPORARILY_BANNED: i64 = POOL_INVALID_TX + 2;
/// The transaction is already in the pool
const POOL_ALREADY_IMPORTED: i64 = POOL_INVALID_TX + 3;
/// Transaction has too low priority to replace existing one in the pool.
const POOL_TOO_LOW_PRIORITY: i64 = POOL_INVALID_TX + 4;
/// Including this transaction would cause a dependency cycle.
const POOL_CYCLE_DETECTED: i64 = POOL_INVALID_TX + 5;
/// The transaction was not included to the pool because of the limits.
const POOL_IMMEDIATELY_DROPPED: i64 = POOL_INVALID_TX + 6;

impl From<Error> for rpc::Error {
	fn from(e: Error) -> Self {
		use txpool::error::{Error as PoolError};

		match e {
			Error::BadFormat => rpc::Error {
				code: rpc::ErrorCode::ServerError(BAD_FORMAT),
				message: "Extrinsic has invalid format.".into(),
				data: None,
			},
			Error::Verification(e) => rpc::Error {
				code: rpc::ErrorCode::ServerError(VERIFICATION_ERROR),
				message: e.description().into(),
				data: Some(format!("{:?}", e).into()),
			},
			Error::Pool(PoolError::InvalidTransaction(code)) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_INVALID_TX),
				message: "Invalid Transaction".into(),
				data: Some(code.into()),
			},
			Error::Pool(PoolError::UnknownTransactionValidity(code)) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_UNKNOWN_VALIDITY),
				message: "Unknown Transaction Validity".into(),
				data: Some(code.into()),
			},
			Error::Pool(PoolError::TemporarilyBanned) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_TEMPORARILY_BANNED),
				message: "Transaction is temporarily banned".into(),
				data: None,
			},
			Error::Pool(PoolError::AlreadyImported(hash)) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_ALREADY_IMPORTED),
				message: "Transaction Already Imported".into(),
				data: Some(format!("{:?}", hash).into()),
			},
			Error::Pool(PoolError::TooLowPriority { old, new }) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_TOO_LOW_PRIORITY),
				message: format!("Priority is too low: ({} vs {})", old, new),
				data: Some("The transaction has too low priority to replace another transaction already in the pool.".into()),
			},
			Error::Pool(PoolError::CycleDetected) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_CYCLE_DETECTED),
				message: "Cycle Detected".into(),
				data: None,
			},
			Error::Pool(PoolError::ImmediatelyDropped) => rpc::Error {
				code: rpc::ErrorCode::ServerError(POOL_IMMEDIATELY_DROPPED),
				message: "Immediately Dropped" .into(),
				data: Some("The transaction couldn't enter the pool because of the limit".into()),
			},
			e => errors::internal(e),
		}
	}
}
