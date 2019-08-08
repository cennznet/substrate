// Copyright 2019
//     by  Centrality Investments Ltd.
//     and Parity Technologies (UK) Ltd.
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

// Ensure we're `no_std` when compiling for Wasm.
#![cfg_attr(not(feature = "std"), no_std)]

use parity_codec::{Codec, Encode, Decode};
use rstd::marker::PhantomData;
use sr_primitives::traits::{
	Member, Zero, DispatchError, Convert,
	SignedExtension, SaturatedConversion, MaybeDebug, Saturating
};
use sr_primitives::weights::{Weight, DispatchInfo};
use sr_primitives::transaction_validity::{TransactionPriority, ValidTransaction};
use support::{
	decl_module,
	StorageMap,
	traits::{BasicCurrency, OnUnbalanced, ComputeFee, Get},
};
use system;

pub type BalanceOf<T> = <<T as Trait>::Currency as BasicCurrency<<T as system::Trait>::AccountId>>::Balance;
pub type NegativeImbalanceOf<T> =
	<<T as Trait>::Currency as BasicCurrency<<T as system::Trait>::AccountId>>::NegativeImbalance;
pub type PositiveImbalanceOf<T> =
	<<T as Trait>::Currency as BasicCurrency<<T as system::Trait>::AccountId>>::PositiveImbalance;

pub trait Trait: system::Trait {
	/// The fee to be paid for making a transaction; the base.
	type TransactionBaseFee: Get<BalanceOf<Self>>;

	/// The fee to be paid for making a transaction; the per-byte portion.
	type TransactionByteFee: Get<BalanceOf<Self>>;

	/// Handler for the unbalanced reduction when taking transaction fees.
	type TransactionPayment: OnUnbalanced<NegativeImbalanceOf<Self>>;
	/// Convert a weight value into a deductible fee based on the currency type.
	type WeightToFee: Convert<Weight, BalanceOf<Self>>;

	type ComputeFee: ComputeFee<Self::AccountId, BalanceOf<Self>>;

	type Currency: BasicCurrency<Self::AccountId, AdditionalInfo=Self::AdditionalInfo>;

	type AdditionalInfo: Member + Codec;
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		/// The fee to be paid for making a transaction; the base.
		const TransactionBaseFee: BalanceOf<T> = T::TransactionBaseFee::get();

		/// The fee to be paid for making a transaction; the per-byte portion.
		const TransactionByteFee: BalanceOf<T> = T::TransactionByteFee::get();
	}
}

pub struct DefaultComputeFee<T: Trait>(PhantomData<T>);

impl<T: Trait> ComputeFee<T::AccountId, BalanceOf<T>> for DefaultComputeFee<T> {

	/// Compute the base fee value for a particular transaction.
	///
	/// The base fee is composed of:
	///   - _length-fee_: This is the amount paid merely to pay for size of the transaction.
	///   - _weight-fee_: This amount is computed based on the weight of the transaction. Unlike
	///      size-fee, this is not input dependent and reflects the _complexity_ of the execution
	///      and the time it consumes.
	fn compute_fee(len: usize, info: DispatchInfo) -> BalanceOf<T> {
		let len_fee = if info.pay_length_fee() {
			let len: BalanceOf<T> = (len as u32).into();
			let base = T::TransactionBaseFee::get();
			let per_byte = T::TransactionByteFee::get();
			base.saturating_add(per_byte.saturating_mul(len))
		} else {
			Zero::zero()
		};

		let weight_fee = {
			// cap the weight to the maximum defined in runtime, otherwise it will be the `Bounded`
			// maximum of its data type, which is not desired.
			let capped_weight = info.weight.min(<T as system::Trait>::MaximumBlockWeight::get());
			let weight_update = <system::Module<T>>::next_weight_multiplier();
			let adjusted_weight = weight_update.apply_to(capped_weight);
			T::WeightToFee::convert(adjusted_weight)
		};

		len_fee.saturating_add(weight_fee)
	}
}

/// Require the transactor pay for themselves and maybe include a tip to gain additional priority
/// in the queue.
#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, Clone, Eq, PartialEq)]
pub struct TakeFees<T: Trait> {
	#[codec(compact)]
	tip: BalanceOf<T>,
	additional_info: T::AdditionalInfo,
}

impl<T: Trait> TakeFees<T> {
	fn compute_fee(len: usize, info: DispatchInfo, tip: BalanceOf<T>) -> BalanceOf<T> {
		let fee = T::ComputeFee::compute_fee(len, info);
		fee.saturating_add(tip)
	}
}

impl<T> SignedExtension for TakeFees<T>
where
	T: Trait + Clone + Eq + MaybeDebug,
	<<T as Trait>::Currency as BasicCurrency<T::AccountId>>::Balance: Sync + Send
{
	type AccountId = T::AccountId;
	type Call = T::Call;
	type AdditionalSigned = ();
	type Pre = ();

	fn additional_signed(&self) -> rstd::result::Result<(), &'static str> { Ok(()) }

	fn validate(
		&self,
		who: &Self::AccountId,
		_call: &Self::Call,
		info: DispatchInfo,
		len: usize,
	) -> rstd::result::Result<ValidTransaction, DispatchError> {
		// pay any fees.
		let fee = Self::compute_fee(len, info, self.tip);
		let imbalance = T::Currency::withdraw(
			who,
			fee,
			&self.additional_info,
		).map_err(|_| DispatchError::Payment)?;
		T::TransactionPayment::on_unbalanced(imbalance);

		let mut r = ValidTransaction::default();
		// NOTE: we probably want to maximize the _fee (of any type) per weight unit_ here, which
		// will be a bit more than setting the priority to tip. For now, this is enough.
		r.priority = fee.saturated_into::<TransactionPriority>();
		Ok(r)
	}
}
