## 0.2.2.1

 - Drop support for GHC 7.
 - Tested with GHC 8.0 - 9.14.

## 0.2.2.0

 - Provide safe coerceible based default-methods for pack/unpack

## 0.2.1.0 *(minor)*

 - Added `Newtype` instances for
     - `Data.Fixed.Fixed`
     - `Data.Functor.Compose.Compose`
     - `Data.Functor.Identity.Identity`
     - `Data.Monoid.Alt`
     - `Data.Monoid.Ap`
     - `Data.Monoid.Dual`
     - `Data.Ord.Down`
  - Declare `Control.Newtype` explicitly as `Trustworthy` under SafeHaskell
