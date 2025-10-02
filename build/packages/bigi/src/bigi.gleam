import gleam/dynamic
import gleam/dynamic/decode
import gleam/list
import gleam/order

/// A big integer.
pub type BigInt

/// Endianness specifier used when converting between a big integer and
/// raw bytes.
pub type Endianness {
  LittleEndian
  BigEndian
}

/// Signedness specifier used when converting between a big integer and
/// raw bytes.
pub type Signedness {
  Signed
  Unsigned
}

/// Create a big integer representing zero.
@external(erlang, "bigi_ffi", "zero")
@external(javascript, "./bigi_ffi.mjs", "zero")
pub fn zero() -> BigInt

/// Create a big integer from a regular integer.
///
/// Note that in the JavaScript target, if your integer is bigger than the
/// [maximum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER)
/// or smaller than the
/// [minimum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER),
/// you may lose precision when operating on it, including when converting it
/// into a big integer (as the JavaScript Number type has already reduced the
/// precision of the value).
@external(erlang, "bigi_ffi", "from")
@external(javascript, "./bigi_ffi.mjs", "from")
pub fn from_int(int: Int) -> BigInt

/// Convert a string into a big integer.
///
/// If the string does not represent a big integer in base 10, an error is
/// returned. Trailing non-digit content is not allowed.
@external(erlang, "bigi_ffi", "from_string")
@external(javascript, "./bigi_ffi.mjs", "from_string")
pub fn from_string(str: String) -> Result(BigInt, Nil)

/// Convert raw bytes into a big integer.
/// 
/// If the bit array does not contain a whole number of bytes then an error is
/// returned.
@external(erlang, "bigi_ffi", "from_bytes")
@external(javascript, "./bigi_ffi.mjs", "from_bytes")
pub fn from_bytes(
  bytes: BitArray,
  endianness: Endianness,
  signedness: Signedness,
) -> Result(BigInt, Nil)

/// Convert a big integer to a regular integer.
///
/// In Erlang, this cannot fail, as all Erlang integers are big integers. In the
/// JavaScript target, this will fail if the integer is bigger than the
/// [maximum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER)
/// or smaller than the
/// [minimum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER).
@external(erlang, "bigi_ffi", "to")
@external(javascript, "./bigi_ffi.mjs", "to")
pub fn to_int(bigint: BigInt) -> Result(Int, Nil)

/// Convert the big integer into a simple string - a sequence of digits.
@external(erlang, "erlang", "integer_to_binary")
@external(javascript, "./bigi_ffi.mjs", "to_string")
pub fn to_string(bigint: BigInt) -> String

/// Convert a big integer to raw bytes.
/// 
/// The size of the returned bit array is specified by `byte_count`, e.g. 8 will
/// return a bit array containing 8 bytes (64 bits). If the big integer doesn't
/// fit in the specified number of bytes then an error is returned.
@external(erlang, "bigi_ffi", "to_bytes")
@external(javascript, "./bigi_ffi.mjs", "to_bytes")
pub fn to_bytes(
  bigint: BigInt,
  endianness: Endianness,
  signedness: Signedness,
  byte_count: Int,
) -> Result(BitArray, Nil)

/// Compare two big integers, returning an order that denotes if `a` is lower,
/// bigger than, or equal to `b`.
@external(erlang, "bigi_ffi", "compare")
@external(javascript, "./bigi_ffi.mjs", "compare")
pub fn compare(a: BigInt, with b: BigInt) -> order.Order

/// Get the absolute value of a big integer.
@external(erlang, "erlang", "abs")
@external(javascript, "./bigi_ffi.mjs", "absolute")
pub fn absolute(bigint: BigInt) -> BigInt

/// Returns the negative of the value provided.
@external(erlang, "bigi_ffi", "negate")
@external(javascript, "./bigi_ffi.mjs", "negate")
pub fn negate(bigint: BigInt) -> BigInt

/// Add two big integers together.
@external(erlang, "bigi_ffi", "add")
@external(javascript, "./bigi_ffi.mjs", "add")
pub fn add(a: BigInt, b: BigInt) -> BigInt

/// Subtract the subtrahend from the minuend.
@external(erlang, "bigi_ffi", "subtract")
@external(javascript, "./bigi_ffi.mjs", "subtract")
pub fn subtract(minuend a: BigInt, subtrahend b: BigInt) -> BigInt

/// Multiply two big integers together.
@external(erlang, "bigi_ffi", "multiply")
@external(javascript, "./bigi_ffi.mjs", "multiply")
pub fn multiply(a: BigInt, b: BigInt) -> BigInt

/// Divide the dividend with the divisor using integer division.
///
/// Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.
@external(erlang, "bigi_ffi", "divide")
@external(javascript, "./bigi_ffi.mjs", "divide")
pub fn divide(dividend a: BigInt, divisor b: BigInt) -> BigInt

/// Performs a *floored* integer division, which means that the result will
/// always be rounded towards negative infinity.
///
/// If you want to perform truncated integer division (rounding towards zero),
/// use `divide` or `divide_no_zero` instead.
///
/// Returns an error if the divisor is 0.
pub fn floor_divide(
  dividend dividend: BigInt,
  divisor divisor: BigInt,
) -> Result(BigInt, Nil) {
  let z = zero()
  case divisor == z {
    True -> Error(Nil)
    False ->
      case compare(multiply(dividend, divisor), z) {
        order.Lt ->
          case remainder(dividend, divisor) != z {
            True -> Ok(subtract(divide(dividend, divisor), from_int(1)))
            False -> Ok(divide(dividend, divisor))
          }
        _ -> Ok(divide(dividend, divisor))
      }
  }
}

/// Divide the dividend with the divisor using integer division.
///
/// Returns an error if the divisor is 0.
@external(erlang, "bigi_ffi", "divide_no_zero")
@external(javascript, "./bigi_ffi.mjs", "divide_no_zero")
pub fn divide_no_zero(
  dividend a: BigInt,
  divisor b: BigInt,
) -> Result(BigInt, Nil)

/// Divide the dividend with the divisor using integer division and return the
/// remainder.
///
/// Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.
@external(erlang, "bigi_ffi", "remainder")
@external(javascript, "./bigi_ffi.mjs", "remainder")
pub fn remainder(dividend a: BigInt, divisor b: BigInt) -> BigInt

/// Divide the dividend with the divisor using integer division and return the
/// remainder.
///
/// Returns an error if the divisor is 0.
@external(erlang, "bigi_ffi", "remainder_no_zero")
@external(javascript, "./bigi_ffi.mjs", "remainder_no_zero")
pub fn remainder_no_zero(
  dividend a: BigInt,
  divisor b: BigInt,
) -> Result(BigInt, Nil)

/// Calculate a mathematical modulo operation.
///
/// Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.
@external(erlang, "bigi_ffi", "modulo")
@external(javascript, "./bigi_ffi.mjs", "modulo")
pub fn modulo(dividend a: BigInt, divisor b: BigInt) -> BigInt

/// Calculate a mathematical modulo operation.
///
/// Returns an error if the divisor is 0.
@external(erlang, "bigi_ffi", "modulo_no_zero")
@external(javascript, "./bigi_ffi.mjs", "modulo_no_zero")
pub fn modulo_no_zero(
  dividend a: BigInt,
  divisor b: BigInt,
) -> Result(BigInt, Nil)

/// Raise the base to the exponent.
///
/// If the exponent is negative, an error is returned.
@external(erlang, "bigi_ffi", "power")
@external(javascript, "./bigi_ffi.mjs", "power")
pub fn power(base a: BigInt, exponent b: BigInt) -> Result(BigInt, Nil)

/// Get the digits in a given bigint as a list of integers in base 10.
///
/// The list is ordered starting from the most significant digit.
pub fn digits(bigint: BigInt) {
  let divisor = from_int(10)
  get_digit(bigint, [], divisor)
}

/// Decode a `gleam/dynamic` value into a big integer, if possible.
@external(erlang, "bigi_ffi", "decode")
@external(javascript, "./bigi_ffi.mjs", "decode")
pub fn decode(dyn: dynamic.Dynamic) -> Result(BigInt, List(decode.DecodeError))

/// Calculates the bitwise AND of its arguments.
@external(erlang, "bigi_ffi", "bitwise_and")
@external(javascript, "./bigi_ffi.mjs", "bitwise_and")
pub fn bitwise_and(a: BigInt, b: BigInt) -> BigInt

/// Calculates the bitwise XOR of its arguments.
@external(erlang, "bigi_ffi", "bitwise_exclusive_or")
@external(javascript, "./bigi_ffi.mjs", "bitwise_exclusive_or")
pub fn bitwise_exclusive_or(a: BigInt, b: BigInt) -> BigInt

/// Calculates the bitwise NOT of its argument.
@external(erlang, "bigi_ffi", "bitwise_not")
@external(javascript, "./bigi_ffi.mjs", "bitwise_not")
pub fn bitwise_not(bigint: BigInt) -> BigInt

/// Calculates the bitwise OR of its arguments.
@external(erlang, "bigi_ffi", "bitwise_or")
@external(javascript, "./bigi_ffi.mjs", "bitwise_or")
pub fn bitwise_or(a: BigInt, b: BigInt) -> BigInt

/// Calculates the result of an arithmetic left bitshift by the given amount.
@external(erlang, "bigi_ffi", "bitwise_shift_left")
@external(javascript, "./bigi_ffi.mjs", "bitwise_shift_left")
pub fn bitwise_shift_left(bigint: BigInt, by amount: Int) -> BigInt

/// Calculates the result of an arithmetic right bitshift by the given amount.
@external(erlang, "bigi_ffi", "bitwise_shift_right")
@external(javascript, "./bigi_ffi.mjs", "bitwise_shift_right")
pub fn bitwise_shift_right(bigint: BigInt, by amount: Int) -> BigInt

/// Restricts a big integer between a lower and upper bound.
pub fn clamp(bigint: BigInt, min min_bound: BigInt, max max_bound: BigInt) {
  bigint
  |> min(max_bound)
  |> max(min_bound)
}

/// Returns whether the big integer provided is odd.
pub fn is_odd(bigint: BigInt) {
  remainder(bigint, from_int(2)) != zero()
}

/// Compares two big integers, returning the larger of the two.
pub fn max(a: BigInt, or b: BigInt) {
  case compare(a, b) {
    order.Lt -> b
    _ -> a
  }
}

/// Compares two big integers, returning the smaller of the two.
pub fn min(a: BigInt, or b: BigInt) {
  case compare(a, b) {
    order.Lt -> a
    _ -> b
  }
}

/// Sums a list of big integers.
///
/// Returns 0 if the list was empty.
pub fn sum(bigints: List(BigInt)) {
  list.fold(bigints, zero(), add)
}

/// Multiplies a list of big integers.
///
/// Returns 1 if the list was empty.
pub fn product(bigints: List(BigInt)) {
  list.fold(bigints, from_int(1), multiply)
}

/// Joins a list of digits into a single value. Returns an error if the base is
/// less than 2 or if the list contains a digit greater than or equal to the
/// specified base.
pub fn undigits(digits: List(Int), base: Int) -> Result(BigInt, Nil) {
  case base < 2 {
    True -> Error(Nil)
    False -> {
      let base = from_int(base)
      list.try_fold(digits, zero(), fn(acc, digit) {
        let digit = from_int(digit)
        case compare(digit, base) {
          order.Gt | order.Eq -> Error(Nil)
          _ -> Ok(add(multiply(acc, base), digit))
        }
      })
    }
  }
}

fn get_digit(bigint: BigInt, digits: List(Int), divisor: BigInt) {
  case compare(bigint, divisor) {
    order.Lt -> {
      let assert Ok(digit) = to_int(bigint)
      [digit, ..digits]
    }
    _ -> {
      let assert Ok(digit) =
        remainder(bigint, divisor)
        |> to_int()
      let digits = [digit, ..digits]
      get_digit(divide(bigint, divisor), digits, divisor)
    }
  }
}
