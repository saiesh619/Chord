import bigi.{type BigInt}
import gleam/float
import gleam/int
import gleam/list
import gleam/order.{Eq, Gt, Lt}
import gleam/yielder.{type Yielder, Next}

pub opaque type Random {
  Random(seed: Int)
}

const unique_seed = 3_447_679_086_515_839_964

const multiplier = 25_214_903_917

const bitmask_48 = 281_474_976_710_655

const int_limit = 2_147_483_647

/// Creates a new random seed with distinctness set using the
/// current time, nominally in nanoseconds.
/// Please note that the actual timings will be microseconds
/// for erland and milliseconds for javascript, so any new random
/// seeds generated in that same time scale will be the same
pub fn new() {
  Random(initial_scramble(init_seed()))
}

/// Creates a new random seed based on the specified seed provided
pub fn with_seed(seed: Int) {
  Random(initial_scramble(seed))
}

/// Returns a tuple containing a BigInt of size n bits and the
/// updated random seed
/// Note that this hasn't been fully tested to generate
/// uniformly distributed values
pub fn next(rnd: Random, bits: Int) -> #(BigInt, Random) {
  get_next(bits, #(bigi.zero(), rnd))
}

/// Returns a tuple containing a uniformly distributed Bool and 
/// the updated random seed
pub fn next_bool(rnd: Random) -> #(Bool, Random) {
  let #(val, rnd) = get_next_bits(rnd, 1)
  #(val == 0, rnd)
}

/// Returns a tuple containing a byte (0-255) in Int form and the
/// updated random seed
pub fn next_byte(rnd: Random) -> #(Int, Random) {
  get_next_bits(rnd, 8)
}

/// Returns a tuple containing a list of n bytes in Int form and
/// the updated random seed
/// Passing n <= 0 will return an empty list
pub fn next_bytes(rnd: Random, n: Int) -> #(List(Int), Random) {
  get_next_bytes(#([], rnd), n)
}

/// Returns a tuple containing a uniformly distributed float 
/// between 0.0 (inclusive) and 1.0 (exclusive) and the updated random seed
pub fn next_float(rnd: Random) -> #(Float, Random) {
  let #(val, rnd) = get_next_bits(rnd, 53)
  let assert Ok(unit) = float.power(2.0, -53.0)
  #(int.to_float(val) *. unit, rnd)
}

/// Returns a result containing either a tuple containing a uniformly distributed
/// Int (where int is 31 bits unsigned) and the updated random seed or an error
/// containing the random seed (currently not updated)
pub fn next_int(rnd: Random, limit: Int) -> Result(#(Int, Random), Random) {
  case limit {
    l if l <= 0 || l >= int_limit -> Error(rnd)
    limit -> {
      let #(next, rnd) = get_next_bits(rnd, 31)

      case int.compare(int.bitwise_and(limit - 1, limit), 0) {
        Eq -> {
          let assert Ok(next) =
            bigi.to_int(bigi.bitwise_shift_right(
              bigi.multiply(bigi.from_int(next), bigi.from_int(limit)),
              31,
            ))
          Ok(#(next, rnd))
        }
        _ -> {
          let #(next, rnd) = get_unique_int(#(next, rnd), limit)
          Ok(#(next, rnd))
        }
      }
    }
  }
}

/// Returns an iterated that generates byte in Int form when iterated
/// Note that the random seed is internally updated but there is no ability
/// to extract the updated seed
pub fn byte_iterator(rnd: Random) -> Yielder(Int) {
  yielder.unfold(from: rnd, with: fn(acc) {
    let #(next, rnd) = next_byte(acc)
    Next(next, rnd)
  })
}

/// Returns an iterated that generates uniformly distributed Floats when iterated
/// Note that the random seed is internally updated but there is no ability
/// to extract the updated seed
pub fn float_iterator(rnd: Random) -> Yielder(Float) {
  yielder.unfold(from: rnd, with: fn(acc) {
    let #(next, rnd) = next_float(acc)
    Next(next, rnd)
  })
}

/// If non-empty choices list is provided, returns an iterator that performs a uniformly 
/// distributed selection from the the items in the list
/// If no choices are passed an Error(Nil) is returned
pub fn choice(rnd: Random, choices: List(value)) -> Result(Yielder(value), Nil) {
  case choices {
    [] -> Error(Nil)
    choices -> {
      let length = list.length(choices)
      Ok(
        yielder.unfold(from: rnd, with: fn(acc) {
          let assert Ok(#(next, rnd)) = next_int(acc, length)
          let assert Ok(next) = list.first(list.split(choices, next).1)
          Next(next, rnd)
        }),
      )
    }
  }
}

// Internal functions

/// Picks a random seed based on a static seed and the current time in nanosecond
/// the long number is chosen from the initial startup value of Java random
/// using the corrected value from L'Ecuyer, "Tables of Linear Congruential Generators of
/// Different Sizes and Good Lattice Structure", 1999
fn init_seed() -> Int {
  int.bitwise_exclusive_or(unique_seed, ffi_now() * 1000)
}

fn initial_scramble(seed: Int) -> Int {
  int.bitwise_and(int.bitwise_exclusive_or(seed, multiplier), bitmask_48)
}

fn get_next_bits(rnd: Random, bits: Int) -> #(Int, Random) {
  let assert Ok(next_seed) =
    bigi.to_int(ffi_to_n_bits(
      bigi.add(
        ffi_to_n_bits(
          bigi.multiply(bigi.from_int(rnd.seed), bigi.from_int(multiplier)),
          64,
        ),
        bigi.from_int(11),
      ),
      48,
    ))
  let next_num = int.bitwise_shift_right(next_seed, 48 - bits)
  #(next_num, Random(next_seed))
}

fn get_next_bytes(acc: #(List(Int), Random), n: Int) -> #(List(Int), Random) {
  case int.compare(n, 0) {
    Eq | Lt -> acc
    Gt -> {
      let #(l, rnd) = acc
      let #(next, rnd) = get_next_bits(rnd, 8)
      get_next_bytes(#([next, ..l], rnd), n - 1)
    }
  }
}

fn get_next(bits: Int, res: #(BigInt, Random)) -> #(BigInt, Random) {
  let #(bi, rnd) = res
  case bits > 48 {
    True -> {
      let #(next, rnd) = get_next_bits(rnd, 48)
      get_next(bits - 48, #(
        bigi.add(bigi.bitwise_shift_left(bi, 48), bigi.from_int(next)),
        rnd,
      ))
    }
    False -> {
      case bits {
        0 -> res
        bits -> {
          let #(next, rnd) = get_next_bits(rnd, bits)
          #(bigi.add(bigi.bitwise_shift_left(bi, 48), bigi.from_int(next)), rnd)
        }
      }
    }
  }
}

fn get_unique_int(acc: #(Int, Random), limit: Int) -> #(Int, Random) {
  let #(r, rnd) = acc
  let u = r
  let assert Ok(r) = int.modulo(u, limit)
  case int.compare(u - r + limit - 1, int_limit) {
    Gt -> get_unique_int(get_next_bits(rnd, 31), limit)
    _ -> #(r, rnd)
  }
}

@external(erlang, "randomlib_ffi", "now")
@external(javascript, "./randomlib_ffi.mjs", "now")
fn ffi_now() -> Int

@external(javascript, "./randomlib_ffi.mjs", "to_n_bits")
fn ffi_to_n_bits(bi: BigInt, n: Int) -> BigInt {
  let assert Ok(i) = bigi.to_int(bi)
  let assert Ok(pow) = int.power(2, int.to_float(n))
  bigi.from_int(int.bitwise_and(i, float.truncate(pow) - 1))
}
// bigi fns needed
// from_int
// to_int
// add
// multiply
// from_string
// zero
// bitwise_shift_left
// bitwise_shift_right
