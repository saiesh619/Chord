import chord_math.{ipow}
import gleam/int

//import gleam/option.{type Option}
//import gleam/string
//import randomlib

// -------------------
// Helpers
// -------------------

/// Hash an integer into the identifier space [0, 2^m)
pub fn hash_id(x: Int, m: Int) -> Int {
  let size = ipow(2, m)
  int.absolute_value(x) % size
}

/// Compute the start of the i-th finger for node n
pub fn finger_start(n: Int, i: Int, m: Int) -> Int {
  let size_val = ipow(2, m)
  let base_offset = ipow(2, i - 1)
  let offset = { n + base_offset } % size_val
  offset
}

/// Pick a random identifier in [0, 2^m)
pub fn rand_key(m: Int) -> Int {
  let size = ipow(2, m)
  int.random(size)
  // Returns an Int in [0, size)
}

/// Interval check: inclusive (start, end]
pub fn in_interval(id: Int, start: Int, end_: Int, _m: Int) -> Bool {
  case start < end_ {
    True -> id > start && id <= end_
    False -> id > start || id <= end_
  }
}

/// Interval check: exclusive (start, end)
pub fn in_interval_exclusive(id: Int, start: Int, end_: Int, _m: Int) -> Bool {
  case start < end_ {
    True -> id > start && id < end_
    False -> id > start || id < end_
  }
}
