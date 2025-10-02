/// Integer exponentiation (safe, recursive)
pub fn ipow(base: Int, exp: Int) -> Int {
  case exp {
    0 -> 1
    _ -> base * ipow(base, exp - 1)
  }
}
