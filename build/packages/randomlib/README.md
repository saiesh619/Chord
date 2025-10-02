# randomlib

[![Package Version](https://img.shields.io/hexpm/v/randomlib)](https://hex.pm/packages/randomlib)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/randomlib/)

A random number library for Gleam

This package provides various function to generate random output for different types
The random generation should be the same between Erlang and Javascript targets

```sh
gleam add randomlib@1
```
```gleam
import randomlib

pub fn main() {
  randomlib.choice(randomlib.new(), ["Red", "Green", "Blue"])

  let #(val, rnd) = randomlib.next_byte(randomlib.with_seed(13470613))
  let #(val2, rnd) = randomlib.next_float(rnd)

  io.debug(val)
  io.debug(val2)

}
// -> 50
// -> 0.9460694304884285
```

Further documentation can be found at <https://hexdocs.pm/randomlib>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
