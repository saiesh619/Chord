export function now() {
  return Date.now() * 1000;
}

export function to_n_bits(bi, n) {
  return bi & (2n**BigInt(n)-1n);
}
