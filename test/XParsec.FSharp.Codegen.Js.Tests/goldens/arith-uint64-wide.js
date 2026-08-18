console.log(String((BigInt.asUintN(64, (10000000000n) + (1n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) - (1n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) * (3n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) / ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((3n))))))));
console.log(String((BigInt.asUintN(64, (10000000000n) % ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((3n))))))));
console.log(String((BigInt.asUintN(64, (18446744073709551615n) / ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((2n))))))));
