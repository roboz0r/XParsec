console.log(String((BigInt.asIntN(64, (1000000000000n) + (1n)))));
console.log(String((BigInt.asIntN(64, (1000000000000n) - (1n)))));
console.log(String((BigInt.asIntN(64, (1000000000000n) * (3n)))));
console.log(String((BigInt.asIntN(64, (3000000000000n) / ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((3n))))))));
console.log(String((BigInt.asIntN(64, (1000000000001n) % ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((10n))))))));
console.log(String((BigInt.asIntN(64, (-7n) / ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((2n))))))));
