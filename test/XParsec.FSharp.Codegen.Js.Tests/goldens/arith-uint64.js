console.log(String((BigInt.asUintN(64, (10n) + (3n)))));
console.log(String((BigInt.asUintN(64, (10n) - (3n)))));
console.log(String((BigInt.asUintN(64, (10n) * (3n)))));
console.log(String((BigInt.asUintN(64, (10n) / ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((3n))))))));
console.log(String((BigInt.asUintN(64, (10n) % ((((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)((3n))))))));
console.log(String((BigInt.asUintN(64, (0n) - (1n)))));
