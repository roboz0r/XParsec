import { checkedDivisor as $Vesper_ArithmeticRuntime_checkedDivisor } from "./Vesper.Core.mjs";
console.log(String((BigInt.asIntN(64, (1000000000000n) + (1n)))));
console.log(String((BigInt.asIntN(64, (1000000000000n) - (1n)))));
console.log(String((BigInt.asIntN(64, (1000000000000n) * (3n)))));
console.log(String((BigInt.asIntN(64, (3000000000000n) / ($Vesper_ArithmeticRuntime_checkedDivisor(3n))))));
console.log(String((BigInt.asIntN(64, (1000000000001n) % ($Vesper_ArithmeticRuntime_checkedDivisor(10n))))));
console.log(String((BigInt.asIntN(64, (-7n) / ($Vesper_ArithmeticRuntime_checkedDivisor(2n))))));
