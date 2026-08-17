import { checkedDivisor as $Vesper_ArithmeticRuntime_checkedDivisor } from "./Vesper.Core/index.mjs";
console.log(String((BigInt.asUintN(64, (10000000000n) + (1n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) - (1n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) * (3n)))));
console.log(String((BigInt.asUintN(64, (10000000000n) / ($Vesper_ArithmeticRuntime_checkedDivisor(3n))))));
console.log(String((BigInt.asUintN(64, (10000000000n) % ($Vesper_ArithmeticRuntime_checkedDivisor(3n))))));
console.log(String((BigInt.asUintN(64, (18446744073709551615n) / ($Vesper_ArithmeticRuntime_checkedDivisor(2n))))));
