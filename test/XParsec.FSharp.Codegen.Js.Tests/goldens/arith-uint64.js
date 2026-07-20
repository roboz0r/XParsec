import { checkedDivisor as $Vesper_ArithmeticRuntime_checkedDivisor } from "./Vesper.Core.mjs";
console.log(String((BigInt.asUintN(64, (10n) + (3n)))));
console.log(String((BigInt.asUintN(64, (10n) - (3n)))));
console.log(String((BigInt.asUintN(64, (10n) * (3n)))));
console.log(String((BigInt.asUintN(64, (10n) / ($Vesper_ArithmeticRuntime_checkedDivisor(3n))))));
console.log(String((BigInt.asUintN(64, (10n) % ($Vesper_ArithmeticRuntime_checkedDivisor(3n))))));
console.log(String((BigInt.asUintN(64, (0n) - (1n)))));
