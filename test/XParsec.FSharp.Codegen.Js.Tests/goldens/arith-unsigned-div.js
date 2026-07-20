import { checkedDivisor as $Vesper_ArithmeticRuntime_checkedDivisor } from "./Vesper.Core.mjs";
console.log(((_s0) => ((_s0) | 0))(((_s2) => (((_s2) * (3)) & 0xFF))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) & 0xFF))));
console.log(((_s6) => ((_s6) | 0))(((_s8) => (((_s8) * (2)) & 0xFF))((((7) / ($Vesper_ArithmeticRuntime_checkedDivisor(2))) & 0xFF))));
console.log(((_s12) => ((_s12) | 0))(((_s14) => (((_s14) * (3)) & 0xFFFF))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) & 0xFFFF))));
console.log(((_s18) => ((_s18) | 0))(((_s20) => (((_s20) * (3)) << 24 >> 24))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) << 24 >> 24))));
console.log(((_s24) => ((_s24) | 0))(((_s26) => (((_s26) * (3)) << 16 >> 16))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) << 16 >> 16))));
console.log((((_s30) => ((_s30) | 0))(((_s32) => (Math.imul((_s32), (3)) >>> 0))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) >>> 0))) >>> 0).toString());
console.log(((_s36) => (Math.imul((_s36), (3))))((((10) / ($Vesper_ArithmeticRuntime_checkedDivisor(3))) | 0)));
