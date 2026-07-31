import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from "./Vesper.Core.mjs";
class P {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const a = new P(3, 4);
const b = new P(3, 4);
const c = new P(5, 4);
console.log(a.X);
console.log(a.Y);
console.log(($Vesper_StructuralRuntime_structuralEquals(a, b) ? "true" : "false"));
console.log(($Vesper_StructuralRuntime_structuralEquals(a, c) ? "true" : "false"));
const d = new P(10, a.Y);
console.log(((_s17) => ((_s18) => (((_s17) + (_s18)) | 0))(d.Y))(d.X));
