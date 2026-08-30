import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from "./Vesper.Core/index.mjs";
class G {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "G`1";
  }
  cases() {
    return ["Val", "Num"];
  }
}
class G_Val extends G {
  constructor(v) {
    super(0);
    this.v = v;
  }
}
class G_Num extends G {
  constructor(n) {
    super(1);
    this.n = n;
  }
}
const pick = (g) => ((_m32) => {
  if ((_m32.tag === 0)) {
    const v = _m32.v;
    return v;
  }
  if ((_m32.tag === 1)) {
    const n = _m32.n;
    return (Math.imul((n), (10)));
  }
  throw new Error("The match cases were incomplete");
})(g);
const a = new G_Val(3);
const b = new G_Num(4);
console.log(pick(a));
console.log(pick(b));
console.log((((_s27) => $Vesper_StructuralRuntime_structuralEquals(a, _s27))(new G_Val(3)) ? "true" : "false"));
console.log(($Vesper_StructuralRuntime_structuralEquals(a, b) ? "true" : "false"));
const s = new G_Val("hi");
const text = (g) => ((_m33) => {
  if ((_m33.tag === 0)) {
    const v = _m33.v;
    return v;
  }
  if ((_m33.tag === 1)) {
    return "num";
  }
  throw new Error("The match cases were incomplete");
})(g);
console.log(text(s));
console.log((((_s31) => $Vesper_StructuralRuntime_structuralEquals(s, _s31))(new G_Val("hi")) ? "true" : "false"));
