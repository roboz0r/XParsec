import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from "./Vesper.Core/index.mjs";
class Shape {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Shape";
  }
  cases() {
    return ["Empty", "Point", "Pair"];
  }
}
class Shape_Empty extends Shape {
  constructor() {
    super(0);
  }
}
class Shape_Point extends Shape {
  constructor(x) {
    super(1);
    this.x = x;
  }
}
class Shape_Pair extends Shape {
  constructor(a, b) {
    super(2);
    this.a = a;
    this.b = b;
  }
}
const describe = (s) => ((_m31) => {
  if ((_m31.tag === 0)) {
    return 0;
  }
  if ((_m31.tag === 1)) {
    const x = _m31.x;
    return x;
  }
  if ((_m31.tag === 2)) {
    const a = _m31.a;
    const b = _m31.b;
    return (((a) + (b)) | 0);
  }
  throw new Error("The match cases were incomplete");
})(s);
const s0 = new Shape_Empty();
const s1 = new Shape_Point(3);
const s2 = new Shape_Pair(4, 5);
console.log(describe(s0));
console.log(describe(s1));
console.log(describe(s2));
console.log((((_s22) => $Vesper_StructuralRuntime_structuralEquals(s1, _s22))(new Shape_Point(3)) ? "true" : "false"));
console.log((((_s24) => $Vesper_StructuralRuntime_structuralEquals(s1, _s24))(new Shape_Empty()) ? "true" : "false"));
console.log((((_s25) => $Vesper_StructuralRuntime_structuralEquals(_s25, s1))(new Shape_Point(3)) ? "true" : "false"));
console.log((((_s28) => $Vesper_StructuralRuntime_structuralEquals(s2, _s28))(new Shape_Pair(4, 5)) ? "true" : "false"));
console.log((((_s30) => $Vesper_StructuralRuntime_structuralEquals(s2, _s30))(new Shape_Pair(4, 6)) ? "true" : "false"));
