import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from "./Vesper.Core/index.mjs";
class Meters {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Meters";
  }
  cases() {
    return ["M"];
  }
}
class Meters_M extends Meters {
  constructor(Item) {
    super(0);
    this.Item = Item;
  }
}
class Shape {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Shape";
  }
  cases() {
    return ["Dot", "Line"];
  }
}
class Shape_Dot extends Shape {
  constructor() {
    super(0);
  }
}
class Shape_Line extends Shape {
  constructor(len) {
    super(1);
    this.len = len;
  }
}
class Quad {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Quad";
  }
  cases() {
    return ["Q0", "Q1", "Q2", "Q3"];
  }
}
class Quad_Q0 extends Quad {
  constructor() {
    super(0);
  }
}
class Quad_Q1 extends Quad {
  constructor(Item) {
    super(1);
    this.Item = Item;
  }
}
class Quad_Q2 extends Quad {
  constructor(Item1, Item2) {
    super(2);
    this.Item1 = Item1;
    this.Item2 = Item2;
  }
}
class Quad_Q3 extends Quad {
  constructor(Item1, Item2, Item3) {
    super(3);
    this.Item1 = Item1;
    this.Item2 = Item2;
    this.Item3 = Item3;
  }
}
const metersValue = (m) => ((_m59) => {
  if ((_m59.tag === 0)) {
    const v = _m59.Item;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(m);
const describeShape = (s) => ((_m60) => {
  if ((_m60.tag === 0)) {
    return 0;
  }
  if ((_m60.tag === 1)) {
    const len = _m60.len;
    return len;
  }
  throw new Error("The match cases were incomplete");
})(s);
const describeQuad = (q) => ((_m61) => {
  if ((_m61.tag === 0)) {
    return 0;
  }
  if ((_m61.tag === 1)) {
    const x = _m61.Item;
    return x;
  }
  if ((_m61.tag === 2)) {
    const a = _m61.Item1;
    const b = _m61.Item2;
    return (((a) + (b)) | 0);
  }
  if ((_m61.tag === 3)) {
    const a = _m61.Item1;
    const b = _m61.Item2;
    const c = _m61.Item3;
    return ((((((a) + (b)) | 0)) + (c)) | 0);
  }
  throw new Error("The match cases were incomplete");
})(q);
console.log(metersValue(new Meters_M(7)));
console.log(describeShape(new Shape_Dot()));
console.log(describeShape(new Shape_Line(4)));
console.log(describeQuad(new Quad_Q0()));
console.log(describeQuad(new Quad_Q1(5)));
console.log(describeQuad(new Quad_Q2(2, 3)));
console.log(describeQuad(new Quad_Q3(1, 2, 3)));
console.log((((_s41) => ((_s42) => $Vesper_StructuralRuntime_structuralEquals(_s41, _s42))(new Meters_M(7)))(new Meters_M(7)) ? "true" : "false"));
console.log((((_s43) => ((_s44) => $Vesper_StructuralRuntime_structuralEquals(_s43, _s44))(new Meters_M(8)))(new Meters_M(7)) ? "true" : "false"));
console.log((((_s45) => ((_s46) => $Vesper_StructuralRuntime_structuralEquals(_s45, _s46))(new Shape_Dot()))(new Shape_Dot()) ? "true" : "false"));
console.log((((_s47) => ((_s48) => $Vesper_StructuralRuntime_structuralEquals(_s47, _s48))(new Shape_Line(4)))(new Shape_Line(4)) ? "true" : "false"));
console.log((((_s49) => ((_s50) => $Vesper_StructuralRuntime_structuralEquals(_s49, _s50))(new Shape_Line(5)))(new Shape_Line(4)) ? "true" : "false"));
console.log((((_s51) => ((_s52) => $Vesper_StructuralRuntime_structuralEquals(_s51, _s52))(new Shape_Dot()))(new Shape_Line(4)) ? "true" : "false"));
console.log((((_s53) => ((_s54) => $Vesper_StructuralRuntime_structuralEquals(_s53, _s54))(new Quad_Q0()))(new Quad_Q0()) ? "true" : "false"));
console.log((((_s55) => ((_s56) => $Vesper_StructuralRuntime_structuralEquals(_s55, _s56))(new Quad_Q2(2, 3)))(new Quad_Q2(2, 3)) ? "true" : "false"));
console.log((((_s57) => ((_s58) => $Vesper_StructuralRuntime_structuralEquals(_s57, _s58))(new Quad_Q3(1, 2, 3)))(new Quad_Q2(2, 3)) ? "true" : "false"));
