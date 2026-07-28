class Vec {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const Vec__Sum = (_s0) => ((_s2) => ((_s1) => (((_s2) + (_s1)) | 0))(_s0.Y))(_s0.X);
const Vec__AddN = (_s0) => (n) => ((_s7) => (((_s7) + (n)) | 0))(((_s5) => ((_s4) => (((_s5) + (_s4)) | 0))(_s0.Y))(_s0.X));
const Vec__get_Doubled = (_s0) => ((_s9) => (Math.imul((_s9), (2))))(_s0.X);
const v = new Vec(3, 4);
console.log(Vec__Sum(v));
console.log(Vec__AddN(v)(10));
console.log(Vec__get_Doubled(v));
console.log(v.X);
