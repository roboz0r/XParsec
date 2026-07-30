class Vec {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const Vec__Sum = (_s0) => ((_s9) => ((_s10) => (((_s9) + (_s10)) | 0))(_s0.Y))(_s0.X);
const Vec__AddN = (_s0) => (n) => ((_s13) => (((_s13) + (n)) | 0))(((_s11) => ((_s12) => (((_s11) + (_s12)) | 0))(_s0.Y))(_s0.X));
const Vec__get_Doubled = (_s0) => ((_s15) => (Math.imul((_s15), (2))))(_s0.X);
const v = new Vec(3, 4);
console.log(Vec__Sum(v));
console.log(Vec__AddN(v)(10));
console.log(Vec__get_Doubled(v));
console.log(v.X);
