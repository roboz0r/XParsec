class Vec {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const Vec__Sum = (_s346) => ((_s0) => ((_s1) => (((_s0) + (_s1)) | 0))(_s346.Y))(_s346.X);
const Vec__AddN = (_s346) => (n) => ((_s2) => (((_s2) + (n)) | 0))(((_s4) => ((_s5) => (((_s4) + (_s5)) | 0))(_s346.Y))(_s346.X));
const Vec__get_Doubled = (_s346) => ((_s6) => (Math.imul((_s6), (2))))(_s346.X);
const v = new Vec(3, 4);
console.log(Vec__Sum(v));
console.log(Vec__AddN(v)(10));
console.log(Vec__get_Doubled(v));
console.log(v.X);
