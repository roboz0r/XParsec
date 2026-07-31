class Vec {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const Vec__Sum = (_s0) => ((_s15) => ((_s16) => (((_s15) + (_s16)) | 0))(_s0.Y))(_s0.X);
const Vec__AddN = (_s0) => (n) => ((_s23) => (((_s23) + (n)) | 0))(((_s19) => ((_s20) => (((_s19) + (_s20)) | 0))(_s0.Y))(_s0.X));
const Vec__get_Doubled = (_s0) => ((_s27) => (Math.imul((_s27), (2))))(_s0.X);
const v = new Vec(3, 4);
console.log(Vec__Sum(v));
console.log(Vec__AddN(v)(10));
console.log(Vec__get_Doubled(v));
console.log(v.X);
