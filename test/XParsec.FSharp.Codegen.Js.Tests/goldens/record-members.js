class Vec {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const Vec__Sum = (_s0) => (((_s0.X) + (_s0.Y)) | 0);
const Vec__AddN = (_s0) => (n) => ((((((_s0.X) + (_s0.Y)) | 0)) + (n)) | 0);
const Vec__get_Doubled = (_s0) => (Math.imul((_s0.X), (2)));
const v = new Vec(3, 4);
console.log(Vec__Sum(v));
console.log(Vec__AddN(v)(10));
console.log(Vec__get_Doubled(v));
console.log(v.X);
