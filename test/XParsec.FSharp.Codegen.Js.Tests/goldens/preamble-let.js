class Boxed {
  constructor(n) {
    this.n = n;
    const _s0 = this;
    (this.m = ((_s10) => (((_s10) + (1)) | 0))(_s0.n));
  }
}
const Boxed__M = (_s0) => ((_s8) => (Math.imul((_s8), (10))))(_s0.m);
const b = new Boxed(4);
console.log(Boxed__M(b));
