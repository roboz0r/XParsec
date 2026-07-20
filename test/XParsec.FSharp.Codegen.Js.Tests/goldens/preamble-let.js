class Boxed {
  constructor(n) {
    this.n = n;
    const _s257 = this;
    (this.m = ((_s2) => (((_s2) + (1)) | 0))(_s257.n));
  }
}
const Boxed__M = (_s257) => ((_s0) => (Math.imul((_s0), (10))))(_s257.m);
const b = new Boxed(4);
console.log(Boxed__M(b));
