class Boxed {
  constructor(n) {
    this.n = n;
    const _s0 = this;
    (this.m = (((_s0.n) + (1)) | 0));
  }
}
const Boxed__M = (_s0) => (Math.imul((_s0.m), (10)));
const b = new Boxed(4);
console.log(Boxed__M(b));
