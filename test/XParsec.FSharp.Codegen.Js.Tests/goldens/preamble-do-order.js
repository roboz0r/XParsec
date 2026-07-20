class Ordered {
  constructor(n) {
    this.n = n;
    const _s274 = this;
    (this.a = ((_s0) => (((_s0) + (1)) | 0))(_s274.n));
    console.log((("") + ("ctor a=") + (_s274.a)));
    (this.b = ((_s2) => (Math.imul((_s2), (3))))(_s274.a));
    console.log((("") + ("ctor b=") + (_s274.b)));
  }
}
const Ordered__B = (_s274) => _s274.b;
const o1 = new Ordered(1);
console.log((("") + ("b=") + (Ordered__B(o1))));
const o2 = new Ordered(5);
console.log((("") + ("b=") + (Ordered__B(o2))));
