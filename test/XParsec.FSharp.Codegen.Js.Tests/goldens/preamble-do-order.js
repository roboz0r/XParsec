class Ordered {
  constructor(n) {
    this.n = n;
    const _s0 = this;
    (this.a = (((_s0.n) + (1)) | 0));
    console.log((("") + ("ctor a=") + (_s0.a)));
    (this.b = (Math.imul((_s0.a), (3))));
    console.log((("") + ("ctor b=") + (_s0.b)));
  }
}
const Ordered__B = (_s0) => _s0.b;
const o1 = new Ordered(1);
console.log((("") + ("b=") + (Ordered__B(o1))));
const o2 = new Ordered(5);
console.log((("") + ("b=") + (Ordered__B(o2))));
