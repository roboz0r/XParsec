class Adder {
  constructor(k) {
    this.k = k;
    const _s388 = this;
    (this.add = (x) => ((_s1) => (((x) + (_s1)) | 0))(_s388.k));
  }
}
const Adder__Twice = (_s388) => (n) => twice(_s388.add, n);
const twice = (f, x) => f(f(x));
const a = new Adder(3);
console.log(Adder__Twice(a)(10));
