class Adder {
  constructor(k) {
    this.k = k;
    const _s3 = this;
    (this.add = (x) => ((_s5) => (((x) + (_s5)) | 0))(_s3.k));
  }
}
const Adder__Twice = (_s3) => (n) => twice(_s3.add, n);
const twice = (f, x) => f(f(x));
const a = new Adder(3);
console.log(Adder__Twice(a)(10));
