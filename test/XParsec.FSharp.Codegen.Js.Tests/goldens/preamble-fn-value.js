class Adder {
  constructor(k) {
    this.k = k;
    const _s3 = this;
    (this.add = (x) => (((x) + (_s3.k)) | 0));
  }
}
const Adder__Twice = (_s3) => (n) => twice(_s3.add, n);
const twice = (f, x) => f(f(x));
const a = new Adder(3);
console.log(Adder__Twice(a)(10));
