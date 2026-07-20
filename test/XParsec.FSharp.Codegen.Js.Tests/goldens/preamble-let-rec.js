class Factorial {
  constructor(n) {
    this.n = n;
    const _s259 = this;
    (this.fact = (k) => (((k) <= (1)) ? 1 : ((_s3) => (Math.imul((k), (_s3))))(_s259.fact((((k) - (1)) | 0)))));
    (this.value = _s259.fact(_s259.n));
  }
}
const Factorial__Value = (_s259) => _s259.value;
const Factorial__Of = (_s259) => (k) => _s259.fact(k);
const f = new Factorial(5);
console.log(Factorial__Value(f));
console.log(Factorial__Of(f)(6));
