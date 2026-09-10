class Factorial {
  constructor(n) {
    this.n = n;
    const _s0 = this;
    (this.fact = (k) => (((k) <= (1)) ? 1 : (Math.imul((k), (_s0.fact((((k) - (1)) | 0)))))));
    (this.value = _s0.fact(_s0.n));
  }
}
const Factorial__Value = (_s0) => _s0.value;
const Factorial__Of = (_s0) => (k) => _s0.fact(k);
const f = new Factorial(5);
console.log(Factorial__Value(f));
console.log(Factorial__Of(f)(6));
