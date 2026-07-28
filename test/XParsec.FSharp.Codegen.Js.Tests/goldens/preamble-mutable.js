class Counter {
  constructor(step) {
    this.step = step;
    const _s0 = this;
    (this.count = 0);
    (this.bump = (k) => (_s0.count = ((_s5) => ((_s4) => (((_s5) + (_s4)) | 0))(((_s2) => (Math.imul((k), (_s2))))(_s0.step)))(_s0.count)));
  }
}
const Counter__Bump = (_s0) => (k) => (_s0.bump(k), _s0.count);
const c = new Counter(2);
console.log(Counter__Bump(c)(3));
console.log(Counter__Bump(c)(4));
