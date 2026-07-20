class Counter {
  constructor(step) {
    this.step = step;
    const _s348 = this;
    (this.count = 0);
    (this.bump = (k) => (_s348.count = ((_s0) => ((_s1) => (((_s0) + (_s1)) | 0))(((_s3) => (Math.imul((k), (_s3))))(_s348.step)))(_s348.count)));
  }
}
const Counter__Bump = (_s348) => (k) => (_s348.bump(k), _s348.count);
const c = new Counter(2);
console.log(Counter__Bump(c)(3));
console.log(Counter__Bump(c)(4));
