class Counter {
  constructor() {
  }
}
const Counter__Add = (_s0) => (k) => (Counter.total = (((Counter.total) + (k)) | 0));
const Counter__Get = (_s0) => Counter.total;
(Counter.total = 0);
const a = new Counter();
const b = new Counter();
Counter__Add(a)(3);
Counter__Add(b)(4);
console.log(Counter__Get(a));
console.log(Counter__Get(b));
