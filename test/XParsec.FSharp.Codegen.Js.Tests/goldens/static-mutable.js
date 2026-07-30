class Counter {
  constructor() {
  }
}
const Counter__Add = (_s0) => (k) => (Counter.total = ((_s7) => (((_s7) + (k)) | 0))(Counter.total));
const Counter__Get = (_s0) => Counter.total;
(Counter.total = 0);
const a = new Counter();
const b = new Counter();
Counter__Add(a)(3);
Counter__Add(b)(4);
console.log(Counter__Get(a));
console.log(Counter__Get(b));
