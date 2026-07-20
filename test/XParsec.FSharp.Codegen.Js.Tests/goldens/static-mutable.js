class Counter {
  constructor() {
  }
}
const Counter__Add = (_s646) => (k) => (Counter.total = ((_s0) => (((_s0) + (k)) | 0))(Counter.total));
const Counter__Get = (_s646) => Counter.total;
(Counter.total = 0);
const a = new Counter();
const b = new Counter();
Counter__Add(a)(3);
Counter__Add(b)(4);
console.log(Counter__Get(a));
console.log(Counter__Get(b));
