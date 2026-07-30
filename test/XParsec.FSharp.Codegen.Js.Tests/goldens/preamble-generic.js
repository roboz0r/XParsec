class Cell {
  constructor(x, n) {
    this.x = x;
    this.n = n;
    const _s0 = this;
    (this.k = ((_s11) => (((_s11) + (1)) | 0))(((_s9) => (Math.imul((_s9), (2))))(_s0.n)));
    (this.get = (_u13) => _s0.x);
  }
}
const Cell__K = (_s0) => _s0.k;
const Cell__X = (_s0) => _s0.get(undefined);
const s = new Cell("hi", 3);
const i = new Cell(9, 10);
console.log((("") + (Cell__K(s)) + (" ") + (Cell__X(s))));
console.log((("") + (Cell__K(i)) + (" ") + (Cell__X(i))));
