class Cell {
  constructor(x, n) {
    this.x = x;
    this.n = n;
    const _s275 = this;
    (this.k = ((_s0) => (((_s0) + (1)) | 0))(((_s2) => (Math.imul((_s2), (2))))(_s275.n)));
    (this.get = (_u337) => _s275.x);
  }
}
const Cell__K = (_s275) => _s275.k;
const Cell__X = (_s275) => _s275.get(undefined);
const s = new Cell("hi", 3);
const i = new Cell(9, 10);
console.log((("") + (Cell__K(s)) + (" ") + (Cell__X(s))));
console.log((("") + (Cell__K(i)) + (" ") + (Cell__X(i))));
