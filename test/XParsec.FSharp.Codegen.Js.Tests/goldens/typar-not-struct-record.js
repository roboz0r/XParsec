class Point {
  constructor(X, Y) {
    this.X = X;
    this.Y = Y;
  }
}
const onlyRef = (x) => x;
const p = onlyRef(new Point(1, 2));
(void (p));
