const twoUses = (x) => ((g) => [g(x), g("a")])((y) => y);
const entangled = (x) => ((g) => [g(1), g("c")])((y) => [y, x]);
const oneUse = (x) => ((g) => [g(x), g(2)])((y) => y);
((_m20) => {
  {
    const a1 = _m20[0];
    const a2 = _m20[1];
    return console.log((("") + (a1) + (" ") + (a2)));
  }
  throw new Error("The match cases were incomplete");
})(twoUses(1));
((_m21) => {
  {
    const c1 = _m21[0][0];
    const c1x = _m21[0][1];
    const c2 = _m21[1][0];
    const c2x = _m21[1][1];
    return console.log((("") + (c1) + (" ") + (c1x) + (" ") + (c2) + (" ") + (c2x)));
  }
  throw new Error("The match cases were incomplete");
})(entangled("outer"));
((_m22) => {
  {
    const o1 = _m22[0];
    const o2 = _m22[1];
    return console.log((("") + (o1) + (" ") + (o2)));
  }
  throw new Error("The match cases were incomplete");
})(oneUse(3));
