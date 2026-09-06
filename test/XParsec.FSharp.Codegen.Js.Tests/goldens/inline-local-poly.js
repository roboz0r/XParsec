const twoUses = (x) => ((g) => [g(x), g("a")])((y) => y);
const annotated = (x) => ((g) => [g(x), g("b")])((y) => y);
const entangled = (x) => ((g) => [g(1), g("c")])((y) => [y, x]);
const served = (x) => ((g) => [g(x), g("e")])((y) => y);
const consume = () => ((_s44) => [_s44(5), _s44("e")])((_s45) => _s45);
((_m55) => {
  {
    const a1 = _m55[0];
    const a2 = _m55[1];
    return console.log((("") + (a1) + (" ") + (a2)));
  }
  throw new Error("The match cases were incomplete");
})(((_s47) => [_s47(1), _s47("a")])((_s48) => _s48));
((_m56) => {
  {
    const b1 = _m56[0];
    const b2 = _m56[1];
    return console.log((("") + (b1) + (" ") + (b2)));
  }
  throw new Error("The match cases were incomplete");
})(((_s50) => [_s50(2), _s50("b")])((_s51) => _s51));
((_m57) => {
  {
    const c1 = _m57[0][0];
    const c1x = _m57[0][1];
    const c2 = _m57[1][0];
    const c2x = _m57[1][1];
    return console.log((("") + (c1) + (" ") + (c1x) + (" ") + (c2) + (" ") + (c2x)));
  }
  throw new Error("The match cases were incomplete");
})(((_s53) => [_s53(1), _s53("c")])((_s54) => [_s54, "outer"]));
((_m58) => {
  {
    const e1 = _m58[0];
    const e2 = _m58[1];
    return console.log((("") + (e1) + (" ") + (e2)));
  }
  throw new Error("The match cases were incomplete");
})(consume());
