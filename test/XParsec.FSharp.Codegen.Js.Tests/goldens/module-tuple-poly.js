const _let8 = [(x) => x, (y) => y];
const f = _let8[0];
const g = _let8[1];
const a = f(1);
const b = f("a");
const c = g(true);
const d = g("x");
console.log((("") + (a) + (" ") + (b) + (" ") + ((c ? "true" : "false")) + (" ") + (d)));
