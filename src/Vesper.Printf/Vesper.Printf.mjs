export class Doc {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Vesper.Doc";
  }
  cases() {
    return ["Text", "Line", "Cat", "Nest", "Group"];
  }
}
export class Doc_Text extends Doc {
  constructor(Item) {
    super(0);
    this.Item = Item;
  }
}
export class Doc_Line extends Doc {
  constructor(Item) {
    super(1);
    this.Item = Item;
  }
}
export class Doc_Cat extends Doc {
  constructor(Item) {
    super(2);
    this.Item = Item;
  }
}
export class Doc_Nest extends Doc {
  constructor(Item1, Item2) {
    super(3);
    this.Item1 = Item1;
    this.Item2 = Item2;
  }
}
export class Doc_Group extends Doc {
  constructor(Item1, Item2) {
    super(4);
    this.Item1 = Item1;
    this.Item2 = Item2;
  }
}
export const cat = (a, b) => ((a) + (b));
export const strEq = (a, b) => ((a) === (b));
export const strLen = (s) => ((s).length);
export const notB = (x) => (!(x));
export const intEq = (a, b) => ((a) === (b));
export const intLt = (a, b) => ((a) < (b));
export const intLe = (a, b) => ((a) <= (b));
export const intGe = (a, b) => ((a) >= (b));
export const intGt = (a, b) => ((a) > (b));
export const inc = (a) => ((a) + 1);
export const dec = (a) => ((a) - 1);
export const addI = (a, b) => ((a) + (b));
export const typeOf = (v) => (typeof (v));
export const isArr = (v) => (Array.isArray((v)));
export const isUndef = (v) => ((v) === undefined);
export const isNullV = (v) => ((v) === null);
export const toStr = (v) => (String((v)));
export const lenOf = (v) => ((v).length);
export const elem = (v, i) => ((v)[(i)]);
export const field = (v, k) => ((v)[(k)]);
export const sElem = (a, i) => ((a)[(i)]);
export const keysOf = (v) => (Object.keys((v)));
export const tagOf = (v) => ((v).tag);
export const casesOf = (v) => ((v).cases());
export const mkBudget = (size) => ([(size)]);
export const getB = (b) => ((b)[0]);
export const setB = (b, v) => ((b)[0] = (v));
export const emptyDocs = ([]);
export const append = (a, d) => ([...(a), (d)]);
export const dLen = (a) => ((a).length);
export const dGet = (a, i) => ((a)[(i)]);
export const flatWidth = (d) => ((_m6541) => {
  if ((_m6541.tag === 0)) {
    const s = _m6541.Item;
    return strLen(s);
  }
  if ((_m6541.tag === 1)) {
    const flat = _m6541.Item;
    return strLen(flat);
  }
  if ((_m6541.tag === 2)) {
    const kids = _m6541.Item;
    return catWidth(kids, 0, 0);
  }
  if ((_m6541.tag === 3)) {
    const inner = _m6541.Item2;
    return flatWidth(inner);
  }
  if ((_m6541.tag === 4)) {
    const inner = _m6541.Item1;
    const parens = _m6541.Item2;
    return addI(flatWidth(inner), (parens ? 2 : 0));
  }
  throw new Error("The match cases were incomplete");
})(d);
export const catWidth = (kids, i, acc) => {
  while (true) {
    if (intGe(i, dLen(kids))) {
      return acc;
    } else {
      const _tc0 = kids;
      const _tc1 = inc(i);
      const _tc2 = addI(acc, flatWidth(dGet(kids, i)));
      kids = _tc0;
      i = _tc1;
      acc = _tc2;
      continue;
    }
  }
};
export const mkStrCell = (s) => ([(s)]);
export const getStr = (c) => ((c)[0]);
export const emit = (out, s) => ((out)[0] = (out)[0] + (s));
export const nSpaces = (n) => (' '.repeat((n)));
export const renderDoc = (d, out, indent, broken, col, width) => ((_m7840) => {
  if ((_m7840.tag === 0)) {
    const s = _m7840.Item;
    return (emit(out, s), addI(col, strLen(s)));
  }
  if ((_m7840.tag === 1)) {
    const flat = _m7840.Item;
    return (broken ? (emit(out, "\n"), emit(out, nSpaces(indent)), indent) : (emit(out, flat), addI(col, strLen(flat))));
  }
  if ((_m7840.tag === 3)) {
    const i = _m7840.Item1;
    const inner = _m7840.Item2;
    return renderDoc(inner, out, addI(indent, i), broken, col, width);
  }
  if ((_m7840.tag === 2)) {
    const kids = _m7840.Item;
    return renderCat(kids, 0, out, indent, broken, col, width);
  }
  if ((_m7840.tag === 4)) {
    const inner = _m7840.Item1;
    const parens = _m7840.Item2;
    return ((openCol) => ((groupBroken) => ((parens ? emit(out, "(") : undefined), ((endCol) => (parens ? (emit(out, ")"), inc(endCol)) : endCol))(renderDoc(inner, out, indent, groupBroken, openCol, width))))((intEq(width, 0) ? false : intGt(addI(openCol, flatWidth(inner)), width))))((parens ? inc(col) : col));
  }
  throw new Error("The match cases were incomplete");
})(d);
export const renderCat = (kids, i, out, indent, broken, col, width) => {
  while (true) {
    if (intGe(i, dLen(kids))) {
      return col;
    } else {
      const col1 = renderDoc(dGet(kids, i), out, indent, broken, col, width);
      const _tc0 = kids;
      const _tc1 = inc(i);
      const _tc2 = out;
      const _tc3 = indent;
      const _tc4 = broken;
      const _tc5 = col1;
      const _tc6 = width;
      kids = _tc0;
      i = _tc1;
      out = _tc2;
      indent = _tc3;
      broken = _tc4;
      col = _tc5;
      width = _tc6;
      continue;
    }
  }
};
export const fmtString = (s) => ((len) => ((out) => ((i) => ((() => {
  while (intLt(i, len)) {
    (strEq(((s)[(i)]), "\\") ? (out = cat(out, "\\\\")) : (strEq(((s)[(i)]), "\n") ? (out = cat(out, "\\n")) : (strEq(((s)[(i)]), "\r") ? (out = cat(out, "\\r")) : (strEq(((s)[(i)]), "\t") ? (out = cat(out, "\\t")) : (strEq(((s)[(i)]), "\"") ? (out = cat(out, "\\\"")) : (out = cat(out, ((s)[(i)]))))))));
    (i = inc(i));
  }
})(), cat(out, "\"")))(0))("\""))(lenOf(s));
export const isUnion = (v) => (isNullV(v) ? false : (notB(strEq(typeOf(v), "object")) ? false : (isArr(v) ? false : (notB(strEq(typeOf(field(v, "tag")), "number")) ? false : strEq(typeOf(field(v, "cases")), "function")))));
export const isVesperList = (v) => (notB(isUnion(v)) ? false : ((cs) => (notB(intEq(lenOf(cs), 2)) ? false : (notB(strEq(sElem(cs, 0), "Empty")) ? false : strEq(sElem(cs, 1), "Cons"))))(casesOf(v)));
export const isPayloadUnion = (v) => (notB(isUnion(v)) ? false : (isVesperList(v) ? false : intGt(lenOf(keysOf(v)), 1)));
export const fmtValue = (v, budget) => (isUndef(v) ? new Doc_Text("()") : (isNullV(v) ? new Doc_Text("null") : ((t) => (strEq(t, "number") ? (setB(budget, dec(getB(budget))), new Doc_Text(toStr(v))) : (strEq(t, "bigint") ? (setB(budget, dec(getB(budget))), new Doc_Text(cat(toStr(v), "L"))) : (strEq(t, "boolean") ? (setB(budget, dec(getB(budget))), new Doc_Text(toStr(v))) : (strEq(t, "string") ? (setB(budget, dec(getB(budget))), new Doc_Text(fmtString(v))) : (isArr(v) ? fmtTuple(v, budget) : (isVesperList(v) ? fmtList(v, budget) : (isUnion(v) ? fmtUnion(v, budget) : fmtRecord(v, budget)))))))))(typeOf(v))));
export const fmtArg = (v, budget) => ((d) => (isPayloadUnion(v) ? new Doc_Group(d, true) : d))(fmtValue(v, budget));
export const fmtTuple = (v, budget) => ((len) => ((inner) => ((i) => ((() => {
  while (intLt(i, len)) {
    (intGt(i, 0) ? ((inner = append(inner, new Doc_Text(","))), (inner = append(inner, new Doc_Line(" ")))) : undefined);
    (inner = append(inner, fmtValue(elem(v, i), budget)));
    (i = inc(i));
  }
})(), ((outer) => ((outer = append(outer, new Doc_Text("("))), (outer = append(outer, new Doc_Nest(1, new Doc_Cat(inner)))), (outer = append(outer, new Doc_Text(")"))), new Doc_Group(new Doc_Cat(outer), false)))(emptyDocs)))(0))(emptyDocs))(lenOf(v));
export const fmtList = (v, budget) => ((elems) => ((elems = append(elems, new Doc_Line(""))), ((cur) => ((i) => ((first) => ((go) => ((() => {
  while (go) {
    (notB(intEq(tagOf(cur), 1)) ? (go = false) : ((notB(first) ? ((elems = append(elems, new Doc_Text(";"))), (elems = append(elems, new Doc_Line(" ")))) : undefined), (intGe(i, 100) ? ((elems = append(elems, new Doc_Text("..."))), (go = false)) : (intLe(getB(budget), 0) ? ((elems = append(elems, new Doc_Text("..."))), (go = false)) : ((elems = append(elems, fmtValue(field(cur, "Head"), budget))), (first = false), (cur = field(cur, "Tail")), (i = inc(i)))))));
  }
})(), ((outer) => ((outer = append(outer, new Doc_Text("["))), (outer = append(outer, new Doc_Nest(2, new Doc_Cat(elems)))), (outer = append(outer, new Doc_Line(""))), (outer = append(outer, new Doc_Text("]"))), new Doc_Group(new Doc_Cat(outer), false)))(emptyDocs)))(true))(true))(0))(v)))(emptyDocs);
export const fmtUnion = (v, budget) => ((name) => ((ks) => ((klen) => ((fcount) => ((j) => ((() => {
  while (intLt(j, klen)) {
    (notB(strEq(sElem(ks, j), "tag")) ? (fcount = inc(fcount)) : undefined);
    (j = inc(j));
  }
})(), (intEq(fcount, 0) ? new Doc_Text(name) : (intEq(fcount, 1) ? ((fk) => ((j2) => ((() => {
  while (intLt(j2, klen)) {
    (notB(strEq(sElem(ks, j2), "tag")) ? (fk = sElem(ks, j2)) : undefined);
    (j2 = inc(j2));
  }
})(), ((outer) => ((outer = append(outer, new Doc_Text(cat(name, " ")))), (outer = append(outer, fmtArg(field(v, fk), budget))), new Doc_Cat(outer)))(emptyDocs)))(0))("") : ((inner) => ((firstF) => ((j3) => ((() => {
  while (intLt(j3, klen)) {
    const k = sElem(ks, j3);
    (notB(strEq(k, "tag")) ? ((notB(firstF) ? ((inner = append(inner, new Doc_Text(","))), (inner = append(inner, new Doc_Line(" ")))) : undefined), (inner = append(inner, fmtValue(field(v, k), budget))), (firstF = false)) : undefined);
    (j3 = inc(j3));
  }
})(), ((grp) => ((grp = append(grp, new Doc_Text("("))), (grp = append(grp, new Doc_Nest(1, new Doc_Cat(inner)))), (grp = append(grp, new Doc_Text(")"))), ((outer) => ((outer = append(outer, new Doc_Text(cat(name, " ")))), (outer = append(outer, new Doc_Group(new Doc_Cat(grp), false))), new Doc_Cat(outer)))(emptyDocs)))(emptyDocs)))(0))(true))(emptyDocs)))))(0))(0))(lenOf(ks)))(keysOf(v)))(sElem(casesOf(v), tagOf(v)));
export const fmtRecord = (v, budget) => ((ks) => ((klen) => (intEq(klen, 0) ? new Doc_Text("{ }") : ((k0) => ((inner) => ((inner = append(inner, fmtValue(field(v, k0), budget))), ((jr) => ((() => {
  while (intLt(jr, klen)) {
    const k = sElem(ks, jr);
    (inner = append(inner, new Doc_Text(";")));
    (inner = append(inner, new Doc_Line(" ")));
    (inner = append(inner, new Doc_Text(cat(k, " = "))));
    (inner = append(inner, fmtValue(field(v, k), budget)));
    (jr = inc(jr));
  }
})(), ((outer) => ((outer = append(outer, new Doc_Text(cat(cat("{ ", k0), " = ")))), (outer = append(outer, new Doc_Nest(2, new Doc_Cat(inner)))), (outer = append(outer, new Doc_Text(" }"))), new Doc_Group(new Doc_Cat(outer), false)))(emptyDocs)))(1)))(emptyDocs))(sElem(ks, 0))))(lenOf(ks)))(keysOf(v));
export const renderRoot = (d, out, width) => (renderDoc(new Doc_Group(d, false), out, 0, false, 0, width), getStr(out));
export const structuralFormat = (value, width, size) => renderRoot(fmtValue(value, mkBudget(size)), mkStrCell(""), width);
