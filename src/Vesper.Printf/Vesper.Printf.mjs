export const cat = (a) => (b) => ((a) + (b));
export const strEq = (a) => (b) => ((a) === (b));
export const notB = (x) => (!(x));
export const intEq = (a) => (b) => ((a) === (b));
export const intLt = (a) => (b) => ((a) < (b));
export const intLe = (a) => (b) => ((a) <= (b));
export const intGe = (a) => (b) => ((a) >= (b));
export const intGt = (a) => (b) => ((a) > (b));
export const inc = (a) => ((a) + 1);
export const dec = (a) => ((a) - 1);
export const typeOf = (v) => (typeof (v));
export const isArr = (v) => (Array.isArray((v)));
export const isUndef = (v) => ((v) === undefined);
export const isNullV = (v) => ((v) === null);
export const toStr = (v) => (String((v)));
export const lenOf = (v) => ((v).length);
export const elem = (v) => (i) => ((v)[(i)]);
export const field = (v) => (k) => ((v)[(k)]);
export const sElem = (a) => (i) => ((a)[(i)]);
export const keysOf = (v) => (Object.keys((v)));
export const tagOf = (v) => ((v).tag);
export const casesOf = (v) => ((v).cases());
export const mkBudget = (size) => ([(size)]);
export const getB = (b) => ((b)[0]);
export const setB = (b) => (v) => ((b)[0] = (v));
export const fmtString = (s) => ((len) => ((out) => ((i) => ((() => {
  while (intLt(i)(len)) {
    (strEq(((s)[(i)]))("\\") ? (out = cat(out)("\\\\")) : (strEq(((s)[(i)]))("\n") ? (out = cat(out)("\\n")) : (strEq(((s)[(i)]))("\r") ? (out = cat(out)("\\r")) : (strEq(((s)[(i)]))("\t") ? (out = cat(out)("\\t")) : (strEq(((s)[(i)]))("\"") ? (out = cat(out)("\\\"")) : (out = cat(out)(((s)[(i)]))))))));
    (i = inc(i));
  }
})(), cat(out)("\"")))(0))("\""))(lenOf(s));
export const isUnion = (v) => (isNullV(v) ? false : (notB(strEq(typeOf(v))("object")) ? false : (isArr(v) ? false : (notB(strEq(typeOf(field(v)("tag")))("number")) ? false : strEq(typeOf(field(v)("cases")))("function")))));
export const isVesperList = (v) => (notB(isUnion(v)) ? false : ((cs) => (notB(intEq(lenOf(cs))(2)) ? false : (notB(strEq(sElem(cs)(0))("Empty")) ? false : strEq(sElem(cs)(1))("Cons"))))(casesOf(v)));
export const isPayloadUnion = (v) => (notB(isUnion(v)) ? false : (isVesperList(v) ? false : intGt(lenOf(keysOf(v)))(1)));
export const fmtValue = (v) => (budget) => (isUndef(v) ? "()" : (isNullV(v) ? "null" : ((t) => (strEq(t)("number") ? (setB(budget)(dec(getB(budget))), toStr(v)) : (strEq(t)("bigint") ? (setB(budget)(dec(getB(budget))), cat(toStr(v))("L")) : (strEq(t)("boolean") ? (setB(budget)(dec(getB(budget))), toStr(v)) : (strEq(t)("string") ? (setB(budget)(dec(getB(budget))), fmtString(v)) : (isArr(v) ? ((len) => ((out) => ((i) => ((() => {
  while (intLt(i)(len)) {
    const s = fmtValue(elem(v)(i))(budget);
    (intEq(i)(0) ? (out = cat(out)(s)) : (out = cat(cat(out)(", "))(s)));
    (i = inc(i));
  }
})(), cat(out)(")")))(0))("("))(lenOf(v)) : (isVesperList(v) ? ((out) => ((cur) => ((i) => ((first) => ((go) => ((() => {
  while (go) {
    (notB(intEq(tagOf(cur))(1)) ? (go = false) : (intGe(i)(100) ? ((out = (first ? cat(out)("...") : cat(out)("; ..."))), (go = false)) : (intLe(getB(budget))(0) ? ((out = (first ? cat(out)("...") : cat(out)("; ..."))), (go = false)) : ((h) => ((first ? (out = cat(out)(h)) : (out = cat(cat(out)("; "))(h))), (first = false), (cur = field(cur)("Tail")), (i = inc(i))))(fmtValue(field(cur)("Head"))(budget)))));
  }
})(), cat(out)("]")))(true))(true))(0))(v))("[") : (isUnion(v) ? ((name) => ((ks) => ((klen) => ((fcount) => ((j) => ((() => {
  while (intLt(j)(klen)) {
    (notB(strEq(sElem(ks)(j))("tag")) ? (fcount = inc(fcount)) : undefined);
    (j = inc(j));
  }
})(), (intEq(fcount)(0) ? name : (intEq(fcount)(1) ? ((fk) => ((j2) => ((() => {
  while (intLt(j2)(klen)) {
    (notB(strEq(sElem(ks)(j2))("tag")) ? (fk = sElem(ks)(j2)) : undefined);
    (j2 = inc(j2));
  }
})(), ((child) => ((childStr) => (isPayloadUnion(child) ? cat(cat(cat(name)(" ("))(childStr))(")") : cat(cat(name)(" "))(childStr)))(fmtValue(child)(budget)))(field(v)(fk))))(0))("") : ((out) => ((firstF) => ((j3) => ((() => {
  while (intLt(j3)(klen)) {
    const k = sElem(ks)(j3);
    (notB(strEq(k)("tag")) ? ((s) => ((firstF ? (out = cat(out)(s)) : (out = cat(cat(out)(", "))(s))), (firstF = false)))(fmtValue(field(v)(k))(budget)) : undefined);
    (j3 = inc(j3));
  }
})(), cat(out)(")")))(0))(true))(cat(name)(" ("))))))(0))(0))(lenOf(ks)))(keysOf(v)))(sElem(casesOf(v))(tagOf(v))) : ((ks) => ((klen) => ((out) => ((firstR) => ((jr) => ((() => {
  while (intLt(jr)(klen)) {
    const k = sElem(ks)(jr);
    const s = cat(cat(k)(" = "))(fmtValue(field(v)(k))(budget));
    (firstR ? (out = cat(out)(s)) : (out = cat(cat(out)("; "))(s)));
    (firstR = false);
    (jr = inc(jr));
  }
})(), cat(out)(" }")))(0))(true))("{ "))(lenOf(ks)))(keysOf(v))))))))))(typeOf(v))));
export const structuralFormat = (value) => (width) => (size) => fmtValue(value)(mkBudget(size));
