class List {
  constructor(tag) {
    this.tag = tag;
  }
  cases() {
    return ["Empty", "Cons"];
  }
}
class List_Empty extends List {
  constructor() {
    super(0);
  }
}
class List_Cons extends List {
  constructor(Head, Tail) {
    super(1);
    this.Head = Head;
    this.Tail = Tail;
  }
}
export const fold = (folder) => (state) => (list) => ((_m2783) => {
  if ((_m2783.tag === 0)) {
    return state;
  }
  if ((_m2783.tag === 1)) {
    const h = _m2783.Head;
    const t = _m2783.Tail;
    return fold(folder)(folder(state)(h))(t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m2927) => {
  if ((_m2927.tag === 0)) {
    return true;
  }
  if ((_m2927.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m3047) => {
  if ((_m3047.tag === 0)) {
    return 0;
  }
  if ((_m3047.tag === 1)) {
    const t = _m3047.Tail;
    return ((_s1) => (((1) + (_s1)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m3164) => {
  if ((_m3164.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3164.tag === 1)) {
    const h = _m3164.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m3310) => {
  if ((_m3310.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3310.tag === 1)) {
    const t = _m3310.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping) => (list) => ((_m3479) => {
  if ((_m3479.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3479.tag === 1)) {
    const h = _m3479.Head;
    const t = _m3479.Tail;
    return new List_Cons(mapping(h), map(mapping)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate) => (list) => ((_m3646) => {
  if ((_m3646.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3646.tag === 1)) {
    const h = _m3646.Head;
    const t = _m3646.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate)(t)) : filter(predicate)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1) => (list2) => ((_m3908) => {
  if ((_m3908.tag === 0)) {
    return list2;
  }
  if ((_m3908.tag === 1)) {
    const h = _m3908.Head;
    const t = _m3908.Tail;
    return new List_Cons(h, append(t)(list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m4045) => {
  if ((_m4045.tag === 0)) {
    return new List_Empty();
  }
  if ((_m4045.tag === 1)) {
    const h = _m4045.Head;
    const t = _m4045.Tail;
    return append(rev(t))(new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
