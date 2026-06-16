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
export const fold = (folder) => (state) => (list) => ((_m2734) => {
  if ((_m2734.tag === 0)) {
    return state;
  }
  if ((_m2734.tag === 1)) {
    const h = _m2734.Head;
    const t = _m2734.Tail;
    return fold(folder)(folder(state)(h))(t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m2873) => {
  if ((_m2873.tag === 0)) {
    return true;
  }
  if ((_m2873.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m2988) => {
  if ((_m2988.tag === 0)) {
    return 0;
  }
  if ((_m2988.tag === 1)) {
    const t = _m2988.Tail;
    return ((_s1) => (((1) + (_s1)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m3100) => {
  if ((_m3100.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3100.tag === 1)) {
    const h = _m3100.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m3241) => {
  if ((_m3241.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3241.tag === 1)) {
    const t = _m3241.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping) => (list) => ((_m3405) => {
  if ((_m3405.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3405.tag === 1)) {
    const h = _m3405.Head;
    const t = _m3405.Tail;
    return new List_Cons(mapping(h), map(mapping)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate) => (list) => ((_m3567) => {
  if ((_m3567.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3567.tag === 1)) {
    const h = _m3567.Head;
    const t = _m3567.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate)(t)) : filter(predicate)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1) => (list2) => ((_m3820) => {
  if ((_m3820.tag === 0)) {
    return list2;
  }
  if ((_m3820.tag === 1)) {
    const h = _m3820.Head;
    const t = _m3820.Tail;
    return new List_Cons(h, append(t)(list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m3952) => {
  if ((_m3952.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3952.tag === 1)) {
    const h = _m3952.Head;
    const t = _m3952.Tail;
    return append(rev(t))(new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
