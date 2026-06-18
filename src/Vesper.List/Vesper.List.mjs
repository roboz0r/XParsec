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
export const fold = (folder, state, list) => ((_m2745) => {
  if ((_m2745.tag === 0)) {
    return state;
  }
  if ((_m2745.tag === 1)) {
    const h = _m2745.Head;
    const t = _m2745.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m2889) => {
  if ((_m2889.tag === 0)) {
    return true;
  }
  if ((_m2889.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m3009) => {
  if ((_m3009.tag === 0)) {
    return 0;
  }
  if ((_m3009.tag === 1)) {
    const t = _m3009.Tail;
    return ((_s1) => (((1) + (_s1)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m3126) => {
  if ((_m3126.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3126.tag === 1)) {
    const h = _m3126.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m3272) => {
  if ((_m3272.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3272.tag === 1)) {
    const t = _m3272.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m3441) => {
  if ((_m3441.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3441.tag === 1)) {
    const h = _m3441.Head;
    const t = _m3441.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m3608) => {
  if ((_m3608.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3608.tag === 1)) {
    const h = _m3608.Head;
    const t = _m3608.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m3870) => {
  if ((_m3870.tag === 0)) {
    return list2;
  }
  if ((_m3870.tag === 1)) {
    const h = _m3870.Head;
    const t = _m3870.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m4007) => {
  if ((_m4007.tag === 0)) {
    return new List_Empty();
  }
  if ((_m4007.tag === 1)) {
    const h = _m4007.Head;
    const t = _m4007.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
