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
export const fold = (folder) => (state) => (list) => ((_m2832) => {
  if ((_m2832.tag === 0)) {
    return state;
  }
  if ((_m2832.tag === 1)) {
    const h = _m2832.Head;
    const t = _m2832.Tail;
    return fold(folder)(folder(state)(h))(t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m2971) => {
  if ((_m2971.tag === 0)) {
    return true;
  }
  if ((_m2971.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m3086) => {
  if ((_m3086.tag === 0)) {
    return 0;
  }
  if ((_m3086.tag === 1)) {
    const t = _m3086.Tail;
    return ((_s1) => (((1) + (_s1)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m3198) => {
  if ((_m3198.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3198.tag === 1)) {
    const h = _m3198.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m3378) => {
  if ((_m3378.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3378.tag === 1)) {
    const t = _m3378.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping) => (list) => ((_m3586) => {
  if ((_m3586.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3586.tag === 1)) {
    const h = _m3586.Head;
    const t = _m3586.Tail;
    return new List_Cons(mapping(h), map(mapping)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate) => (list) => ((_m3748) => {
  if ((_m3748.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3748.tag === 1)) {
    const h = _m3748.Head;
    const t = _m3748.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate)(t)) : filter(predicate)(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1) => (list2) => ((_m4001) => {
  if ((_m4001.tag === 0)) {
    return list2;
  }
  if ((_m4001.tag === 1)) {
    const h = _m4001.Head;
    const t = _m4001.Tail;
    return new List_Cons(h, append(t)(list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m4133) => {
  if ((_m4133.tag === 0)) {
    return new List_Empty();
  }
  if ((_m4133.tag === 1)) {
    const h = _m4133.Head;
    const t = _m4133.Tail;
    return append(rev(t))(new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
