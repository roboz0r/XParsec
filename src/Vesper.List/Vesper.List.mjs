// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s1 = this;
    return ((_s1.started ? ((_m60) => {
      if ((_m60.tag === 0)) {
        return undefined;
      }
      if ((_m60.tag === 1)) {
        const t = _m60.Tail;
        return (_s1.cursor = t);
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor) : (_s1.started = true)), ((_m61) => {
      if ((_m61.tag === 0)) {
        return false;
      }
      if ((_m61.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor));
  }
  Current() {
    const _s1 = this;
    return ((_m62) => {
      if ((_m62.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m62.tag === 1)) {
        const h = _m62.Head;
        return h;
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor);
  }
}
export class List {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Vesper.Collections.List`1";
  }
  cases() {
    return ["Empty", "Cons"];
  }
  *[Symbol.iterator]() {
    const _s0 = this;
    const _e63 = new ListEnumerator(_s0);
    while (_e63.MoveNext()) {
      yield _e63.Current();
    }
  }
}
export class List_Empty extends List {
  constructor() {
    super(0);
  }
}
export class List_Cons extends List {
  constructor(Head, Tail) {
    super(1);
    this.Head = Head;
    this.Tail = Tail;
  }
}
export const fold = (folder, state, list) => ((_m64) => {
  if ((_m64.tag === 0)) {
    return state;
  }
  if ((_m64.tag === 1)) {
    const h = _m64.Head;
    const t = _m64.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m65) => {
  if ((_m65.tag === 0)) {
    return true;
  }
  if ((_m65.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m66) => {
  if ((_m66.tag === 0)) {
    return 0;
  }
  if ((_m66.tag === 1)) {
    const t = _m66.Tail;
    return ((_s55) => (((1) + (_s55)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m67) => {
  if ((_m67.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m67.tag === 1)) {
    const h = _m67.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m68) => {
  if ((_m68.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m68.tag === 1)) {
    const t = _m68.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m69) => {
  if ((_m69.tag === 0)) {
    return new List_Empty();
  }
  if ((_m69.tag === 1)) {
    const h = _m69.Head;
    const t = _m69.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m70) => {
  if ((_m70.tag === 0)) {
    return new List_Empty();
  }
  if ((_m70.tag === 1)) {
    const h = _m70.Head;
    const t = _m70.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m71) => {
  if ((_m71.tag === 0)) {
    return list2;
  }
  if ((_m71.tag === 1)) {
    const h = _m71.Head;
    const t = _m71.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m72) => {
  if ((_m72.tag === 0)) {
    return new List_Empty();
  }
  if ((_m72.tag === 1)) {
    const h = _m72.Head;
    const t = _m72.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
