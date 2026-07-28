// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s1 = this;
    return ((_s1.started ? ((_m46) => {
      if ((_m46.tag === 0)) {
        return undefined;
      }
      if ((_m46.tag === 1)) {
        const t = _m46.Tail;
        return (_s1.cursor = t);
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor) : (_s1.started = true)), ((_m47) => {
      if ((_m47.tag === 0)) {
        return false;
      }
      if ((_m47.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor));
  }
  Current() {
    const _s1 = this;
    return ((_m48) => {
      if ((_m48.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m48.tag === 1)) {
        const h = _m48.Head;
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
    const _e49 = new ListEnumerator(_s0);
    while (_e49.MoveNext()) {
      yield _e49.Current();
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
export const fold = (folder, state, list) => ((_m50) => {
  if ((_m50.tag === 0)) {
    return state;
  }
  if ((_m50.tag === 1)) {
    const h = _m50.Head;
    const t = _m50.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m51) => {
  if ((_m51.tag === 0)) {
    return true;
  }
  if ((_m51.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m52) => {
  if ((_m52.tag === 0)) {
    return 0;
  }
  if ((_m52.tag === 1)) {
    const t = _m52.Tail;
    return ((_s14) => (((1) + (_s14)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m53) => {
  if ((_m53.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m53.tag === 1)) {
    const h = _m53.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m54) => {
  if ((_m54.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m54.tag === 1)) {
    const t = _m54.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m55) => {
  if ((_m55.tag === 0)) {
    return new List_Empty();
  }
  if ((_m55.tag === 1)) {
    const h = _m55.Head;
    const t = _m55.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m56) => {
  if ((_m56.tag === 0)) {
    return new List_Empty();
  }
  if ((_m56.tag === 1)) {
    const h = _m56.Head;
    const t = _m56.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m57) => {
  if ((_m57.tag === 0)) {
    return list2;
  }
  if ((_m57.tag === 1)) {
    const h = _m57.Head;
    const t = _m57.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m58) => {
  if ((_m58.tag === 0)) {
    return new List_Empty();
  }
  if ((_m58.tag === 1)) {
    const h = _m58.Head;
    const t = _m58.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
