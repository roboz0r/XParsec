// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s1 = this;
    return ((_s1.started ? ((_m1709) => {
      if ((_m1709.tag === 0)) {
        return undefined;
      }
      if ((_m1709.tag === 1)) {
        const t = _m1709.Tail;
        return (_s1.cursor = t);
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor) : (_s1.started = true)), ((_m1877) => {
      if ((_m1877.tag === 0)) {
        return false;
      }
      if ((_m1877.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s1.cursor));
  }
  Current() {
    const _s1 = this;
    return ((_m2008) => {
      if ((_m2008.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m2008.tag === 1)) {
        const h = _m2008.Head;
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
    const _e732 = new ListEnumerator(_s0);
    while (_e732.MoveNext()) {
      yield _e732.Current();
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
export const fold = (folder, state, list) => ((_m2548) => {
  if ((_m2548.tag === 0)) {
    return state;
  }
  if ((_m2548.tag === 1)) {
    const h = _m2548.Head;
    const t = _m2548.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m2692) => {
  if ((_m2692.tag === 0)) {
    return true;
  }
  if ((_m2692.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m2812) => {
  if ((_m2812.tag === 0)) {
    return 0;
  }
  if ((_m2812.tag === 1)) {
    const t = _m2812.Tail;
    return ((_s14) => (((1) + (_s14)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m2929) => {
  if ((_m2929.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m2929.tag === 1)) {
    const h = _m2929.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m3075) => {
  if ((_m3075.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m3075.tag === 1)) {
    const t = _m3075.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m3244) => {
  if ((_m3244.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3244.tag === 1)) {
    const h = _m3244.Head;
    const t = _m3244.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m3411) => {
  if ((_m3411.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3411.tag === 1)) {
    const h = _m3411.Head;
    const t = _m3411.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m3673) => {
  if ((_m3673.tag === 0)) {
    return list2;
  }
  if ((_m3673.tag === 1)) {
    const h = _m3673.Head;
    const t = _m3673.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m3810) => {
  if ((_m3810.tag === 0)) {
    return new List_Empty();
  }
  if ((_m3810.tag === 1)) {
    const h = _m3810.Head;
    const t = _m3810.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
