// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s3554 = this;
    return ((_s3554.started ? ((_m4084) => {
      if ((_m4084.tag === 0)) {
        return undefined;
      }
      if ((_m4084.tag === 1)) {
        const t = _m4084.Tail;
        return (_s3554.cursor = t);
      }
      throw new Error("The match cases were incomplete");
    })(_s3554.cursor) : (_s3554.started = true)), ((_m4252) => {
      if ((_m4252.tag === 0)) {
        return false;
      }
      if ((_m4252.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s3554.cursor));
  }
  Current() {
    const _s3554 = this;
    return ((_m4383) => {
      if ((_m4383.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m4383.tag === 1)) {
        const h = _m4383.Head;
        return h;
      }
      throw new Error("The match cases were incomplete");
    })(_s3554.cursor);
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
    const _s2412 = this;
    const _e3107 = new ListEnumerator(_s2412);
    while (_e3107.MoveNext()) {
      yield _e3107.Current();
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
export const fold = (folder, state, list) => ((_m4923) => {
  if ((_m4923.tag === 0)) {
    return state;
  }
  if ((_m4923.tag === 1)) {
    const h = _m4923.Head;
    const t = _m4923.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m5067) => {
  if ((_m5067.tag === 0)) {
    return true;
  }
  if ((_m5067.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m5187) => {
  if ((_m5187.tag === 0)) {
    return 0;
  }
  if ((_m5187.tag === 1)) {
    const t = _m5187.Tail;
    return ((_s2) => (((1) + (_s2)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m5304) => {
  if ((_m5304.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5304.tag === 1)) {
    const h = _m5304.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m5450) => {
  if ((_m5450.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5450.tag === 1)) {
    const t = _m5450.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m5619) => {
  if ((_m5619.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5619.tag === 1)) {
    const h = _m5619.Head;
    const t = _m5619.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m5786) => {
  if ((_m5786.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5786.tag === 1)) {
    const h = _m5786.Head;
    const t = _m5786.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m6048) => {
  if ((_m6048.tag === 0)) {
    return list2;
  }
  if ((_m6048.tag === 1)) {
    const h = _m6048.Head;
    const t = _m6048.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m6185) => {
  if ((_m6185.tag === 0)) {
    return new List_Empty();
  }
  if ((_m6185.tag === 1)) {
    const h = _m6185.Head;
    const t = _m6185.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
