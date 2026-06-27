// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s3681 = this;
    return (_s3681.started ? ((_m3980) => {
      if ((_m3980.tag === 0)) {
        return false;
      }
      if ((_m3980.tag === 1)) {
        const t = _m3980.Tail;
        return ((_s3681.cursor = t), ((_m4122) => {
          if ((_m4122.tag === 0)) {
            return false;
          }
          if ((_m4122.tag === 1)) {
            return true;
          }
          throw new Error("The match cases were incomplete");
        })(_s3681.cursor));
      }
      throw new Error("The match cases were incomplete");
    })(_s3681.cursor) : ((_s3681.started = true), ((_m4293) => {
      if ((_m4293.tag === 0)) {
        return false;
      }
      if ((_m4293.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s3681.cursor)));
  }
  Current() {
    const _s3681 = this;
    return ((_m4432) => {
      if ((_m4432.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m4432.tag === 1)) {
        const h = _m4432.Head;
        return h;
      }
      throw new Error("The match cases were incomplete");
    })(_s3681.cursor);
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
    const _e3180 = new ListEnumerator(_s2412);
    while (_e3180.MoveNext()) {
      yield _e3180.Current();
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
export const fold = (folder, state, list) => ((_m4766) => {
  if ((_m4766.tag === 0)) {
    return state;
  }
  if ((_m4766.tag === 1)) {
    const h = _m4766.Head;
    const t = _m4766.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m4910) => {
  if ((_m4910.tag === 0)) {
    return true;
  }
  if ((_m4910.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m5030) => {
  if ((_m5030.tag === 0)) {
    return 0;
  }
  if ((_m5030.tag === 1)) {
    const t = _m5030.Tail;
    return ((_s2) => (((1) + (_s2)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m5147) => {
  if ((_m5147.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5147.tag === 1)) {
    const h = _m5147.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m5293) => {
  if ((_m5293.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5293.tag === 1)) {
    const t = _m5293.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m5462) => {
  if ((_m5462.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5462.tag === 1)) {
    const h = _m5462.Head;
    const t = _m5462.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m5629) => {
  if ((_m5629.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5629.tag === 1)) {
    const h = _m5629.Head;
    const t = _m5629.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m5891) => {
  if ((_m5891.tag === 0)) {
    return list2;
  }
  if ((_m5891.tag === 1)) {
    const h = _m5891.Head;
    const t = _m5891.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m6028) => {
  if ((_m6028.tag === 0)) {
    return new List_Empty();
  }
  if ((_m6028.tag === 1)) {
    const h = _m6028.Head;
    const t = _m6028.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
