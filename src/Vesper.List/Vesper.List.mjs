// Generated from list.js.fs
export class ListEnumerator {
  constructor(cursor, started) {
    this.cursor = cursor;
    this.started = started;
  }
  MoveNext() {
    const _s3655 = this;
    return ((_s3655.started ? ((_m4213) => {
      if ((_m4213.tag === 0)) {
        return undefined;
      }
      if ((_m4213.tag === 1)) {
        const t = _m4213.Tail;
        return (_s3655.cursor = t);
      }
      throw new Error("The match cases were incomplete");
    })(_s3655.cursor) : (_s3655.started = true)), ((_m4381) => {
      if ((_m4381.tag === 0)) {
        return false;
      }
      if ((_m4381.tag === 1)) {
        return true;
      }
      throw new Error("The match cases were incomplete");
    })(_s3655.cursor));
  }
  Current() {
    const _s3655 = this;
    return ((_m4512) => {
      if ((_m4512.tag === 0)) {
        return ((() => { throw new Error(("The input list was empty.")); })());
      }
      if ((_m4512.tag === 1)) {
        const h = _m4512.Head;
        return h;
      }
      throw new Error("The match cases were incomplete");
    })(_s3655.cursor);
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
export const fold = (folder, state, list) => ((_m5052) => {
  if ((_m5052.tag === 0)) {
    return state;
  }
  if ((_m5052.tag === 1)) {
    const h = _m5052.Head;
    const t = _m5052.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m5196) => {
  if ((_m5196.tag === 0)) {
    return true;
  }
  if ((_m5196.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m5316) => {
  if ((_m5316.tag === 0)) {
    return 0;
  }
  if ((_m5316.tag === 1)) {
    const t = _m5316.Tail;
    return ((_s2) => (((1) + (_s2)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m5433) => {
  if ((_m5433.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5433.tag === 1)) {
    const h = _m5433.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m5579) => {
  if ((_m5579.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m5579.tag === 1)) {
    const t = _m5579.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m5748) => {
  if ((_m5748.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5748.tag === 1)) {
    const h = _m5748.Head;
    const t = _m5748.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m5915) => {
  if ((_m5915.tag === 0)) {
    return new List_Empty();
  }
  if ((_m5915.tag === 1)) {
    const h = _m5915.Head;
    const t = _m5915.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m6177) => {
  if ((_m6177.tag === 0)) {
    return list2;
  }
  if ((_m6177.tag === 1)) {
    const h = _m6177.Head;
    const t = _m6177.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m6314) => {
  if ((_m6314.tag === 0)) {
    return new List_Empty();
  }
  if ((_m6314.tag === 1)) {
    const h = _m6314.Head;
    const t = _m6314.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
