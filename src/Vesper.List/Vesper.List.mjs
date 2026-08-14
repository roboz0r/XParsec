// Generated from list.fs
export class BoxedItems {
  constructor(cursor) {
    this.inner = cursor;
  }
  Current() {
    const _s0 = this;
    return ((_s70) => _s70)(_s0.inner.Current());
  }
  MoveNext() {
    const _s0 = this;
    return _s0.inner.MoveNext();
  }
  *[Symbol.iterator]() {
    const _s0 = this;
    const _e85 = _s0;
    while (_e85.MoveNext()) {
      yield _e85.Current();
    }
  }
  [Symbol.dispose]() {
    const _s0 = this;
    return _s0.inner[Symbol.dispose]();
  }
}
export class ListEnumerator {
  constructor(s) {
    this.cursor = s;
    this.started = false;
  }
  Current() {
    const _s7 = this;
    return List__get_Head(_s7.cursor);
  }
  MoveNext() {
    const _s7 = this;
    return (_s7.started ? ((_m86) => {
      if ((_m86.tag === 0)) {
        return false;
      }
      if ((_m86.tag === 1)) {
        const t = _m86.Tail;
        return ((_s7.cursor = t), ((_s77) => (!(_s77)))(List__get_IsEmpty(_s7.cursor)));
      }
      throw new Error("The match cases were incomplete");
    })(_s7.cursor) : ((_s7.started = true), ((_s78) => (!(_s78)))(List__get_IsEmpty(_s7.cursor))));
  }
  [Symbol.dispose]() {
    const _s7 = this;
    return undefined;
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
  Format(sink) {
    const _s2 = this;
    return sink.Sequence(new BoxedItems(new ListEnumerator(_s2)));
  }
  *[Symbol.iterator]() {
    const _s2 = this;
    const _e87 = new ListEnumerator(_s2);
    while (_e87.MoveNext()) {
      yield _e87.Current();
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
export const List__get_Length = (_s2) => ((_m88) => {
  if ((_m88.tag === 0)) {
    return 0;
  }
  if ((_m88.tag === 1)) {
    const t = _m88.Tail;
    return ((_s72) => (((1) + (_s72)) | 0))(List__get_Length(t));
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_IsEmpty = (_s2) => ((_m89) => {
  if ((_m89.tag === 0)) {
    return true;
  }
  if ((_m89.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Head = (_s2) => ((_m90) => {
  if ((_m90.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m90.tag === 1)) {
    const h = _m90.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Tail = (_s2) => ((_m91) => {
  if ((_m91.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m91.tag === 1)) {
    const t = _m91.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const fold = (folder, state, list) => ((_m92) => {
  if ((_m92.tag === 0)) {
    return state;
  }
  if ((_m92.tag === 1)) {
    const h = _m92.Head;
    const t = _m92.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m93) => {
  if ((_m93.tag === 0)) {
    return true;
  }
  if ((_m93.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m94) => {
  if ((_m94.tag === 0)) {
    return 0;
  }
  if ((_m94.tag === 1)) {
    const t = _m94.Tail;
    return ((_s80) => (((1) + (_s80)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m95) => {
  if ((_m95.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m95.tag === 1)) {
    const h = _m95.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m96) => {
  if ((_m96.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m96.tag === 1)) {
    const t = _m96.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m97) => {
  if ((_m97.tag === 0)) {
    return new List_Empty();
  }
  if ((_m97.tag === 1)) {
    const h = _m97.Head;
    const t = _m97.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m98) => {
  if ((_m98.tag === 0)) {
    return new List_Empty();
  }
  if ((_m98.tag === 1)) {
    const h = _m98.Head;
    const t = _m98.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m99) => {
  if ((_m99.tag === 0)) {
    return list2;
  }
  if ((_m99.tag === 1)) {
    const h = _m99.Head;
    const t = _m99.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m100) => {
  if ((_m100.tag === 0)) {
    return new List_Empty();
  }
  if ((_m100.tag === 1)) {
    const h = _m100.Head;
    const t = _m100.Tail;
    return append(rev(t), new List_Cons(h, new List_Empty()));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const toSeq = (list) => list;
export const ofSeq = (source) => ((acc) => ((() => {
  for (const x of source) {
    (acc = new List_Cons(x, acc));
  }
})(), rev(acc)))(new List_Empty());
