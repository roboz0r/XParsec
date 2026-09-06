// Generated from list.fs
export class BoxedItems {
  constructor(cursor) {
    this.inner = cursor;
  }
  *[Symbol.iterator]() {
    const _s0 = this;
    const _e119 = _s0;
    while (_e119.MoveNext()) {
      yield _e119.Current();
    }
  }
  Current() {
    const _s0 = this;
    return ((_s89) => _s89)(_s0.inner.Current());
  }
  MoveNext() {
    const _s0 = this;
    return _s0.inner.MoveNext();
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
    const _s15 = this;
    return List__get_Head(_s15.cursor);
  }
  MoveNext() {
    const _s15 = this;
    return (_s15.started ? ((_m120) => {
      if ((_m120.tag === 0)) {
        return false;
      }
      if ((_m120.tag === 1)) {
        const t = _m120.Tail;
        return ((_s15.cursor = t), ((_s111) => (!(_s111)))(List__get_IsEmpty(_s15.cursor)));
      }
      throw new Error("The match cases were incomplete");
    })(_s15.cursor) : ((_s15.started = true), ((_s112) => (!(_s112)))(List__get_IsEmpty(_s15.cursor))));
  }
  [Symbol.dispose]() {
    const _s15 = this;
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
  *[Symbol.iterator]() {
    const _s2 = this;
    const _e121 = new ListEnumerator(_s2);
    while (_e121.MoveNext()) {
      yield _e121.Current();
    }
  }
  Format(sink) {
    const _s2 = this;
    return sink.Sequence(new BoxedItems(new ListEnumerator(_s2)));
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
export const List__get_Length = (_s2) => ((_m122) => {
  if ((_m122.tag === 0)) {
    return 0;
  }
  if ((_m122.tag === 1)) {
    const t = _m122.Tail;
    return ((_s91) => (((1) + (_s91)) | 0))(List__get_Length(t));
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_IsEmpty = (_s2) => ((_m123) => {
  if ((_m123.tag === 0)) {
    return true;
  }
  if ((_m123.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Head = (_s2) => ((_m124) => {
  if ((_m124.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m124.tag === 1)) {
    const h = _m124.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Tail = (_s2) => ((_m125) => {
  if ((_m125.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m125.tag === 1)) {
    const t = _m125.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Item = (_s2) => (index) => ((nth) => nth(_s2)(index))((l) => (n) => ((_m126) => {
  if ((_m126.tag === 0)) {
    return ((() => { throw new Error(("The index was outside the range of elements in the list.")); })());
  }
  if ((_m126.tag === 1)) {
    const h = _m126.Head;
    const t = _m126.Tail;
    return (((n) === (0)) ? h : nth(t)((((n) - (1)) | 0)));
  }
  throw new Error("The match cases were incomplete");
})(l));
export const List__GetReverseIndex = (_s2) => (_rank) => (offset) => ((_s107) => (((_s107) - (1)) | 0))(((_s103) => (((_s103) - (offset)) | 0))(List__get_Length(_s2)));
export const fold = (folder, state, list) => ((_m127) => {
  if ((_m127.tag === 0)) {
    return state;
  }
  if ((_m127.tag === 1)) {
    const h = _m127.Head;
    const t = _m127.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m128) => {
  if ((_m128.tag === 0)) {
    return true;
  }
  if ((_m128.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m129) => {
  if ((_m129.tag === 0)) {
    return 0;
  }
  if ((_m129.tag === 1)) {
    const t = _m129.Tail;
    return ((_s114) => (((1) + (_s114)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m130) => {
  if ((_m130.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m130.tag === 1)) {
    const h = _m130.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m131) => {
  if ((_m131.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m131.tag === 1)) {
    const t = _m131.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m132) => {
  if ((_m132.tag === 0)) {
    return new List_Empty();
  }
  if ((_m132.tag === 1)) {
    const h = _m132.Head;
    const t = _m132.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m133) => {
  if ((_m133.tag === 0)) {
    return new List_Empty();
  }
  if ((_m133.tag === 1)) {
    const h = _m133.Head;
    const t = _m133.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m134) => {
  if ((_m134.tag === 0)) {
    return list2;
  }
  if ((_m134.tag === 1)) {
    const h = _m134.Head;
    const t = _m134.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m135) => {
  if ((_m135.tag === 0)) {
    return new List_Empty();
  }
  if ((_m135.tag === 1)) {
    const h = _m135.Head;
    const t = _m135.Tail;
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
