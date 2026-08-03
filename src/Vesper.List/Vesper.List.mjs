// Generated from list.fs
export class ListEnumerator {
  constructor(s) {
    this.cursor = s;
    this.started = false;
  }
  Current() {
    const _s4 = this;
    return List__get_Head(_s4.cursor);
  }
  MoveNext() {
    const _s4 = this;
    return (_s4.started ? ((_m79) => {
      if ((_m79.tag === 0)) {
        return false;
      }
      if ((_m79.tag === 1)) {
        const t = _m79.Tail;
        return ((_s4.cursor = t), ((_s71) => (!(_s71)))(List__get_IsEmpty(_s4.cursor)));
      }
      throw new Error("The match cases were incomplete");
    })(_s4.cursor) : ((_s4.started = true), ((_s72) => (!(_s72)))(List__get_IsEmpty(_s4.cursor))));
  }
  [Symbol.dispose]() {
    const _s4 = this;
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
    const _s0 = this;
    const _e80 = new ListEnumerator(_s0);
    while (_e80.MoveNext()) {
      yield _e80.Current();
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
export const List__get_Length = (_s0) => ((_m81) => {
  if ((_m81.tag === 0)) {
    return 0;
  }
  if ((_m81.tag === 1)) {
    const t = _m81.Tail;
    return ((_s66) => (((1) + (_s66)) | 0))(List__get_Length(t));
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const List__get_IsEmpty = (_s0) => ((_m82) => {
  if ((_m82.tag === 0)) {
    return true;
  }
  if ((_m82.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const List__get_Head = (_s0) => ((_m83) => {
  if ((_m83.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m83.tag === 1)) {
    const h = _m83.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const List__get_Tail = (_s0) => ((_m84) => {
  if ((_m84.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m84.tag === 1)) {
    const t = _m84.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const fold = (folder, state, list) => ((_m85) => {
  if ((_m85.tag === 0)) {
    return state;
  }
  if ((_m85.tag === 1)) {
    const h = _m85.Head;
    const t = _m85.Tail;
    return fold(folder, folder(state)(h), t);
  }
  throw new Error("The match cases were incomplete");
})(list);
export const isEmpty = (list) => ((_m86) => {
  if ((_m86.tag === 0)) {
    return true;
  }
  if ((_m86.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m87) => {
  if ((_m87.tag === 0)) {
    return 0;
  }
  if ((_m87.tag === 1)) {
    const t = _m87.Tail;
    return ((_s74) => (((1) + (_s74)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m88) => {
  if ((_m88.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m88.tag === 1)) {
    const h = _m88.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m89) => {
  if ((_m89.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m89.tag === 1)) {
    const t = _m89.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m90) => {
  if ((_m90.tag === 0)) {
    return new List_Empty();
  }
  if ((_m90.tag === 1)) {
    const h = _m90.Head;
    const t = _m90.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => ((_m91) => {
  if ((_m91.tag === 0)) {
    return new List_Empty();
  }
  if ((_m91.tag === 1)) {
    const h = _m91.Head;
    const t = _m91.Tail;
    return (predicate(h) ? new List_Cons(h, filter(predicate, t)) : filter(predicate, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const append = (list1, list2) => ((_m92) => {
  if ((_m92.tag === 0)) {
    return list2;
  }
  if ((_m92.tag === 1)) {
    const h = _m92.Head;
    const t = _m92.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m93) => {
  if ((_m93.tag === 0)) {
    return new List_Empty();
  }
  if ((_m93.tag === 1)) {
    const h = _m93.Head;
    const t = _m93.Tail;
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
