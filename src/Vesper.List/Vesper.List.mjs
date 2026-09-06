// Generated from list.fs
export class BoxedItems {
  constructor(cursor) {
    this.inner = cursor;
  }
  *[Symbol.iterator]() {
    const _s0 = this;
    const _e159 = _s0;
    while (_e159.MoveNext()) {
      yield _e159.Current();
    }
  }
  Current() {
    const _s0 = this;
    return ((_s111) => _s111)(_s0.inner.Current());
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
    const _s31 = this;
    return List__get_Head(_s31.cursor);
  }
  MoveNext() {
    const _s31 = this;
    return (_s31.started ? ((_m160) => {
      if ((_m160.tag === 0)) {
        return false;
      }
      if ((_m160.tag === 1)) {
        const t = _m160.Tail;
        return ((_s31.cursor = t), ((_s151) => (!(_s151)))(List__get_IsEmpty(_s31.cursor)));
      }
      throw new Error("The match cases were incomplete");
    })(_s31.cursor) : ((_s31.started = true), ((_s152) => (!(_s152)))(List__get_IsEmpty(_s31.cursor))));
  }
  [Symbol.dispose]() {
    const _s31 = this;
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
    const _e161 = new ListEnumerator(_s2);
    while (_e161.MoveNext()) {
      yield _e161.Current();
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
export const List__get_Length = (_s2) => ((_m162) => {
  if ((_m162.tag === 0)) {
    return 0;
  }
  if ((_m162.tag === 1)) {
    const t = _m162.Tail;
    return ((_s113) => (((1) + (_s113)) | 0))(List__get_Length(t));
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_IsEmpty = (_s2) => ((_m163) => {
  if ((_m163.tag === 0)) {
    return true;
  }
  if ((_m163.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Head = (_s2) => ((_m164) => {
  if ((_m164.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m164.tag === 1)) {
    const h = _m164.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Tail = (_s2) => ((_m165) => {
  if ((_m165.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m165.tag === 1)) {
    const t = _m165.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(_s2);
export const List__get_Item = (_s2) => (index) => (() => {
  const nth = (l) => (n) => {
    while (true) {
      const _m166 = l;
      if ((_m166.tag === 0)) {
        return ((() => { throw new Error(("The index was outside the range of elements in the list.")); })());
      }
      if ((_m166.tag === 1)) {
        const h = _m166.Head;
        const t = _m166.Tail;
        if (((n) === (0))) {
          return h;
        } else {
          const _tc0 = t;
          const _tc1 = (((n) - (1)) | 0);
          l = _tc0;
          n = _tc1;
          continue;
        }
      }
      throw new Error("The match cases were incomplete");
    }
  };
  return nth(_s2)(index);
})();
export const List__GetSlice = (_s2) => (startIndex) => (endIndex) => (() => {
  const sliceSkip = (n) => (l) => {
    while (true) {
      if (((n) <= (0))) {
        return l;
      } else {
        const _m167 = l;
        if ((_m167.tag === 0)) {
          return new List_Empty();
        }
        if ((_m167.tag === 1)) {
          const t = _m167.Tail;
          const _tc0 = (((n) - (1)) | 0);
          const _tc1 = t;
          n = _tc0;
          l = _tc1;
          continue;
        }
        throw new Error("The match cases were incomplete");
      }
    }
  };
  return (() => {
    const sliceTake = (n) => (l) => (((n) < (0)) ? new List_Empty() : ((_m168) => {
      if ((_m168.tag === 0)) {
        return new List_Empty();
      }
      if ((_m168.tag === 1)) {
        const h = _m168.Head;
        const t = _m168.Tail;
        return new List_Cons(h, sliceTake((((n) - (1)) | 0))(t));
      }
      throw new Error("The match cases were incomplete");
    })(l));
    return ((_m169) => {
      if (((_m169[0].tag === 0) && (_m169[1].tag === 0))) {
        return _s2;
      }
      if (((_m169[0].tag === 1) && (_m169[1].tag === 0))) {
        const i = _m169[0].Value;
        return sliceSkip(i)(_s2);
      }
      if (((_m169[0].tag === 0) && (_m169[1].tag === 1))) {
        const j = _m169[1].Value;
        return sliceTake(j)(_s2);
      }
      if (((_m169[0].tag === 1) && (_m169[1].tag === 1))) {
        const i = _m169[0].Value;
        const j = _m169[1].Value;
        return ((start) => sliceTake((((j) - (start)) | 0))(sliceSkip(start)(_s2)))((((i) < (0)) ? 0 : i));
      }
      throw new Error("The match cases were incomplete");
    })([startIndex, endIndex]);
  })();
})();
export const List__GetReverseIndex = (_s2) => (_rank) => (offset) => ((_s147) => (((_s147) - (1)) | 0))(((_s143) => (((_s143) - (offset)) | 0))(List__get_Length(_s2)));
export const fold = (folder, state, list) => {
  while (true) {
    const _m170 = list;
    if ((_m170.tag === 0)) {
      return state;
    }
    if ((_m170.tag === 1)) {
      const h = _m170.Head;
      const t = _m170.Tail;
      const _tc0 = folder;
      const _tc1 = folder(state)(h);
      const _tc2 = t;
      folder = _tc0;
      state = _tc1;
      list = _tc2;
      continue;
    }
    throw new Error("The match cases were incomplete");
  }
};
export const isEmpty = (list) => ((_m171) => {
  if ((_m171.tag === 0)) {
    return true;
  }
  if ((_m171.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const length = (list) => ((_m172) => {
  if ((_m172.tag === 0)) {
    return 0;
  }
  if ((_m172.tag === 1)) {
    const t = _m172.Tail;
    return ((_s154) => (((1) + (_s154)) | 0))(length(t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const head = (list) => ((_m173) => {
  if ((_m173.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m173.tag === 1)) {
    const h = _m173.Head;
    return h;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const tail = (list) => ((_m174) => {
  if ((_m174.tag === 0)) {
    return ((() => { throw new Error(("The input list was empty.")); })());
  }
  if ((_m174.tag === 1)) {
    const t = _m174.Tail;
    return t;
  }
  throw new Error("The match cases were incomplete");
})(list);
export const map = (mapping, list) => ((_m175) => {
  if ((_m175.tag === 0)) {
    return new List_Empty();
  }
  if ((_m175.tag === 1)) {
    const h = _m175.Head;
    const t = _m175.Tail;
    return new List_Cons(mapping(h), map(mapping, t));
  }
  throw new Error("The match cases were incomplete");
})(list);
export const filter = (predicate, list) => {
  while (true) {
    const _m176 = list;
    if ((_m176.tag === 0)) {
      return new List_Empty();
    }
    if ((_m176.tag === 1)) {
      const h = _m176.Head;
      const t = _m176.Tail;
      if (predicate(h)) {
        return new List_Cons(h, filter(predicate, t));
      } else {
        const _tc0 = predicate;
        const _tc1 = t;
        predicate = _tc0;
        list = _tc1;
        continue;
      }
    }
    throw new Error("The match cases were incomplete");
  }
};
export const append = (list1, list2) => ((_m177) => {
  if ((_m177.tag === 0)) {
    return list2;
  }
  if ((_m177.tag === 1)) {
    const h = _m177.Head;
    const t = _m177.Tail;
    return new List_Cons(h, append(t, list2));
  }
  throw new Error("The match cases were incomplete");
})(list1);
export const rev = (list) => ((_m178) => {
  if ((_m178.tag === 0)) {
    return new List_Empty();
  }
  if ((_m178.tag === 1)) {
    const h = _m178.Head;
    const t = _m178.Tail;
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
