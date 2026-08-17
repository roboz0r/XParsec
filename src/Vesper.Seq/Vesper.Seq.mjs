// Generated from seq.fs
import { zeroCreate as $Vesper_Collections_ArrayModule_zeroCreate } from "../Vesper.Array/index.mjs";
import { enumeratorOf as $Vesper_Collections_enumeratorOf } from "../Vesper.Core/Vesper.Core.mjs";
export class TruncateSeq {
  constructor(source, limit) {
    this.source = source;
    this.limit = limit;
  }
  *[Symbol.iterator]() {
    const _s0 = this;
    const _e151 = new TruncateEnumerator($Vesper_Collections_enumeratorOf(_s0.source), _s0.limit);
    while (_e151.MoveNext()) {
      yield _e151.Current();
    }
  }
}
export class TruncateEnumerator {
  constructor(inner, limit) {
    this.inner = inner;
    this.limit = limit;
    this.taken = 0;
  }
  Current() {
    const _s3 = this;
    return _s3.inner.Current();
  }
  MoveNext() {
    const _s3 = this;
    return (((_s96) => ((_s97) => ((_s96) >= (_s97)))(_s3.limit))(_s3.taken) ? false : (_s3.inner.MoveNext() ? ((_s3.taken = ((_s98) => (((_s98) + (1)) | 0))(_s3.taken)), true) : false));
  }
  [Symbol.dispose]() {
    const _s3 = this;
    return _s3.inner[Symbol.dispose]();
  }
}
export const fold = (folder, state, source) => ((acc) => ((() => {
  for (const x of source) {
    (acc = folder(acc)(x));
  }
})(), acc))(state);
export const reduce = (reduction, source) => ((acc) => ((seen) => ((() => {
  for (const x of source) {
    (acc = (seen ? reduction(acc)(x) : x));
    (seen = true);
  }
})(), ((!(seen)) ? ((() => { throw new Error(((((((("The input sequence was empty.") + (" (Parameter '"))) + ("source"))) + ("')")))); })()) : undefined), acc))(false))((null));
export const truncate = (count, source) => new TruncateSeq(source, count);
export const toArray = (source) => ((buffer) => ((count) => ((() => {
  for (const x of source) {
    (((_s120) => ((count) === (_s120)))(buffer.length) ? ((grown) => ((() => {
      const _lim152 = (((count) - (1)) | 0);
      for (let i = 0; i <= _lim152; i++) {
        const _s134 = buffer[i];
        (grown[i] = _s134);
      }
    })(), (buffer = grown)))($Vesper_Collections_ArrayModule_zeroCreate(((_s122) => (Math.imul((_s122), (2))))(buffer.length))) : undefined);
    (buffer[count] = x);
    (count = (((count) + (1)) | 0));
  }
})(), ((result) => ((() => {
  const _lim153 = (((count) - (1)) | 0);
  for (let i = 0; i <= _lim153; i++) {
    const _s150 = buffer[i];
    (result[i] = _s150);
  }
})(), result))($Vesper_Collections_ArrayModule_zeroCreate(count))))(0))($Vesper_Collections_ArrayModule_zeroCreate(4));
