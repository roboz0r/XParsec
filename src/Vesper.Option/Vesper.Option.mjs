// Generated from option.fs
export class Option {
  constructor(tag) {
    this.tag = tag;
  }
  get $type() {
    return "Vesper.Option`1";
  }
  cases() {
    return ["None", "Some"];
  }
}
export class Option_None extends Option {
  constructor() {
    super(0);
  }
}
export class Option_Some extends Option {
  constructor(Value) {
    super(1);
    this.Value = Value;
  }
}
export const Option__get_Value = (_s1264) => ((_m1350) => {
  if ((_m1350.tag === 1)) {
    const v = _m1350.Value;
    return v;
  }
  if ((_m1350.tag === 0)) {
    return ((_s0) => ((() => { throw (_s0); })()))(new Error("Option.Value: the option value was None"));
  }
  throw new Error("The match cases were incomplete");
})(_s1264);
export const Option__get_IsSome = (_s1264) => ((_m1521) => {
  if ((_m1521.tag === 1)) {
    return true;
  }
  if ((_m1521.tag === 0)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s1264);
export const Option__get_IsNone = (_s1264) => ((_m1625) => {
  if ((_m1625.tag === 0)) {
    return true;
  }
  if ((_m1625.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s1264);
export const isSome = (option) => ((_m2045) => {
  if ((_m2045.tag === 0)) {
    return false;
  }
  if ((_m2045.tag === 1)) {
    return true;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const isNone = (option) => ((_m2163) => {
  if ((_m2163.tag === 0)) {
    return true;
  }
  if ((_m2163.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultValue = (value, option) => ((_m2299) => {
  if ((_m2299.tag === 0)) {
    return value;
  }
  if ((_m2299.tag === 1)) {
    const v = _m2299.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultWith = (defThunk, option) => ((_m2442) => {
  if ((_m2442.tag === 0)) {
    return defThunk(undefined);
  }
  if ((_m2442.tag === 1)) {
    const v = _m2442.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElse = (ifNone, option) => ((_m2583) => {
  if ((_m2583.tag === 0)) {
    return ifNone;
  }
  if ((_m2583.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElseWith = (ifNoneThunk, option) => ((_m2741) => {
  if ((_m2741.tag === 0)) {
    return ifNoneThunk(undefined);
  }
  if ((_m2741.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const get = (option) => ((_m2867) => {
  if ((_m2867.tag === 0)) {
    return ((_s1) => ((() => { throw (_s1); })()))(new Error("Option.get: the option value was None"));
  }
  if ((_m2867.tag === 1)) {
    const v = _m2867.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const count = (option) => ((_m3049) => {
  if ((_m3049.tag === 0)) {
    return 0;
  }
  if ((_m3049.tag === 1)) {
    return 1;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const fold = (folder, state, option) => ((_m3216) => {
  if ((_m3216.tag === 0)) {
    return state;
  }
  if ((_m3216.tag === 1)) {
    const x = _m3216.Value;
    return folder(state)(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const exists = (predicate, option) => ((_m3368) => {
  if ((_m3368.tag === 0)) {
    return false;
  }
  if ((_m3368.tag === 1)) {
    const x = _m3368.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const forall = (predicate, option) => ((_m3517) => {
  if ((_m3517.tag === 0)) {
    return true;
  }
  if ((_m3517.tag === 1)) {
    const x = _m3517.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const iter = (action, option) => ((_m3660) => {
  if ((_m3660.tag === 0)) {
    return undefined;
  }
  if ((_m3660.tag === 1)) {
    const x = _m3660.Value;
    return action(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const map = (mapping, option) => ((_m3796) => {
  if ((_m3796.tag === 0)) {
    return new Option_None();
  }
  if ((_m3796.tag === 1)) {
    const x = _m3796.Value;
    return new Option_Some(mapping(x));
  }
  throw new Error("The match cases were incomplete");
})(option);
export const bind = (binder, option) => ((_m3948) => {
  if ((_m3948.tag === 0)) {
    return new Option_None();
  }
  if ((_m3948.tag === 1)) {
    const x = _m3948.Value;
    return binder(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const flatten = (option) => ((_m4077) => {
  if ((_m4077.tag === 0)) {
    return new Option_None();
  }
  if ((_m4077.tag === 1)) {
    const x = _m4077.Value;
    return x;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const filter = (predicate, option) => ((_m4215) => {
  if ((_m4215.tag === 0)) {
    return new Option_None();
  }
  if ((_m4215.tag === 1)) {
    const x = _m4215.Value;
    return (predicate(x) ? new Option_Some(x) : new Option_None());
  }
  throw new Error("The match cases were incomplete");
})(option);
