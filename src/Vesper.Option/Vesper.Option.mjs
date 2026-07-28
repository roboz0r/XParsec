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
export const Option__get_Value = (_s0) => ((_m220) => {
  if ((_m220.tag === 1)) {
    const v = _m220.Value;
    return v;
  }
  if ((_m220.tag === 0)) {
    return ((_s1) => ((() => { throw (_s1); })()))(new Error("Option.Value: the option value was None"));
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const Option__get_IsSome = (_s0) => ((_m391) => {
  if ((_m391.tag === 1)) {
    return true;
  }
  if ((_m391.tag === 0)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const Option__get_IsNone = (_s0) => ((_m495) => {
  if ((_m495.tag === 0)) {
    return true;
  }
  if ((_m495.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const isSome = (option) => ((_m764) => {
  if ((_m764.tag === 0)) {
    return false;
  }
  if ((_m764.tag === 1)) {
    return true;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const isNone = (option) => ((_m882) => {
  if ((_m882.tag === 0)) {
    return true;
  }
  if ((_m882.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultValue = (value, option) => ((_m1018) => {
  if ((_m1018.tag === 0)) {
    return value;
  }
  if ((_m1018.tag === 1)) {
    const v = _m1018.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultWith = (defThunk, option) => ((_m1161) => {
  if ((_m1161.tag === 0)) {
    return defThunk(undefined);
  }
  if ((_m1161.tag === 1)) {
    const v = _m1161.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElse = (ifNone, option) => ((_m1302) => {
  if ((_m1302.tag === 0)) {
    return ifNone;
  }
  if ((_m1302.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElseWith = (ifNoneThunk, option) => ((_m1460) => {
  if ((_m1460.tag === 0)) {
    return ifNoneThunk(undefined);
  }
  if ((_m1460.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const get = (option) => ((_m1586) => {
  if ((_m1586.tag === 0)) {
    return ((_s21) => ((() => { throw (_s21); })()))(new Error("Option.get: the option value was None"));
  }
  if ((_m1586.tag === 1)) {
    const v = _m1586.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const count = (option) => ((_m1768) => {
  if ((_m1768.tag === 0)) {
    return 0;
  }
  if ((_m1768.tag === 1)) {
    return 1;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const fold = (folder, state, option) => ((_m1935) => {
  if ((_m1935.tag === 0)) {
    return state;
  }
  if ((_m1935.tag === 1)) {
    const x = _m1935.Value;
    return folder(state)(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const exists = (predicate, option) => ((_m2087) => {
  if ((_m2087.tag === 0)) {
    return false;
  }
  if ((_m2087.tag === 1)) {
    const x = _m2087.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const forall = (predicate, option) => ((_m2236) => {
  if ((_m2236.tag === 0)) {
    return true;
  }
  if ((_m2236.tag === 1)) {
    const x = _m2236.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const iter = (action, option) => ((_m2379) => {
  if ((_m2379.tag === 0)) {
    return undefined;
  }
  if ((_m2379.tag === 1)) {
    const x = _m2379.Value;
    return action(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const map = (mapping, option) => ((_m2515) => {
  if ((_m2515.tag === 0)) {
    return new Option_None();
  }
  if ((_m2515.tag === 1)) {
    const x = _m2515.Value;
    return new Option_Some(mapping(x));
  }
  throw new Error("The match cases were incomplete");
})(option);
export const bind = (binder, option) => ((_m2667) => {
  if ((_m2667.tag === 0)) {
    return new Option_None();
  }
  if ((_m2667.tag === 1)) {
    const x = _m2667.Value;
    return binder(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const flatten = (option) => ((_m2796) => {
  if ((_m2796.tag === 0)) {
    return new Option_None();
  }
  if ((_m2796.tag === 1)) {
    const x = _m2796.Value;
    return x;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const filter = (predicate, option) => ((_m2934) => {
  if ((_m2934.tag === 0)) {
    return new Option_None();
  }
  if ((_m2934.tag === 1)) {
    const x = _m2934.Value;
    return (predicate(x) ? new Option_Some(x) : new Option_None());
  }
  throw new Error("The match cases were incomplete");
})(option);
