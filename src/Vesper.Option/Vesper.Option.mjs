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
export const Option__get_Value = (_s0) => ((_m59) => {
  if ((_m59.tag === 1)) {
    const v = _m59.Value;
    return v;
  }
  if ((_m59.tag === 0)) {
    return ((_s1) => ((() => { throw (_s1); })()))(new Error("Option.Value: the option value was None"));
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const Option__get_IsSome = (_s0) => ((_m60) => {
  if ((_m60.tag === 1)) {
    return true;
  }
  if ((_m60.tag === 0)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const Option__get_IsNone = (_s0) => ((_m61) => {
  if ((_m61.tag === 0)) {
    return true;
  }
  if ((_m61.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(_s0);
export const isSome = (option) => ((_m62) => {
  if ((_m62.tag === 0)) {
    return false;
  }
  if ((_m62.tag === 1)) {
    return true;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const isNone = (option) => ((_m63) => {
  if ((_m63.tag === 0)) {
    return true;
  }
  if ((_m63.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultValue = (value, option) => ((_m64) => {
  if ((_m64.tag === 0)) {
    return value;
  }
  if ((_m64.tag === 1)) {
    const v = _m64.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultWith = (defThunk, option) => ((_m65) => {
  if ((_m65.tag === 0)) {
    return defThunk(undefined);
  }
  if ((_m65.tag === 1)) {
    const v = _m65.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElse = (ifNone, option) => ((_m66) => {
  if ((_m66.tag === 0)) {
    return ifNone;
  }
  if ((_m66.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElseWith = (ifNoneThunk, option) => ((_m67) => {
  if ((_m67.tag === 0)) {
    return ifNoneThunk(undefined);
  }
  if ((_m67.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const get = (option) => ((_m68) => {
  if ((_m68.tag === 0)) {
    return ((_s21) => ((() => { throw (_s21); })()))(new Error("Option.get: the option value was None"));
  }
  if ((_m68.tag === 1)) {
    const v = _m68.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const count = (option) => ((_m69) => {
  if ((_m69.tag === 0)) {
    return 0;
  }
  if ((_m69.tag === 1)) {
    return 1;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const fold = (folder, state, option) => ((_m70) => {
  if ((_m70.tag === 0)) {
    return state;
  }
  if ((_m70.tag === 1)) {
    const x = _m70.Value;
    return folder(state)(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const exists = (predicate, option) => ((_m71) => {
  if ((_m71.tag === 0)) {
    return false;
  }
  if ((_m71.tag === 1)) {
    const x = _m71.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const forall = (predicate, option) => ((_m72) => {
  if ((_m72.tag === 0)) {
    return true;
  }
  if ((_m72.tag === 1)) {
    const x = _m72.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const iter = (action, option) => ((_m73) => {
  if ((_m73.tag === 0)) {
    return undefined;
  }
  if ((_m73.tag === 1)) {
    const x = _m73.Value;
    return action(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const map = (mapping, option) => ((_m74) => {
  if ((_m74.tag === 0)) {
    return new Option_None();
  }
  if ((_m74.tag === 1)) {
    const x = _m74.Value;
    return new Option_Some(mapping(x));
  }
  throw new Error("The match cases were incomplete");
})(option);
export const bind = (binder, option) => ((_m75) => {
  if ((_m75.tag === 0)) {
    return new Option_None();
  }
  if ((_m75.tag === 1)) {
    const x = _m75.Value;
    return binder(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const flatten = (option) => ((_m76) => {
  if ((_m76.tag === 0)) {
    return new Option_None();
  }
  if ((_m76.tag === 1)) {
    const x = _m76.Value;
    return x;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const filter = (predicate, option) => ((_m77) => {
  if ((_m77.tag === 0)) {
    return new Option_None();
  }
  if ((_m77.tag === 1)) {
    const x = _m77.Value;
    return (predicate(x) ? new Option_Some(x) : new Option_None());
  }
  throw new Error("The match cases were incomplete");
})(option);
