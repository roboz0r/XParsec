class Option {
  constructor(tag) {
    this.tag = tag;
  }
  cases() {
    return ["None", "Some"];
  }
}
class Option_None extends Option {
  constructor() {
    super(0);
  }
}
class Option_Some extends Option {
  constructor(Value) {
    super(1);
    this.Value = Value;
  }
}
export const isSome = (option) => ((_m1722) => {
  if ((_m1722.tag === 0)) {
    return false;
  }
  if ((_m1722.tag === 1)) {
    return true;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const isNone = (option) => ((_m1835) => {
  if ((_m1835.tag === 0)) {
    return true;
  }
  if ((_m1835.tag === 1)) {
    return false;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultValue = (value) => (option) => ((_m1966) => {
  if ((_m1966.tag === 0)) {
    return value;
  }
  if ((_m1966.tag === 1)) {
    const v = _m1966.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const defaultWith = (defThunk) => (option) => ((_m2104) => {
  if ((_m2104.tag === 0)) {
    return defThunk(undefined);
  }
  if ((_m2104.tag === 1)) {
    const v = _m2104.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElse = (ifNone) => (option) => ((_m2240) => {
  if ((_m2240.tag === 0)) {
    return ifNone;
  }
  if ((_m2240.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const orElseWith = (ifNoneThunk) => (option) => ((_m2393) => {
  if ((_m2393.tag === 0)) {
    return ifNoneThunk(undefined);
  }
  if ((_m2393.tag === 1)) {
    return option;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const get = (option) => ((_m2514) => {
  if ((_m2514.tag === 0)) {
    return ((() => { throw new Error(("Option.get: the option value was None")); })());
  }
  if ((_m2514.tag === 1)) {
    const v = _m2514.Value;
    return v;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const count = (option) => ((_m2705) => {
  if ((_m2705.tag === 0)) {
    return 0;
  }
  if ((_m2705.tag === 1)) {
    return 1;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const fold = (folder) => (state) => (option) => ((_m2867) => {
  if ((_m2867.tag === 0)) {
    return state;
  }
  if ((_m2867.tag === 1)) {
    const x = _m2867.Value;
    return folder(state)(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const exists = (predicate) => (option) => ((_m3014) => {
  if ((_m3014.tag === 0)) {
    return false;
  }
  if ((_m3014.tag === 1)) {
    const x = _m3014.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const forall = (predicate) => (option) => ((_m3158) => {
  if ((_m3158.tag === 0)) {
    return true;
  }
  if ((_m3158.tag === 1)) {
    const x = _m3158.Value;
    return predicate(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const iter = (action) => (option) => ((_m3296) => {
  if ((_m3296.tag === 0)) {
    return undefined;
  }
  if ((_m3296.tag === 1)) {
    const x = _m3296.Value;
    return action(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const map = (mapping) => (option) => ((_m3427) => {
  if ((_m3427.tag === 0)) {
    return new Option_None();
  }
  if ((_m3427.tag === 1)) {
    const x = _m3427.Value;
    return new Option_Some(mapping(x));
  }
  throw new Error("The match cases were incomplete");
})(option);
export const bind = (binder) => (option) => ((_m3574) => {
  if ((_m3574.tag === 0)) {
    return new Option_None();
  }
  if ((_m3574.tag === 1)) {
    const x = _m3574.Value;
    return binder(x);
  }
  throw new Error("The match cases were incomplete");
})(option);
export const flatten = (option) => ((_m3698) => {
  if ((_m3698.tag === 0)) {
    return new Option_None();
  }
  if ((_m3698.tag === 1)) {
    const x = _m3698.Value;
    return x;
  }
  throw new Error("The match cases were incomplete");
})(option);
export const filter = (predicate) => (option) => ((_m3831) => {
  if ((_m3831.tag === 0)) {
    return new Option_None();
  }
  if ((_m3831.tag === 1)) {
    const x = _m3831.Value;
    return (predicate(x) ? new Option_Some(x) : new Option_None());
  }
  throw new Error("The match cases were incomplete");
})(option);
