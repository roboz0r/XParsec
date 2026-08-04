// Generated from array.fs
export const zeroCreate = (count) => Array(count).fill(null);
export const length = (array) => array.length;
export const isEmpty = (array) => ((_s187) => ((_s187) === (0)))(array.length);
export const get = (array, index) => array[index];
export const set = (array, index, value) => (array[index] = value);
export const create = (count, value) => ((result) => ((() => {
  const _lim320 = (((count) - (1)) | 0);
  for (let i = 0; i <= _lim320; i++) {
    (result[i] = value);
  }
})(), result))(zeroCreate(count));
export const init = (count, initializer) => ((result) => ((() => {
  const _lim321 = (((count) - (1)) | 0);
  for (let i = 0; i <= _lim321; i++) {
    const _s207 = initializer(i);
    (result[i] = _s207);
  }
})(), result))(zeroCreate(count));
export const copy = (array) => ((len) => ((result) => ((() => {
  const _lim322 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim322; i++) {
    const _s217 = array[i];
    (result[i] = _s217);
  }
})(), result))(zeroCreate(len)))(array.length);
export const append = (array1, array2) => ((len1) => ((len2) => ((result) => ((() => {
  const _lim323 = (((len1) - (1)) | 0);
  for (let i = 0; i <= _lim323; i++) {
    const _s232 = array1[i];
    (result[i] = _s232);
  }
})(), (() => {
  const _lim324 = (((len2) - (1)) | 0);
  for (let i = 0; i <= _lim324; i++) {
    const _s245 = array2[i];
    (result[(((len1) + (i)) | 0)] = _s245);
  }
})(), result))(zeroCreate((((len1) + (len2)) | 0))))(array2.length))(array1.length);
export const rev = (array) => ((len) => ((result) => ((() => {
  const _lim325 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim325; i++) {
    const _s263 = array[((((((len) - (1)) | 0)) - (i)) | 0)];
    (result[i] = _s263);
  }
})(), result))(zeroCreate(len)))(array.length);
export const map = (mapping, array) => ((len) => ((result) => ((() => {
  const _lim326 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim326; i++) {
    const _s273 = mapping(array[i]);
    (result[i] = _s273);
  }
})(), result))(zeroCreate(len)))(array.length);
export const mapi = (mapping, array) => ((len) => ((result) => ((() => {
  const _lim327 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim327; i++) {
    const _s283 = mapping(i)(array[i]);
    (result[i] = _s283);
  }
})(), result))(zeroCreate(len)))(array.length);
export const iter = (action, array) => (() => {
  const _lim328 = ((_s285) => (((_s285) - (1)) | 0))(array.length);
  for (let i = 0; i <= _lim328; i++) {
    action(array[i]);
  }
})();
export const iteri = (action, array) => (() => {
  const _lim329 = ((_s292) => (((_s292) - (1)) | 0))(array.length);
  for (let i = 0; i <= _lim329; i++) {
    action(i)(array[i]);
  }
})();
export const fold = (folder, state, array) => ((acc) => ((() => {
  const _lim330 = ((_s299) => (((_s299) - (1)) | 0))(array.length);
  for (let i = 0; i <= _lim330; i++) {
    (acc = folder(acc)(array[i]));
  }
})(), acc))(state);
export const foldBack = (folder, array, state) => ((acc) => ((len) => ((() => {
  const _lim331 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim331; i++) {
    (acc = folder(array[((((((len) - (1)) | 0)) - (i)) | 0)])(acc));
  }
})(), acc))(array.length))(state);
