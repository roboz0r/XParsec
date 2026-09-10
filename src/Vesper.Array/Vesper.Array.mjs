// Generated from array.fs
export const zeroCreate = (count) => Array(count).fill(null);
export const length = (array) => array.length;
export const isEmpty = (array) => ((array.length) === (0));
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
    (result[i] = initializer(i));
  }
})(), result))(zeroCreate(count));
export const copy = (array) => ((len) => ((result) => ((() => {
  const _lim322 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim322; i++) {
    (result[i] = array[i]);
  }
})(), result))(zeroCreate(len)))(array.length);
export const append = (array1, array2) => ((len1) => ((len2) => ((result) => ((() => {
  const _lim323 = (((len1) - (1)) | 0);
  for (let i = 0; i <= _lim323; i++) {
    (result[i] = array1[i]);
  }
})(), (() => {
  const _lim324 = (((len2) - (1)) | 0);
  for (let i = 0; i <= _lim324; i++) {
    (result[(((len1) + (i)) | 0)] = array2[i]);
  }
})(), result))(zeroCreate((((len1) + (len2)) | 0))))(array2.length))(array1.length);
export const rev = (array) => ((len) => ((result) => ((() => {
  const _lim325 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim325; i++) {
    (result[i] = array[((((((len) - (1)) | 0)) - (i)) | 0)]);
  }
})(), result))(zeroCreate(len)))(array.length);
export const map = (mapping, array) => ((len) => ((result) => ((() => {
  const _lim326 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim326; i++) {
    (result[i] = mapping(array[i]));
  }
})(), result))(zeroCreate(len)))(array.length);
export const mapi = (mapping, array) => ((len) => ((result) => ((() => {
  const _lim327 = (((len) - (1)) | 0);
  for (let i = 0; i <= _lim327; i++) {
    (result[i] = mapping(i)(array[i]));
  }
})(), result))(zeroCreate(len)))(array.length);
export const iter = (action, array) => (() => {
  const _lim328 = (((array.length) - (1)) | 0);
  for (let i = 0; i <= _lim328; i++) {
    action(array[i]);
  }
})();
export const iteri = (action, array) => (() => {
  const _lim329 = (((array.length) - (1)) | 0);
  for (let i = 0; i <= _lim329; i++) {
    action(i)(array[i]);
  }
})();
export const fold = (folder, state, array) => ((acc) => ((() => {
  const _lim330 = (((array.length) - (1)) | 0);
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
