// Sibling module of the multi-file package fixture (item 18). `index.d.ts` pulls
// this in across files via `export * from "./shapes"` AND references `Shape` in its
// own declarations — proving the extractor walks the package entry's CROSS-FILE
// re-export closure, not just one local `.d.ts`.
export interface Shape {
  kind: string;
  area(): number;
}
