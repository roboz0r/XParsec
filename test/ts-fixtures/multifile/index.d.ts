// Package ENTRY of the multi-file fixture (item 18). Re-exports a sibling module's
// `Shape` (cross-file closure) and adds its own declarations that reference it, so
// the emitted manifest must contain symbols pulled from BOTH files.
import { Shape } from "./shapes";

export * from "./shapes";

export interface Drawing {
  name: string;
  shapes: Shape[];
}

export function totalArea(shapes: Shape[]): number;
