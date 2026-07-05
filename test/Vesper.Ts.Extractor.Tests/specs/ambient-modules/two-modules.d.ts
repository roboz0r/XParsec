// W1 isolation fixture (decision A: one manifest per quoted ambient module). A single
// `.d.ts` declaring TWO quoted ambient modules — the `@types/node` shape in miniature
// (`declare module "fs" { … } declare module "events" { … }`). The `--ambient-modules`
// entry must enumerate BOTH and emit a SEPARATE manifest per module (homed `node/a`,
// `node/b`), NOT one aggregate artifact.
//
// Module "b" imports a type from sibling module "a": even though both live in the SAME
// physical file (which the file-origin home oracle would call LOCAL), the per-module
// split makes that reference CROSS-MANIFEST, so it must home to the DECLARING module's
// specifier (`node/a`), not the package name — the (A) cross-module ref convention.

declare module "a" {
    export interface Widget {
        id: number;
        label: string;
    }

    export function makeWidget(id: number): Widget;
}

declare module "b" {
    import { Widget } from "a";

    export interface Gadget {
        widget: Widget;
        enabled: boolean;
    }

    export function wrap(w: Widget): Gadget;
}
