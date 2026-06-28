// Tier-4 item 17 (namespace) fixture. A top-level `export namespace` whose members
// span the kinds the recursion must carry — an interface, a free function, and a
// VARIABLE — plus a NESTED namespace to prove the recursion folds arbitrary depth.
// The provider registers every nested member under its DOTTED QUALIFIED name
// (`Geometry.Shape`, `Geometry.Trig.degrees`, …), the same name the front end forms
// from a `Geometry.Shape` use site.

export namespace Geometry {
  export interface Shape {
    area(): number;
  }

  export function origin(): Shape;

  export const tau: number;

  export namespace Trig {
    export interface Angle {
      radians: number;
    }

    export function degrees(radians: number): number;
  }
}
