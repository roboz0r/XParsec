// Tier-2 item 10 fixture: TS get/set accessors map onto the EXISTING
// MemberKind.Property (interim — both a data property and an accessor lower to
// `x.foo` on JS). Exercises all three shapes that must land as `Property`:
//   - a get-only accessor (`name`),
//   - a symmetric get+set accessor (`size`, get/set both `number`), and
//   - a plain data property (`label`),
// proving accessor and data property are indistinguishable at the schema seam.
// (Asymmetric get/set is verified out-of-band — it throws, so it cannot live in a
// committed golden fixture.)

export class Widget {
  readonly label: boolean;
  get name(): string;
  get size(): number;
  set size(value: number);
}
