// Tier-4 item 16 (heritage) fixture. Exercises all three heritage forms whose
// disambiguation the provider must get right from a FLAT heritage list:
//   - interface-extends-interface  (Named extends Entity)        → FrozenInterfaces
//   - class-extends-class          (Dog extends AnimalBase)      → FrozenBaseType
//   - class-implements-interface   (Dog implements Named)        → FrozenInterfaces
// The base class AND the extended/implemented interfaces are all exported so the
// provider's name-resolution disambiguation (Option A) sees them in the type table.

export interface Entity {
  id: string;
}

export interface Named extends Entity {
  name: string;
}

export class AnimalBase {
  legs: number;
}

export class Dog extends AnimalBase implements Named {
  id: string;
  name: string;
  bark(): string;
}
