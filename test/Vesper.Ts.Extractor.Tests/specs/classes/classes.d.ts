// Tier-1 class fixture: an instance property + instance method (Static:false), a
// static method + static data property (Static:true), and TWO constructor overloads
// (distinct argSig: [float] vs [float,float]) to exercise the `.ctor` seam expansion
// and the duplicate-argSig guard's negative case.

export class Point {
  x: number;
  y: number;
  constructor(x: number);
  constructor(x: number, y: number);
  distanceTo(other: Point): number;
  static origin(): Point;
  static readonly unit: number;
}
