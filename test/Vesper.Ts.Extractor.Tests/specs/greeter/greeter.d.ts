// MVP vertical-slice fixture: one non-generic interface with a primitive-typed
// property + method, and one free function. Expected output: greeter.manifest.json.

export interface Greeter {
  greeting: string;
  greet(name: string): string;
}

export function shout(message: string): string;
