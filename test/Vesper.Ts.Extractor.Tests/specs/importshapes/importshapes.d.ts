// Import-shape fixture: `export default` (Default) coexisting with a plain named
// export (Named). The extractor must stamp each export's import field from the
// export-table entry, not a hardcoded `Named`. (`export =` cannot share a module
// with other exports, so it lives in the sibling commonjs.d.ts.)

export default function greet(name: string): string;

export function shout(message: string): string;
