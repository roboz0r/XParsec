// Import-shape fixture: CommonJS `export = <named>` (CommonJsExport). The export
// must be followed through its alias to the real function symbol, while the
// export-table entry's escaped name (`export=`) drives the import shape. A module
// using `export =` may not carry any other export.

declare function legacy(value: string): string;

export = legacy;
