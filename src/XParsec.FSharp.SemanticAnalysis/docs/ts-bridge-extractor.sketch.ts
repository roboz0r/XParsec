/**
 * ts-bridge-extractor.sketch.ts
 *
 * Design-discussion sketch (not wired into any build) for the Codegen.Js
 * external symbol provider — see codegen-js-symbol-provider-plan.md.
 *
 * Skeleton for extracting a package's public type surface from the TypeScript
 * compiler API into a serializable IR — the JS/TS analog of reading .NET
 * assemblies through a MetadataLoadContext.
 *
 * Conceptual mapping (.NET -> here):
 *   MetadataLoadContext                 -> ts.Program + ts.TypeChecker
 *   MetadataAssemblyResolver            -> ts.CompilerHost + module resolution
 *   Assembly.Load (reflection-only)     -> adding the package to the module graph
 *   assembly.GetExportedTypes()         -> checker.getExportsOfModule(moduleSymbol)
 *   Type / MethodInfo / PropertyInfo    -> ts.Symbol (+ ts.Type for structure)
 *   "no code runs"                      -> checker only reads .d.ts; nothing executes
 *
 * Runs on the classic `typescript` package (the "Strada" API). Keep everything
 * behind the LoadContext/IR boundary so a future stable Corsa/TS7 API can be
 * swapped in without touching your serialized format.
 *
 *   npm i -D typescript
 *   npx tsx ts-bridge-extractor.sketch.ts <projectDir> <package> [<package> ...]
 */

import * as ts from "typescript";
import * as path from "node:path";
import * as fs from "node:fs";

// ─── 1. The load context — your MetadataLoadContext ────────────────────────
// A Program is the closed, resolved universe. The CompilerHost is your
// assembly resolver: it decides how bare specifiers and their *dependency
// closure* resolve to .d.ts files (bundled types AND @types/* on the side).

interface LoadContext {
  program: ts.Program;
  checker: ts.TypeChecker;
  entryFile: string;
  options: ts.CompilerOptions;
}

function createLoadContext(packages: string[], projectDir: string): LoadContext {
  const options: ts.CompilerOptions = {
    moduleResolution: ts.ModuleResolutionKind.Bundler, // or NodeNext for strict node semantics
    module: ts.ModuleKind.ESNext,
    target: ts.ScriptTarget.ESNext,
    strict: true,
    skipLibCheck: true, // we're reading the world, not validating it
    noEmit: true,
    types: [], // don't auto-pull ambient @types you didn't ask for
    baseUrl: projectDir,
  };

  // A synthetic entry file that imports each package, so the program's module
  // graph pulls in their .d.ts closure. This is "hand the context the set of
  // assemblies to load."
  const entryFile = path.join(projectDir, "__bridge_entry__.ts");
  const entrySource = packages
    .map((p, i) => `import * as m${i} from ${JSON.stringify(p)};`)
    .join("\n");

  const host = ts.createCompilerHost(options);
  const realGetSourceFile = host.getSourceFile.bind(host);
  host.getSourceFile = (fileName, lang, onErr, create) =>
    fileName === entryFile
      ? ts.createSourceFile(fileName, entrySource, lang, true)
      : realGetSourceFile(fileName, lang, onErr, create);
  host.fileExists = (f) => f === entryFile || ts.sys.fileExists(f);
  host.readFile = (f) => (f === entryFile ? entrySource : ts.sys.readFile(f));

  const program = ts.createProgram([entryFile], options, host);
  return { program, checker: program.getTypeChecker(), entryFile, options };
}

// ─── 2. Resolve a package to its module symbol — Assembly.Load ─────────────

function getModuleSymbol(ctx: LoadContext, pkg: string): ts.Symbol | undefined {
  const resolved = ts.resolveModuleName(
    pkg,
    ctx.entryFile,
    ctx.options,
    ts.sys
  ).resolvedModule;
  if (!resolved) return undefined;

  const sf = ctx.program.getSourceFile(resolved.resolvedFileName);
  if (!sf) return undefined;

  // File module symbol, or the merged ambient `declare module` symbol.
  return (
    ctx.checker.getSymbolAtLocation(sf) ??
    ctx.checker.getMergedSymbol((sf as any).symbol)
  );
}

// ─── 3. The serializable IR ────────────────────────────────────────────────
// TypeRef is the central design decision. The pragmatic version is a string
// (checker.typeToString). The deep version is a structured discriminated union
// you resolve yourself — see the note in extractType() below.

type TypeRef = string; // pragmatic. Replace with a structured union for a real bridge.

interface ParamIR { name: string; type: TypeRef; optional: boolean; rest: boolean; }
interface SignatureIR { typeParams: TypeRef[]; params: ParamIR[]; returns: TypeRef; }
interface MemberIR {
  name: string;
  kind: "property" | "method";
  type?: TypeRef;
  signatures?: SignatureIR[];
  optional: boolean;
  static: boolean;
  readonly: boolean;
}

type ExportIR =
  | { kind: "function"; name: string; signatures: SignatureIR[] }
  | { kind: "class"; name: string; typeParams: TypeRef[]; members: MemberIR[]; heritage: TypeRef[] }
  | { kind: "interface"; name: string; typeParams: TypeRef[]; members: MemberIR[]; heritage: TypeRef[] }
  | { kind: "typeAlias"; name: string; typeParams: TypeRef[]; target: TypeRef }
  | { kind: "enum"; name: string; members: { name: string; value?: string | number }[] }
  | { kind: "variable"; name: string; type: TypeRef; const: boolean }
  | { kind: "namespace"; name: string; exports: ExportIR[] };

interface PackageIR { package: string; version?: string; exports: ExportIR[]; }

// ─── 4. The dispatcher — classify each exported symbol ─────────────────────

function extractPackage(ctx: LoadContext, pkg: string): PackageIR {
  const mod = getModuleSymbol(ctx, pkg);
  if (!mod) throw new Error(`Could not resolve types for "${pkg}"`);
  const exports = ctx.checker.getExportsOfModule(mod);
  return {
    package: pkg,
    version: readVersion(ctx, pkg),
    exports: exports
      .map((s) => extractSymbol(ctx, s))
      .filter((x): x is ExportIR => x != null),
  };
}

function extractSymbol(ctx: LoadContext, sym: ts.Symbol): ExportIR | undefined {
  const F = ts.SymbolFlags;
  const flags = sym.getFlags();
  const name = sym.getName();
  const decl = sym.declarations?.[0];
  if (!decl) return undefined;

  if (flags & F.Function) return extractFunction(ctx, sym, name, decl);
  if (flags & F.Class) return extractClassLike(ctx, sym, name, "class");
  if (flags & F.Interface) return extractClassLike(ctx, sym, name, "interface");
  if (flags & F.TypeAlias) return extractTypeAlias(ctx, sym, name);
  if (flags & F.Enum) return extractEnum(ctx, sym, name);
  if (flags & (F.Module | F.NamespaceModule)) return extractNamespace(ctx, sym, name);
  if (flags & (F.Variable | F.BlockScopedVariable)) return extractVariable(ctx, sym, name, decl);
  return undefined; // alias re-exports etc. — follow with checker.getAliasedSymbol if needed
}

// ─── 5. Extractors (showing the two core patterns) ─────────────────────────

function extractFunction(ctx: LoadContext, sym: ts.Symbol, name: string, decl: ts.Declaration): ExportIR {
  const t = ctx.checker.getTypeOfSymbolAtLocation(sym, decl);
  return { kind: "function", name, signatures: t.getCallSignatures().map((s) => extractSignature(ctx, s)) };
}

function extractSignature(ctx: LoadContext, sig: ts.Signature): SignatureIR {
  return {
    typeParams: (sig.getTypeParameters() ?? []).map((tp) => extractType(ctx, tp)),
    params: sig.getParameters().map((p) => {
      const pd = p.valueDeclaration as ts.ParameterDeclaration | undefined;
      const t = ctx.checker.getTypeOfSymbolAtLocation(p, pd ?? sig.getDeclaration()!);
      return {
        name: p.getName(),
        type: extractType(ctx, t),
        optional: pd?.questionToken != null || pd?.initializer != null,
        rest: pd?.dotDotDotToken != null,
      };
    }),
    returns: extractType(ctx, sig.getReturnType()),
  };
}

function extractClassLike(ctx: LoadContext, sym: ts.Symbol, name: string, kind: "class" | "interface"): ExportIR {
  // getDeclaredTypeOfSymbol gives the *open generic* declared type — the
  // analog of an open generic Type<> in reflection. Don't pre-instantiate.
  const declared = ctx.checker.getDeclaredTypeOfSymbol(sym);
  const members: MemberIR[] = ctx.checker
    .getPropertiesOfType(declared)
    .map((p) => extractMember(ctx, p));

  const typeParams = (declared as ts.InterfaceType).typeParameters?.map((tp) => extractType(ctx, tp)) ?? [];
  const heritage = ctx.checker.getBaseTypes(declared as ts.InterfaceType).map((b) => extractType(ctx, b));

  return kind === "class"
    ? { kind: "class", name, typeParams, members, heritage }
    : { kind: "interface", name, typeParams, members, heritage };
}

function extractMember(ctx: LoadContext, prop: ts.Symbol): MemberIR {
  const decl = prop.valueDeclaration ?? prop.declarations?.[0];
  const t = decl ? ctx.checker.getTypeOfSymbolAtLocation(prop, decl) : undefined;
  const callSigs = t?.getCallSignatures() ?? [];
  const isMethod = callSigs.length > 0 && (prop.getFlags() & ts.SymbolFlags.Method) !== 0;
  const mods = decl ? ts.getCombinedModifierFlags(decl) : 0;

  return {
    name: prop.getName(),
    kind: isMethod ? "method" : "property",
    type: isMethod ? undefined : t ? extractType(ctx, t) : undefined,
    signatures: isMethod ? callSigs.map((s) => extractSignature(ctx, s)) : undefined,
    optional: (prop.getFlags() & ts.SymbolFlags.Optional) !== 0,
    static: (mods & ts.ModifierFlags.Static) !== 0,
    readonly: (mods & ts.ModifierFlags.Readonly) !== 0,
  };
}

function extractTypeAlias(ctx: LoadContext, sym: ts.Symbol, name: string): ExportIR {
  const t = ctx.checker.getDeclaredTypeOfSymbol(sym);
  const tp = (sym.declarations?.[0] as ts.TypeAliasDeclaration)?.typeParameters ?? [];
  return {
    kind: "typeAlias",
    name,
    typeParams: tp.map((p) => p.name.text),
    target: extractType(ctx, t),
  };
}

function extractEnum(ctx: LoadContext, sym: ts.Symbol, name: string): ExportIR {
  const members = ctx.checker.getExportsOfModule(sym).map((m) => {
    const d = m.valueDeclaration;
    const value = d ? ctx.checker.getConstantValue(d as ts.EnumMember) : undefined;
    return { name: m.getName(), value };
  });
  return { kind: "enum", name, members };
}

function extractVariable(ctx: LoadContext, sym: ts.Symbol, name: string, decl: ts.Declaration): ExportIR {
  const t = ctx.checker.getTypeOfSymbolAtLocation(sym, decl);
  const isConst = (ts.getCombinedNodeFlags(decl) & ts.NodeFlags.Const) !== 0;
  return { kind: "variable", name, type: extractType(ctx, t), const: isConst };
}

function extractNamespace(ctx: LoadContext, sym: ts.Symbol, name: string): ExportIR {
  const exports = ctx.checker
    .getExportsOfModule(sym)
    .map((s) => extractSymbol(ctx, s))
    .filter((x): x is ExportIR => x != null);
  return { kind: "namespace", name, exports };
}

// ─── 6. extractType — the single most important decision in the whole thing ─
// PRAGMATIC (below): freeze to a textual reference via typeToString. Good
// enough to get a working bridge, lossy for downstream resolution.
//
// DEEP (what a real language bridge wants): recurse into the Type and emit a
// structured discriminated union, e.g.:
//   if (t.isUnion())            -> { kind:"union", members:[...] }
//   if (t.isIntersection())     -> { kind:"intersection", members:[...] }
//   if (t.flags & TypeFlags.Object && objectFlags & Reference)
//                               -> { kind:"ref", name, typeArgs:[...] }   // generic instantiation
//   if (checker.getIndexInfoOfType / getCallSignatures) -> object shapes
//   conditional / mapped / template-literal types -> usually NOT worth
//       resolving eagerly; capture the reference and resolve lazily, or
//       drop to typeToString and treat as opaque.
//
// Track visited types to break cycles — Type graphs are cyclic and lazy,
// unlike a finite .NET member list.
function extractType(ctx: LoadContext, t: ts.Type): TypeRef {
  return ctx.checker.typeToString(
    t,
    undefined,
    ts.TypeFormatFlags.NoTruncation | ts.TypeFormatFlags.UseFullyQualifiedType
  );
}

// ─── 7. Plumbing ───────────────────────────────────────────────────────────

function readVersion(ctx: LoadContext, pkg: string): string | undefined {
  try {
    const r = ts.resolveModuleName(pkg, ctx.entryFile, ctx.options, ts.sys).resolvedModule;
    if (!r?.packageId) return undefined;
    return r.packageId.version;
  } catch {
    return undefined;
  }
}

function main() {
  const [projectDir, ...packages] = process.argv.slice(2);
  if (!projectDir || packages.length === 0) {
    console.error("usage: tsx ts-bridge-extractor.ts <projectDir> <package> [...]");
    process.exit(1);
  }
  const ctx = createLoadContext(packages, path.resolve(projectDir));
  const out = packages.map((p) => extractPackage(ctx, p));
  fs.writeFileSync("bridge-types.json", JSON.stringify(out, null, 2));
  console.error(`Wrote bridge-types.json (${out.length} package(s))`);
}

main();
