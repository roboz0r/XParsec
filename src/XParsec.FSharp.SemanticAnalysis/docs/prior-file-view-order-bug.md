# Prior-file views compose OLDEST-first, so the wrong file wins a re-declaration

Open defect. Not a plan: this file stays until the bug is fixed, then goes with it.

## Symptom

Within one assembly, a name declared in two files resolves to the EARLIER file's declaration.
The intended rule is the opposite, and `AssemblyFiles.fs:92` states it: *"a name a nearer file
re-declares shadows a farther one's."*

## The mechanism

`priorViews` is built by PREPENDING (`AssemblyFiles.fs:128`):

```fsharp
priorViews <- view :: priorViews
```

so after file1, file2 it holds `[v2; v1]` — newest at the head. It is then reversed before
composing (`AssemblyFiles.fs:109`):

```fsharp
ExternalSymbolProviders.composite ((List.rev priorViews) @ [ external ])
```

giving `[v1; v2; external]` — OLDEST at the head. `composite` → `stack` → `firstHit` scans from
index 0 and stops at the first hit (`ExternalSymbolProviders.fs:239-253`), so `v1` wins. The
`List.rev` is what inverts the intent; without it the list is already nearest-first.

The comment above it (`AssemblyFiles.fs:99-100`) mis-describes the data it is reasoning about —
it calls `priorViews` "FILE ORDER (oldest first)" and then says the head is the newest, which
cannot both be true of one list. The `List.rev` follows from the wrong half.

## Codegen has the same order and the same wrong comment

`ClrDriver.fs:141-142` composes `[ for f in analysed.Files -> f.View ] @ [ external ]` — file
order, oldest first — under a comment claiming "nearest-first".

So analysis and codegen AGREE with each other; both disagree with the documented rule. (An
earlier scouting pass reported these two sites as disagreeing, which is wrong — worth stating
because it is the more alarming reading and it is not what the code does.)

## Why nothing catches it

No file in any Vesper package re-declares a name another file in the SAME package declares.
The rule is stated but never exercised.

## Fix must include

A test in `AssemblyFilesTests` declaring one name in two files of one assembly and pinning that
a third file sees the NEARER declaration — and the codegen counterpart, since the two compose
independently and only a test keeps them in step. Fixing one site and not the other would
create the divergence that does not exist today.
