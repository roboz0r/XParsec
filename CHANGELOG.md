# Changelog

## 0.2.5 - 2025-12-09

### 0.2.5 Additions

- Adds to `CharParsers` `anyString`, `anyStringCI`, `anyStringBy` and more variants
- Adds to `Parsers` `skip`, `fold`, `fold1`, `foldUserState`, `folderUserState1`

### 0.2.5 Fixes

- Make `sepEndBy` allow main parser to succeed without consuming input - by @bisgardo
- Make JSON number parser locale-independent - by @bisgardo
- Improve docs for Combinators `sepBy` variants

## 0.2.4 - 2025-11-23

### 0.2.4 Bugfixes

- Correctly sort operators by precedence in `Operator.create` to handle overlapping token definitions.

## 0.2.3 - 2025-10-12

### 0.2.3 Features

- Adds combinators `countManySatisfies` `countMany1Satisfies` `skipManySatisfies` `skipMany1Satisfies`
- Adds operations to `ParserCE`: `Using`, `While`, `For`, `Combine`.

### 0.2.3 Bugfixes

- Fix inlining of the bind operator `>>=`
