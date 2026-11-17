# Robust Rust/Verus and C Parsers for HAMR — Requirements

Status: requirements, not started. Audience: an implementing agent (Codex) and its reviewer.

## 1. Background

HAMR's Microkit reporter (`MicrokitReporterPlugin`) locates artifacts in generated and
user-edited code: Rust functions, impls, traits, structs, fields and constants; the GUMBO
contract clauses inside Verus `requires`/`ensures`; marker regions; and C functions and
shared-memory variables. It does this with two hand-written character scanners in
`jvm/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/reporting/`:

| File | Size | Produces |
|---|---|---|
| `RustParser.scala` | ~1,000 lines | `RustContainers.RustFile` |
| `CParser.scala` | ~300 lines | `CContainers.CFile` |
| `Parsers_Ext.scala` | | `Parsers.parseRust` / `Parsers.parseC` entry points (`tryParse` wraps them) |

These scanners match keywords character by character, balance braces, and fail on any
syntax they were not written for. That is a problem for the user-modifiable files
(`*_app.rs`, user C), where the syntax is open-ended.

**Goal:** provide more robust, very fast replacements for the C and Rust parsers codegen
currently uses. The new implementations need not be based on the existing scanners.
They must recover from malformed or unfamiliar source and expose exact source locations
for generated artifacts and user edits: functions, structs and their impl blocks, methods,
fields, parameters, local variables, contract clauses, and marker regions.

Typical queries include "where is GUMBO guarantee `latch` in `d1p_dst_app.rs`?", "where is
method `timeTriggered` of `impl d1p_dst`?", "which impl blocks syntactically name this
struct?", and "where is local variable `x` declared in this function/block?" Rust artifact
and GUMBO lookup must work for **both `verus! { ... }` macro syntax and
`#[verus_spec(...)]` attribute syntax**, including files mixing the two.

The symbol table is a source artifact index. Locating declarations and syntactically
matching a struct to impl self-type text do not require semantic name or trait resolution.
Robustness, lookup accuracy, and speed are the priorities; recovery must never fabricate
an exact declaration location from an opaque or ambiguous region.

## 2. Hard constraints

1. **No new external dependencies.** Use only what Sireum already ships. That rules out
   tree-sitter, Eclipse CDT, the ANTLR4 tool, syn/rust-analyzer (Rust programs), and any new
   ivy dependency. Do not use the ANTLR4 or ANTLR3 runtimes either: the parsers must be
   plain Slang.
2. **Hand-written recursive descent in Slang** (`// #Sireum` files), with Pratt (precedence
   climbing) parsing for expressions. Do not use Sireum's LL(k) parser generator: it has no
   error recovery and its fixed-k prediction cannot handle Rust.
3. **Location:** module `hamr-codegen-shared`
   (`shared/src/main/scala/org/sireum/hamr/codegen/...`), so the parsers are
   platform-neutral. Suggested packages: `...codegen.common.parsers.rust` and
   `...codegen.common.parsers.c`.
4. **Positions** use `org.sireum.message.Position` / `FlatPos`, as the current containers do
   (1-based line/column, 0-based offset, length; `uriOpt` set from the file path relative to
   the root dir the caller passes, as today).
5. **Diagnostics** go through `org.sireum.message.Reporter`. A syntax error is reported but
   never fatal: the parse always returns a tree and a symbol table.

## 3. Non-goals

- Type checking, type inference, trait resolution, borrow checking.
- Name resolution across `use`/paths (a possible later phase, not part of this work).
- Macro expansion. `macro_rules!` definitions and ordinary macro invocations are kept
  as token trees. The known Verus syntax containers listed in §5.3 are parsed internally
  without executing or expanding macros.
- C preprocessing. No `#include` expansion and no macro expansion (see §6).
- Pretty-printing or rewriting source.

## 4. Shared architecture (both languages)

- **Input and position contract.** The core parser accepts decoded Slang `String` source;
  offsets and lengths use the same character indexing as the existing reporting parsers,
  not UTF-8 byte offsets. Preserve the original source and map spans to it. File adapters
  decode UTF-8 without normalizing line endings; invalid encoding is an I/O diagnostic,
  not a syntax error. Byte-for-byte round trip applies to valid UTF-8 files; arbitrary
  decoded characters, including NUL, must still be accepted without crashing. Preserving
  undecodable original bytes is outside the core parser's contract.
- **Lossless lexer.** Every source character belongs to a token or to trivia. Trivia means whitespace,
  line comments, block comments (nested in Rust), and C preprocessor lines. Each trivia
  item keeps its position and is attached as *leading trivia* to the next token. Comments
  must never be thrown away: GUMBO ids and markers live in them. Attach trailing trivia
  to an explicit EOF token, including for empty or comment-only files. Splitting tokens
  for generic closers must preserve their original source slices without duplication.
- **Syntax tree.** A concrete syntax tree that carries positions, from which the symbol
  table is built. The tree can be untyped (kind + children + span); the symbol table is the
  typed API.
- **Error recovery** (a hard requirement):
  - At item level: when an item fails to parse, emit an `Error` node covering the skipped
    tokens, resynchronize at the next item-start keyword at the same brace depth or after
    the next balanced `}`/`;`, and continue.
  - Inside a function body or expression: same idea, at statement boundaries (`;`, `}`).
  - Track delimiter nesting over tokens, but allow missing/mismatched delimiters. Do not
    require globally balanced input before parsing. Use bounded recovery and synthetic
    missing delimiters where appropriate; synthetic tokens contribute no source text.
  - Every parse/recovery loop must advance the cursor, return, or consume a finite retry
    budget. Impose a documented, configurable nesting limit; exceeding it produces a
    diagnostic and a recovered region, never stack overflow or `halt`.
  - Errors are reported with positions. Items that parsed are fully present in the symbol
    table.
- **Never halt** on any decoded input, including garbage characters, unterminated strings
  and comments, and an empty file. Fuzz with random truncations of corpus files (§9).
- **Performance:** target linear lexing/parsing/index construction with bounded recovery,
  no repeated whole-file scans, and no unbounded speculative backtracking. Use source
  spans instead of repeatedly copying source substrings. Build declaration/name and
  scope indexes once; repeated name lookup must not reparse or scan the whole file.
  Target well under one second for a 10k-line generated file on the development machine
  after JVM warmup, including lexing, parsing, and index construction but excluding I/O.
  The spike must record reproducible timings, input bytes/tokens, machine/runtime,
  warmup, median/tail latency, and comparison with the existing scanners. Benchmark
  lookup latency and scaling over increasing file sizes as well as total parse time.
- **Declaration identity and scopes.** Give declarations stable identities within a parse
  (file, enclosing declaration/scope, source position), independent of display names.
  Record parameter and local binding identifiers individually, including destructuring,
  their declaration/name spans and lexical scope. Preserve shadowed declarations and
  duplicate names. Queries return candidates in source order or an explicit ambiguity;
  never silently choose a same-named method, conditional definition, or local variable.
  An enclosing scope is a syntactic container, not a claim about resolved references or
  precise semantic visibility. Opaque macro-generated declarations remain unknown.

## 5. Rust + Verus parser

### 5.1 Lexer

- Pin the supported Rust toolchain version, edition(s), and Verus commit in the spike
  report and test corpus manifest. Rust 2021 is the initial edition target; explicitly
  inventory newer or unstable syntax required by that Verus revision. Subsequent version
  upgrades require an explicit corpus/coverage review rather than a moving "current" target.
- Rust 2021 tokens: identifiers (including raw `r#ident`), keywords, lifetimes
  (`'a` vs char literal `'a'`), integer/float literals with suffixes (`0i32`, `1u8`, `1.0f64`),
  char/byte/string/byte-string/C-string literals, raw strings `r#"…"#` with any number of `#`,
  nested `/* /* */ */` block comments, doc comments (`///`, `//!`, `/** */`), and every
  punctuation/operator token.
- Verus-only tokens: `==>`, `<==>`, `<==`, `&&&`, `|||`, `=~=`, `!~=`, `=~~=`, `!~~=`, `===`,
  `!==`, plus the contextual keywords below (lex as identifiers and recognize them in the
  parser).
- `>>` / `>=` / `>>=` must be splittable when closing generic argument lists.

### 5.2 Rust grammar coverage

All of stable Rust syntax at item level, including:
- `mod` (inline and `mod foo;`), `use` trees, `extern crate`, `extern "C" { … }` blocks.
- `fn` (`const`/`async`/`unsafe`/`extern "abi"`, generics, where clauses, `self` forms),
  `struct` (named, tuple, unit), `enum`, `union`, `trait` (supertraits, associated
  types/consts, default methods), `impl` (inherent, trait, generic, negative), `type`, `const`,
  `static`, `macro_rules!`.
- Outer and inner attributes on every construct that allows them (items, fields, variants,
  params, statements, expressions, match arms).
- Visibility: `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`.
- Types: paths with generics, references, raw pointers, slices, arrays, tuples, `fn` pointers,
  `impl Trait`, `dyn Trait`, `!`, `_`, qualified paths `<T as Trait>::X`.
- Statements and expressions in full: `let`/`let … else`, blocks, `unsafe`/`const`/`async`
  blocks, closures, `if`/`if let` chains, `match` with guards, loops with labels,
  `break`/`continue` with labels and values, ranges, `?`, casts, method calls, turbofish,
  struct literals (with the no-struct-literal restriction in `if`/`while`/`match` heads),
  tuple/field access, indexing, macro invocations as token trees.
- Patterns: all forms, including `|` alternatives, ranges, `@` bindings, `ref`/`mut`, rest `..`.

Function bodies must be **fully parsed**, not skipped. When a body fails to parse, the
recovery of §4 still gives the function its full span.

### 5.3 Verus syntax

Verus code shows up in two forms, and both must be parsed:

**(a) Macro form**, `verus! { … }`. Its body is parsed as items in *Verus mode*:
- Function modes: `spec fn`, `proof fn`, `exec fn`, `open spec fn`, `closed spec fn`,
  `uninterp spec fn`, `broadcast proof fn`, `broadcast group`, `broadcast use`.
- Clauses after the signature and before the body, each a comma-separated expression list:
  `requires`, `ensures`, `recommends`, `decreases` (with `when` and `via`), `invariant`,
  `invariant_except_break`, `invariant_ensures`, `opens_invariants`, `no_unwind`, `returns`.
  Clauses are also allowed on loops.
- Named return in signatures: `-> (r: T)`.
- `ghost`/`tracked` on params, `let` bindings and fields; `Ghost<T>`/`Tracked<T>`
  (ordinary types).
- Expressions: the Verus operators of §5.1 with Verus precedence (`==>` right-associative
  and binding looser than `||`; `&&&`/`|||` as prefix bullet lists); `forall|x: T| e`,
  `exists|x| e`, `choose|x| e`; `x@` view; `x is Variant` / `x matches pat`;
  `assert(e)`, `assert(e) by { … }`, `assert(e) by(solver)`, `assert forall|x| … by { … }`,
  `assume(e)`; `proof { … }` blocks; `old(x)`, `final(x)`; `#[trigger]` and `#![trigger …]`
  inside quantifiers; `int`/`nat` literals and casts.
- Use the Verus syntax reference and `vstd` sources at the pinned revision as the
  specification. Wherever they disagree, follow what that pinned Verus accepts.

**(b) Attribute form**, used by HAMR-generated code:

```rust
#[verus_spec(
  requires
    // BEGIN MARKER TIME TRIGGERED REQUIRES
    // assume AADL_Requirement
    //   All outgoing event ports must be empty
    old(api).ack.is_none(),
    // END MARKER TIME TRIGGERED REQUIRES
  ensures
    // BEGIN MARKER TIME TRIGGERED ENSURES
    // guarantee latch
    //   record what arrives
    final(api).val.is_some() ==>
      (final(self).latest == final(api).val.unwrap()),
    // END MARKER TIME TRIGGERED ENSURES
)]
pub fn timeTriggered<API: d1p_dst_Full_Api>(&mut self, api: &mut d1p_dst_Application_Api<API>) { … }
```

- Re-parse the token tree inside `#[verus_spec(…)]` with the same clause parser as
  form (a). Attach specifications to the decorated function, loop, statement, or
  expression as permitted by the pinned Verus syntax; preserve that owner's identity.
- Also recognize `#[verus_verify]`, `#[verifier::…]` (e.g. `external_body`, `opaque`,
  `external`), and `#[verus_spec(r => …)]` named-return and `with` forms.
- Parse contents of `verus!`, `proof!`, `proof_decl!`, and `proof_with!` according to
  their syntax at the pinned revision, including proof statements and ghost/tracked
  bindings. Inventory any additional required Verus syntax containers during the spike.
  Unknown attributes and unrelated macros stay as token trees with explicit opaque status.
- Both forms must produce **the same symbol-table structure** for clauses and artifact
  lookup, while retaining source-form metadata. This is required for macro-only,
  attribute-only, and mixed files; neither form is a fallback or optional feature.

### 5.4 Comment-borne HAMR metadata

These line-comment shapes must be recognized, as the current parser recognizes them
(`RustParser.consumeWhiteSpaceAndCommentsH`):

| Comment | Meaning |
|---|---|
| `// BEGIN MARKER <id>` / `// END MARKER <id>` | marker region `<id>` (the rest of the line, trimmed) |
| `// guarantee <id>` | GUMBO guarantee clause `<id>` |
| `// assume <id>` | GUMBO assume clause `<id>` |
| `// case <id>` | GUMBO case clause `<id>` |

Indented `//   description` lines that follow belong to the same clause as free text.

Attachment rules:
- A GUMBO-id comment attaches to **the next clause expression** in the enclosing
  `requires`/`ensures` list. That clause's span is the expression's span, without the
  trailing comma. Today the span runs from the id comment to the end of the expression;
  keep both spans (`idPos`, `exprPos`) so the reporter can choose.
- A clause with no preceding GUMBO comment is still recorded, with `gumboId = None()`.
- Marker regions pair BEGIN/END with the same id. Record the region's span and the
  enclosing symbol (struct, fn, clause list). Report a warning (not an error) for an
  unmatched or mis-nested marker.
- Recognize these comments anywhere: in field lists, struct literals, clause lists, bodies.

### 5.5 Rust symbol table

A typed `@datatype` model, roughly:

- `RustFile(items, errors)`, each item having a qualified path (module nesting, including
  `verus!` blocks, which are transparent) and positions for the **whole item**, the **name
  identifier**, and, where present, the **body** and **signature**.
- Items: `Module`, `Use`, `Fn`, `Struct` (fields with visibility, `ghost`/`tracked`, position),
  `Enum` (variants), `Union`, `Trait` (methods, associated types/consts), `Impl` (self type as
  path text, generics, optional trait path, methods/consts/types), `Const`, `Static`,
  `TypeAlias`, `ExternBlock` (fns, statics), `MacroRules`, `MacroCall`, `Error`.
- `Fn`: name, visibility, qualifiers (`const`/`unsafe`/`async`/`extern`), Verus mode
  (`spec`/`proof`/`exec`/plain) and `open`/`closed`/`uninterp`/`broadcast`, generic params
  (text + position), params (pattern text, type text, `ghost`/`tracked`), return type text,
  attributes, clauses.
- `Clause`: kind (`requires`/`ensures`/`recommends`/`decreases`/`invariant`/…), the
  form it came from (macro or attribute), and its entries. Each entry has an expression span,
  an optional `GumboId(kind = Assume|Guarantee|Case, id, idPos, description)`, and its
  enclosing marker id, if any.
- `Marker(id, beginPos, endPos, regionPos)`.
- Parameters and local bindings: identifier, pattern/declaration/name spans, enclosing
  function/closure/block scope, mutability, and ghost/tracked annotations. Include bindings
  in `let`, closure/function parameters, `if let`, `while let`, `for`, and match patterns.
- Lookups (memoized where possible), at least:
  - `functions`, `structs`, `traits`, `impls` (top-level and qualified);
  - `getImpl(selfType, traitOpt, genericOpt)`, `implMethod(selfType, method)`,
    `traitMethod(trait, method)`;
  - `fnByPath(path)`;
  - `fields(owner)`, `field(owner, name)`, `implsForType(selfTypeText)`;
  - `parameters(functionId)`, `locals(functionId)`, `bindings(scopeId, name)`;
  - `gumboClause(fnPath, id)` → entry;
  - `marker(id)` → regions;
  - `symbolAt(offset)` → innermost recorded artifact/clause containing an offset;
  - `allSymbols` → flat list of (qualified name, kind, name pos, item pos), for
    declaration-location queries, including fields, parameters, and locals. These are
    source declaration lookups, not semantic go-to-definition for identifier uses.

Impl/type matching is syntactic and documented (including whitespace normalization);
aliases, imports, and generic equivalence are not resolved. Preserve distinct impls and
same-named methods, with candidate/ambiguity behavior as specified in §4.

## 6. C parser

### 6.1 Lexer
- C11/C17 tokens, including universal character names, all literal forms with suffixes and
  prefixes (`u8"…"`, `L'x'`), digraphs, and line continuations.
- **Preprocessor directive lines** (`#include`, `#define` spanning continuations, `#if`/
  `#ifdef`/`#elif`/`#else`/`#endif`, `#pragma`, `#error`, `_Pragma(...)`) become trivia,
  with their text and position. Directives are not expanded. Record `#include` targets and
  `#define` names (object-like and function-like) in the symbol table as `Include`/`Macro`.

### 6.2 Grammar coverage
- All C11/C17 declarations: storage classes, qualifiers, `_Alignas`, `_Atomic`,
  `_Static_assert`, `_Thread_local`, `_Noreturn`, `_Generic`, struct/union/enum with
  bitfields, anonymous members, designated initializers, compound literals, function
  pointers and abstract declarators, K&R-free function definitions, variadics.
- Full statement and expression grammar.
- GCC/Clang extensions seen in seL4/Microkit/sDDF code: `__attribute__((…))` anywhere it may
  appear, `__asm__`/`asm` (including `volatile`/`goto` and operand lists),
  `__typeof__`/`typeof`, statement expressions `({ … })`, `__extension__`,
  `__builtin_*` calls, `__restrict`, `__inline`, `_Bool`, case ranges `case 1 ... 5:`,
  empty structs, zero-length arrays, labels as values (`&&label`, `goto *p`).
- **Typedef ambiguity** (`T * x;`, `(T)(x)`): keep a scoped typedef-name table that the
  parser updates while it parses and consults when it needs to decide. Pre-seed it with
  the standard typedefs (`size_t`, `uintN_t`, `intN_t`, `uintptr_t`, `bool`, …) and with any
  `typedef` found earlier in the same file. When a name is still ambiguous (it comes from an
  unseen header), assume a type when the token after it can only start a declarator, and
  record the guess in the table. Distinguish guessed names from explicit typedefs;
  ordinary declarations can shadow typedef names, and branch-local guesses must not leak
  into sibling conditional branches.
- **Macros used as syntax**, which an unpreprocessed parse will meet: an identifier call in
  declaration-specifier position (`DECLARE_X(name);`), macros standing in for
  qualifiers/attributes (`MY_INLINE static int f(…)`), and a bare macro statement without `;`.
  Handle these heuristically and recover where needed. Extract declarations that remain
  syntactically identifiable; mark guesses and opaque/skipped spans explicitly. Declaration
  extraction from arbitrary macro expansions cannot be guaranteed without preprocessing.
- **Conditional compilation:** retain directive groups and branch boundaries. Parse each
  independently parseable branch with isolated typedef state, tag declarations with its
  enclosing condition text (including preceding alternatives for `#elif`/`#else`), and
  preserve duplicate declarations. Do not concatenate mutually exclusive branch tokens.
  For branches that split declarations or share unmatched delimiters with surrounding
  source, use bounded best-effort recovery, preserve uncertain/skipped regions and their
  conditions, and report coverage limitations. Extraction of every definition is guaranteed
  for independently parseable branches, not arbitrary cross-branch syntax. Do not explore
  every combination of nested conditionals or claim a selected active configuration.

### 6.3 C symbol table
- `CFile(items, includes, macros, errors)`.
- Items: `Function` (definition vs prototype, name, return type text, params, storage
  class, attributes, name pos, signature pos, body pos), `Global` (name, type text,
  qualifiers including `volatile`/`const`, storage class, initializer present, positions),
  `Struct`/`Union`/`Enum` (tag, members/enumerators with positions), `Typedef`, `Error`.
- Index struct/union fields, function parameters, and local declarations (including `for`
  declarations) with individual identifier and declaration spans, enclosing lexical scope,
  and conditional provenance. Preserve multiple declarators and shadowed names.
- Lookups: `function(name)`, `global(name)`, `typedef(name)`, `struct(tag)`,
  `fields(owner)`, `field(owner, name)`, `parameters(functionId)`, `locals(functionId)`,
  `bindings(scopeId, name)`, `symbolAt(offset)`, `allSymbols`. Duplicate/conditional
  candidates follow §4; unique convenience lookups must expose ambiguity explicitly.
- The same marker comments as Rust (`// BEGIN MARKER` / `// END MARKER`, also in `/* */`
  form) are recognized and recorded.

## 7. Integration

1. Add the new parsers next to the existing ones; do not delete the old ones until step 3.
2. Add adapters that turn the new symbol tables into the existing `RustContainers.RustFile` /
   `CContainers.CFile`, so `MicrokitReporterPlugin` keeps working unchanged. Every lookup it
   does today must give an equal result:
   - `structs`, `getImpl`/`getImplH`, `functions`, `traits`, `externs()`, `methods`;
   - `RustFn.requires/ensures.gumboClauses` with `getAssumeClausePos`/`getGuaranteeClausePos`;
   - `CFile.getMethgod`, `CFile.getField`.
   The positions the adapters produce must match today's for every file the current parser
   handles. Where the new span is deliberately more precise, document it and update the
   reporter's expected outputs only after the owner has reviewed the diff.
3. Switch `Parsers.parseRust`/`parseC` to the new parsers behind a flag (a CLI/system
   property, defaulting to the old parsers). Once the owner signs off, make the new parsers
   the default and remove the old ones.
4. The new parser path must return recovered trees/indexes even when syntax diagnostics
   contain errors. Today's `tryParse` discards results when its local reporter has errors;
   adapt or bypass that behavior for the new parsers, distinguishing recovered syntax
   errors from I/O failures. Keep necessary I/O exception handling and the existing
   protection for the old parsers and MSD parser. Partial results must remain safe for
   reporter consumers; missing/ambiguous artifacts must not cause unchecked lookups.

## 8. Slang coding rules (must follow)

The full rules are in the kekinian `CLAUDE.md`. The ones that matter most here:
- Every Slang file starts with `// #Sireum` and has `import org.sireum._` as the first import.
- Non-`@strictpure` methods use an explicit `return` on every path, including every `match`
  arm. `@strictpure` methods are single expressions (no `return`, `var`, loops or nested
  `def`s); prefer them where possible.
- No method overloading; no nested classes (put types at top level or in a top-level
  `object`).
- `@datatype` is immutable and `@record` is mutable; immutable types cannot hold mutable
  ones. The lexer/parser state can be a `@record` class or local `var`s with nested `def`s,
  but the resulting trees and symbol tables must be `@datatype`.
- Lambdas cannot capture `var`s; nested `def`s can mutate outer `var`s but cannot escape.
- `String` in Slang has no `+`; use `st"$a$b".render`. Read characters via
  `conversions.String.toCis(text)` and indexing (there is no `charAt`).
- `C` supports `<`/`<=` directly; convert with `c.toZ`.
- `Z` has no bitwise ops (use `U32` etc. if needed).
- Do not use `halt`, throwing assertions, or unchecked indexing/option extraction for
  input-dependent parser conditions, including hot loops. Report and recover instead.
- Calls with more than 3 arguments use named arguments.
- Keep Scaladoc in sync with any method you change.

## 9. Testing and acceptance

Test suites are plain Scala `TestSuite`s under `jvm/src/test/scala/...` (the
`hamr-codegen-test` submodule, `jvm/src/test`). Suggested suites: `RustParserTests`,
`VerusParserTests`, `CParserRobustTests`.

**Corpus.** Run the parsers over:
- every `.rs` and `.c`/`.h` file under `jvm/src/test/resources` and under codegen output
  directories produced by the existing test suites;
- the INSPECTA models' generated crates;
- `vstd` sources and the Verus `examples/` directory, when a Verus checkout is available
  (skip with `cancel(...)` if not);
- sDDF and Microkit SDK headers/sources, when available (skip otherwise).

**Acceptance criteria:**
1. **No crash.** No exception or `halt` on any corpus file, nor on random truncations,
   byte deletions and insertions of them (a seeded fuzz test, at least 1,000 mutations per
   run).
2. **Coverage.** 100% of HAMR-generated Rust and C files parse with zero errors. ≥ 99% of
   `vstd`/Verus-example files parse with zero errors; list the failures. Report parsed,
   opaque, recovered, and skipped spans separately, including token counts. Required
   grammar constructs may not be hidden as opaque regions to meet the zero-error target;
   ordinary macro token trees explicitly allowed by §3 are counted separately.
3. **Recovery.** Insert a syntax error into one item of a file: every other item is still
   present in the symbol table with correct positions in the edited source. Equal-length
   edits preserve unaffected offsets; insertions/deletions shift subsequent spans by the
   corresponding source-length delta. Include missing delimiters and excessive nesting.
4. **GUMBO.** For every generated `*_app.rs` and GUMBOX file in the test resources, each
   GUMBO id has exactly one clause entry, its expression span matches the source, and it
   sits in the right marker region. This includes user-style edits: reordered clauses, extra
   comments, blank lines, clauses on one line, trailing commas.
5. **Parity.** The adapters reproduce, for every file the old parser accepts, exactly what
   `MicrokitReporterPlugin` consumes today. Before switching the default, the reporter's
   existing tests pass unchanged.
6. **Round trip.** Concatenating all tokens and trivia reproduces decoded input exactly,
   including EOF trivia and error regions. Re-encoding valid UTF-8 files reproduces their
   bytes exactly, including BOM and original line endings. Test Unicode and NUL explicitly.
7. **Position spot checks.** Unit tests pin the name/item/body spans for a representative
   item of every kind in both languages, including fields, parameters, locals, shadowing,
   destructuring, duplicate impl methods, and conditional C definitions.
8. **Both Verus forms.** Paired macro/attribute fixtures and mixed files must provide
   equivalent artifact and GUMBO lookup results (allowing different source positions and
   source-form metadata), including loop contracts and proof-local bindings.
9. **Speed.** Record parse/index and repeated lookup benchmarks using the §4 protocol,
   demonstrate scaling, and report regressions against the spike baseline before sign-off.

**Verification commands** (from the kekinian root; module ids are in
`hamr/codegen/project.json`, where the Slang module is `hamr-codegen-shared` and the JVM/test
module is `hamr-codegen`):
- type check: `bin/sireum proyek tipe --slice hamr-codegen-shared .`
- compile: `bin/sireum proyek compile --slice hamr-codegen .`
- test: `bin/sireum proyek test --slice hamr-codegen --classes org.sireum.hamr.codegen.test.<Suite> .`

Replace `<Suite>` with an actual suite name. Positional arguments after the project
directory select root packages; use `--classes` for specific suites. In a standalone
hamr-codegen checkout, use `$SIREUM_HOME/bin/sireum` and add
`--project bin/project-standalone.cmd` before the project directory for each command,
as in the standalone build script. Do not use the kekinian-relative `bin/sireum` there.

Never regenerate or accept expected test outputs (golden files) without the owner's
explicit approval; report the diffs instead.

## 10. Phasing

| Phase | Deliverable | Exit criterion |
|---|---|---|
| 0. Spike (1–2 weeks) | Rust lexer + item-level parser (bodies initially token trees) + item index + clause parsing/comment attachment for both Verus forms; C feasibility samples and performance baseline | Every repo `.rs` file returns a tree/index without crashing; distinguish clean parses from recovered/opaque regions. GUMBO lookups match the current parser on test resources; report gaps, pinned versions, timings, and revised effort |
| 1. Rust full | Full bodies/expressions/patterns, error recovery, fuzz test | Criteria 1, 3, 6 for Rust |
| 2. Verus | Complete `verus!` mode, attribute form, proof containers, and Verus index fields | Criteria 2, 4, 8 for Rust |
| 3. Rust integration | Adapter, flag, parity test | Criterion 5 for Rust |
| 4. C | Lexer, parser, typedef table, macro heuristics, symbol table, adapter | Criteria 1–7 and 9 for C; documented conditional/macro limitations |
| 5. Switch-over | New parsers become the default; old scanners deleted | Owner sign-off |

Artifact lookups for locals become complete with full body parsing in Phase 1; Phase 2
extends them to Verus proof containers. Rust performance acceptance (criterion 9) is
reviewed during integration and before switch-over.

Planning estimate: approximately 6–10 person-months for the complete specification,
assuming an experienced compiler engineer and reuse of Sireum infrastructure. This is a
provisional estimate, to be revised after the spike; it is not the spike's duration.

Each phase is reviewed before the next one starts. Never create commits without asking
the owner first. Never push, including as part of an approved commit workflow. Never
regenerate or accept golden files without the owner's explicit approval.
