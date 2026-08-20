# FIXME: AADL GUMBO grammar rejects postfix access on `In(...)`

**Status:** open
**Filed:** 2026-08-20
**Affects:** AADL front end only (SysMLv2/KerML front end is unaffected)

## Summary

The AADL GUMBO Xtext grammar treats `In(stateVar)` as a terminal expression. It cannot
be indexed (`In(sv)(0)`), have a field selected (`In(sv).f`), or have a builtin applied
(`In(sv).size`). Any state-variable guarantee that needs to reach *into* the pre-state
value of an array- or struct-typed state variable is therefore unwritable in AADL.

The equivalent contracts parse and resolve fine on the SysMLv2 side, so the two front
ends are **not at parity** for GUMBO state-var guarantees.

## Reproduction

In `jvm/src/test/resources/models/INSPECTA-models/micro-examples/microkit/gumbo-verus/structs_arrays/aadl_sysml/Gumbo_Structs_Arrays.aadl`,
uncomment the five state-var guarantees in the `ConsumerThr` compute clause
(the `noChange_*_StateVar_Guarantee` / `isSorted_*_StateVar_Guarantee` block).

OSATE reports:

```
mismatched input '(' expecting ';'    line 396   -- In(myArrayInt32_StateVar)(0)
mismatched input '(' expecting ';'    line 399   -- In(myArrayStruct_StateVar)(0).fieldSInt32
mismatched input '(' expecting ')'    line 403   -- In(myArrayInt32_StateVar)(i)
mismatched input ')' expecting ';'    line 403
Couldn't resolve reference to EObject 'i'.  line 403
mismatched input '(' expecting ')'    line 407   -- In(myArrayStruct_StateVar)(i).fieldSInt32
mismatched input '.' expecting ';'    line 407
mismatched input '.' expecting ';'    line 411   -- In(myStructArray_StateVar).fieldArray(i)
mismatched input '.' expecting ')'    line 411
```

All five clauses are currently commented out in that model. The same five are **live**
in the SysMLv2 twin at `.../structs_arrays/sysml/Gumbo_Structs_Arrays.sysml`, which is
why the two committed AIR instances disagree: `aadl_sysml` carries 17 `GclGuarantee`
nodes, `sysml` carries 22.

## Root cause

In `Gumbo.xtext` (osate-plugin repo, `aadl-gumbo/org.sireum.aadl.gumbo/src/org/sireum/aadl/gumbo/Gumbo.xtext`):

```xtext
PrimaryExpr returns GExpr
    : BaseExpr
    | {PostFixExpr} baseExp=AccessibleBaseExpr ( posts+=Postfix)*;

BaseExpr returns GExpr
    : ...
    | ({InStateExpr} 'In' '(') stateVar=[StateVarDecl|ID] ')'
    | ...
    | {ParenExpr} '(' exp=OwnedExpression ')'
    ;

AccessibleBaseExpr returns GExpr
    : {CallExpr}      id=[ecore::EObject|QualifiedAADLName] callSuffix=SlangCallSuffix
    | {RecordLitExpr} ...
    | {DataRefExpr}   portOrSubcomponentOrStateVar=[ecore::EObject|ID]
    | {ResultExpr}    'res'
    ;

Postfix: MemberAccess | ArrayAccess | BuiltinAccess;
```

`Postfix` attaches only to `AccessibleBaseExpr`. `InStateExpr` sits under the *other*
`PrimaryExpr` alternative (`BaseExpr`), so it can never carry a postfix. The same
limitation applies to `MaySend`, `MustSend`, `NoSend`, and `HasEvent`.

Note that `ParenExpr` is also a `BaseExpr`, so the obvious workaround does **not**
help — `(In(sv))(0)` fails for the same reason.

## Possible fixes

1. Move `InStateExpr` (and the send/event predicates) from `BaseExpr` into
   `AccessibleBaseExpr` so `Postfix*` applies. Needs a check that the resulting
   grammar stays LL-parseable, and that the GUMBO AST builder / Slang translator
   handle a postfixed `InStateExpr`.
2. Alternatively, add an explicit `InStateExpr ( posts+=Postfix )*` alternative to
   `PrimaryExpr`, leaving `AccessibleBaseExpr` untouched.

Either way the downstream translation to Slang must emit `In(sv)` as the receiver of
the index/select chain — the SysMLv2 path already produces exactly this shape, so its
`GclGuarantee` AST can be used as the reference encoding.

## When fixed

Uncomment the five guarantees in `aadl_sysml/Gumbo_Structs_Arrays.aadl` and regenerate
`aadl_sysml/.slang/Gumbo_Structs_Arrays_Sys_i_Instance.json`, then confirm the AADL and
SysMLv2 AIR instances agree on 22 `GclGuarantee` / 11 `Exp.Input` nodes.
