# Unreleased

* Updated verus dependencies to 0.2025.09.25.04e8687

<details><summary>How to build</summary>

```
git clone --rec --depth 1 https://github.com/sireum/kekinian.git
cd kekinian
./bin/build.cmd
```
</details>

<!-- begin unreleased commits -->

<details><summary>Commits</summary>

* [ac97ca7](https://github.com/sireum/hamr-codegen/commit/ac97ca7) update submodule

* [d23d0b6](https://github.com/sireum/hamr-codegen/commit/d23d0b6) microkit: update verus dependencies to 0.2025.09.25.04e8687 release

* [6b056fd](https://github.com/sireum/hamr-codegen/commit/6b056fd) move scripts

* [9e82bc0](https://github.com/sireum/hamr-codegen/commit/9e82bc0) update changelog
</details>
<br>
<!-- end unreleased commits -->

<!-- released -->
# [4.20250924.c877daf](https://github.com/sireum/kekinian/releases/tag/4.20250924.c877daf)

**Microkit**

  * Added PreState containers and put_concrete_input methods to test apis

  * Added support for smc and passive attributes for protection domains

  * Added inverted markers to microkit system description file to allow use contributions

  * Updated Verus dependencies to 0.2025.09.07.6129810

  * Makefiles use cargo-verus by default for building/verification

  * Added missing getter integration constraints to the unverified apis

  * Added custom proptest strategies for AADL enums

**Phantom**

  * Updated to OSATE 2.17

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250924.c877daf https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [5f7072a](https://github.com/sireum/hamr-codegen/commit/5f7072a) update submodule

* [36dcd70](https://github.com/sireum/hamr-codegen/commit/36dcd70) microkit: add put_concrete_input test apis

* [665d668](https://github.com/sireum/hamr-codegen/commit/665d668) microkit: add put_concrete_input test apis

* [c311fc1](https://github.com/sireum/hamr-codegen/commit/c311fc1) update phantom to 2.17

* [c7b2ce7](https://github.com/sireum/hamr-codegen/commit/c7b2ce7) add microkit support for smc and passive protection domain attributes

* [3933605](https://github.com/sireum/hamr-codegen/commit/3933605) update submodule

* [38117b2](https://github.com/sireum/hamr-codegen/commit/38117b2) update build props

* [a8d1ebc](https://github.com/sireum/hamr-codegen/commit/a8d1ebc) update submodule

* [11ea33d](https://github.com/sireum/hamr-codegen/commit/11ea33d) add invert markes to msd

* [e48188e](https://github.com/sireum/hamr-codegen/commit/e48188e) don't replace new lines in json files

* [198598e](https://github.com/sireum/hamr-codegen/commit/198598e) update submodule

* [604eb1e](https://github.com/sireum/hamr-codegen/commit/604eb1e) update to verus 0.2025.09.07.6129810 switch to use cargo-verus add get integration constraints to unverified apis add custom proptest strategies for enums
</details>
<br>


# [4.20250825.20d1bda](https://github.com/sireum/kekinian/releases/tag/4.20250825.20d1bda)

**Microkit**

  * Added attestation plugin

  * Added Microkit codegen reporting plugin

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250825.20d1bda https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [de01932](https://github.com/sireum/hamr-codegen/commit/de01932) echo provisioning when verbose

* [b4c649a](https://github.com/sireum/hamr-codegen/commit/b4c649a) normalize newlines

* [91e2f74](https://github.com/sireum/hamr-codegen/commit/91e2f74) update submodule

* [596f031](https://github.com/sireum/hamr-codegen/commit/596f031) resolve plugin loop

* [e1b2197](https://github.com/sireum/hamr-codegen/commit/e1b2197) set exit code when appraisal fails

* [47325e2](https://github.com/sireum/hamr-codegen/commit/47325e2) camkes container tweak

* [339672f](https://github.com/sireum/hamr-codegen/commit/339672f) dewin paths

* [116a317](https://github.com/sireum/hamr-codegen/commit/116a317) camkes container tweak

* [74bad8f](https://github.com/sireum/hamr-codegen/commit/74bad8f) remove attestation hack

* [b461bff](https://github.com/sireum/hamr-codegen/commit/b461bff) tweak

* [09833c9](https://github.com/sireum/hamr-codegen/commit/09833c9) add language prefix to attestation files

* [1a0099f](https://github.com/sireum/hamr-codegen/commit/1a0099f) mod name

* [972ea04](https://github.com/sireum/hamr-codegen/commit/972ea04) update submodule

* [5448a33](https://github.com/sireum/hamr-codegen/commit/5448a33) add attestation plugin

* [91c9a94](https://github.com/sireum/hamr-codegen/commit/91c9a94) t

* [951f674](https://github.com/sireum/hamr-codegen/commit/951f674) tipe fix

* [470534c](https://github.com/sireum/hamr-codegen/commit/470534c) relativize paths in json files

* [12efc23](https://github.com/sireum/hamr-codegen/commit/12efc23) update submodule

* [85d83d1](https://github.com/sireum/hamr-codegen/commit/85d83d1) add tool report

* [54585ae](https://github.com/sireum/hamr-codegen/commit/54585ae) update submodule

* [cee4ff3](https://github.com/sireum/hamr-codegen/commit/cee4ff3) add codegen reporting
</details>
<br>


# [4.20250721.f13c8b0e](https://github.com/sireum/kekinian/releases/tag/4.20250721.f13c8b0e)

**Microkit**

  * Added proptest support

  * Added testing apis to bridge/testing_apis.rs 

  * Switched from Hamr::Bit_Codec_Max_Size to Memory_Properties::Data_Size when specifying the maximum size required to store a value of an AADL datatype

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250721.f13c8b0e https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8961e7b](https://github.com/sireum/hamr-codegen/commit/8961e7b) tipe fixes

* [160b3f1](https://github.com/sireum/hamr-codegen/commit/160b3f1) add proptest support

* [5377e87](https://github.com/sireum/hamr-codegen/commit/5377e87) tipe fix

* [1fa27dd](https://github.com/sireum/hamr-codegen/commit/1fa27dd) update submodule

* [5cfb420](https://github.com/sireum/hamr-codegen/commit/5cfb420) switch to Data_Size property

* [72a4532](https://github.com/sireum/hamr-codegen/commit/72a4532) add testing apis

* [84ae959](https://github.com/sireum/hamr-codegen/commit/84ae959) update submodule

* [8c0bd65](https://github.com/sireum/hamr-codegen/commit/8c0bd65) add repo check

* [63a98f0](https://github.com/sireum/hamr-codegen/commit/63a98f0) add microkit behavior tests
</details>
<br>


# [4.20250630.0392c909](https://github.com/sireum/kekinian/releases/tag/4.20250630.0392c909)

**Microkit**

  * Uses conjunction instead of implication when emitting Verus/Rust quantified expressions

  * Use cargo when verifying via Verus

  * Added Microkit AADL-profile linter

  * Use macros for for exec versions of implications to ensure proper short-circuit evaluations

  * Added GUMBO to Verus array support

**JVM**

  * Use the first enum value instead of byOrdinal(0) for enums when emitting example api usage

  * Added support for fixed-sized AADL array datatypes 

  * Added GUMBO to Slang/Logika array support

**Phantom**

  * Update to use OSATE 2.16

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250630.0392c909 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [2162bad](https://github.com/sireum/hamr-codegen/commit/2162bad) update submodule

* [ba85cea](https://github.com/sireum/hamr-codegen/commit/ba85cea) build props

* [43059ed](https://github.com/sireum/hamr-codegen/commit/43059ed) update submodule

* [69d6df4](https://github.com/sireum/hamr-codegen/commit/69d6df4) use conjunction instead of implication when rewriting quantification expressions

* [f62bb30](https://github.com/sireum/hamr-codegen/commit/f62bb30) update submodule

* [bda9737](https://github.com/sireum/hamr-codegen/commit/bda9737) use cargo for verus

* [2f62858](https://github.com/sireum/hamr-codegen/commit/2f62858) update submodule

* [e8666e0](https://github.com/sireum/hamr-codegen/commit/e8666e0) refine array message

* [388792e](https://github.com/sireum/hamr-codegen/commit/388792e) rewrite T/F Idents

* [ec105fb](https://github.com/sireum/hamr-codegen/commit/ec105fb) update submodule

* [0588322](https://github.com/sireum/hamr-codegen/commit/0588322) update build props

* [da2b40f](https://github.com/sireum/hamr-codegen/commit/da2b40f) add support for fixed, bounded, unbounded arrays.

* [ed60c1d](https://github.com/sireum/hamr-codegen/commit/ed60c1d) update submodule

* [48b5138](https://github.com/sireum/hamr-codegen/commit/48b5138) update submodule

* [6d84ca6](https://github.com/sireum/hamr-codegen/commit/6d84ca6) update build props

* [6db5df6](https://github.com/sireum/hamr-codegen/commit/6db5df6) adapt to changes to ArrayType, add linter

* [a36f224](https://github.com/sireum/hamr-codegen/commit/a36f224) add gumbo array support for jvm

* [1032e6a](https://github.com/sireum/hamr-codegen/commit/1032e6a) update submodule

* [85914bc](https://github.com/sireum/hamr-codegen/commit/85914bc) mickrokit: use macros for exec versions of implication, fix testing port get apis for event data ports

* [8f970d4](https://github.com/sireum/hamr-codegen/commit/8f970d4) Fixed pattern var binding name shadowing check.

* [6705e8e](https://github.com/sireum/hamr-codegen/commit/6705e8e) update submodule

* [0928e1e](https://github.com/sireum/hamr-codegen/commit/0928e1e) update osate version
</details>
<br>


# [4.20250603.ae9204b1](https://github.com/sireum/kekinian/releases/tag/4.20250603.ae9204b1)

**GUMBO**

   * Added AADL array datatype support

**Ros2**

  * Added example api usage to the generated behavior code

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250603.ae9204b1 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8ffcee9](https://github.com/sireum/hamr-codegen/commit/8ffcee9) update submodule

* [c7e891b](https://github.com/sireum/hamr-codegen/commit/c7e891b) update versions

* [a98ab33](https://github.com/sireum/hamr-codegen/commit/a98ab33) update versions

* [8475f1f](https://github.com/sireum/hamr-codegen/commit/8475f1f) update instructions

* [9ebb427](https://github.com/sireum/hamr-codegen/commit/9ebb427) update submodule

* [20a8957](https://github.com/sireum/hamr-codegen/commit/20a8957) add array gubmo/x support

* [538a5a4](https://github.com/sireum/hamr-codegen/commit/538a5a4) allow plugins to be disabled, adapt to gumbo changes

* [9afe71a](https://github.com/sireum/hamr-codegen/commit/9afe71a) Actually fixed the bug, and minor formatting changes

* [fbe4c8d](https://github.com/sireum/hamr-codegen/commit/fbe4c8d) Another pass at fixing the bug

* [792d98b](https://github.com/sireum/hamr-codegen/commit/792d98b) Fixed minor bug

* [d8c99c8](https://github.com/sireum/hamr-codegen/commit/d8c99c8) Completed behavior code example code generation

* [29510c1](https://github.com/sireum/hamr-codegen/commit/29510c1) Added behavior code examples

* [feb77fb](https://github.com/sireum/hamr-codegen/commit/feb77fb) Removed code that caused type-check error

* [f478159](https://github.com/sireum/hamr-codegen/commit/f478159) Added ROS2 codegen option (invertTopicBinding) and added topic inversion option into generator

* [1fd9809](https://github.com/sireum/hamr-codegen/commit/1fd9809) Overhauled launch files; also bug fixes
</details>
<br>


# [4.20250421.c5f6fe7d](https://github.com/sireum/kekinian/releases/tag/4.20250421.c5f6fe7d)

**Microkit**

  * logging.rs no longer overwritten

  * Add verify and test entries to rust crate Makefiles

  * Added GUMBO to Verus translation support

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250421.c5f6fe7d https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8dff417](https://github.com/sireum/hamr-codegen/commit/8dff417) update submodule

* [e84ece1](https://github.com/sireum/hamr-codegen/commit/e84ece1) don't overwrite logging.rs, make app optional, allows new to be non-const

* [e2c7834](https://github.com/sireum/hamr-codegen/commit/e2c7834) update submodule

* [ae96283](https://github.com/sireum/hamr-codegen/commit/ae96283) update submodule

* [3d84a33](https://github.com/sireum/hamr-codegen/commit/3d84a33) remove warnings

* [f385067](https://github.com/sireum/hamr-codegen/commit/f385067) fix array/string handling

* [a44734b](https://github.com/sireum/hamr-codegen/commit/a44734b) update submodule

* [520a0eb](https://github.com/sireum/hamr-codegen/commit/520a0eb) fix annex clause lookup

* [8a5d807](https://github.com/sireum/hamr-codegen/commit/8a5d807) always use classifier when referring to data components

* [2ca8df2](https://github.com/sireum/hamr-codegen/commit/2ca8df2) tipe fix

* [f31ca2c](https://github.com/sireum/hamr-codegen/commit/f31ca2c) add crusty makefile verify and test entries

* [1e01ceb](https://github.com/sireum/hamr-codegen/commit/1e01ceb) further slang exp 2 rust refinements, add slang exp 2 rust test suite, add Slang interpolates imports for Slang APIs

* [1b10892](https://github.com/sireum/hamr-codegen/commit/1b10892) refined Slang to Rust expression translation

* [c58377d](https://github.com/sireum/hamr-codegen/commit/c58377d) merge fork, add microkit plugin support, add verus support
</details>
<br>


# [4.20250321.dc36763](https://github.com/sireum/kekinian/releases/tag/4.20250321.dc36763)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250321.dc36763 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [63ff928](https://github.com/sireum/hamr-codegen/commit/63ff928) merge auxResources

* [cbfdff5](https://github.com/sireum/hamr-codegen/commit/cbfdff5) added changelog

* [06e4a3e](https://github.com/sireum/hamr-codegen/commit/06e4a3e) use passed in reporter

* [6642db8](https://github.com/sireum/hamr-codegen/commit/6642db8) update submodule

* [2b3cf90](https://github.com/sireum/hamr-codegen/commit/2b3cf90) update build props

* [c062836](https://github.com/sireum/hamr-codegen/commit/c062836) add finalizePlugin

* [b25bc58](https://github.com/sireum/hamr-codegen/commit/b25bc58) refactor plugin architecture

* [ed9d34b](https://github.com/sireum/hamr-codegen/commit/ed9d34b) correct gumbo positions, cleanup

* [1f818cf](https://github.com/sireum/hamr-codegen/commit/1f818cf) Added unclonable record and anvil.{hls, test} annotation support.

* [10231c8](https://github.com/sireum/hamr-codegen/commit/10231c8) Refactored Slang modules.

* [de2e4e6](https://github.com/sireum/hamr-codegen/commit/de2e4e6) update submodule

* [19ac373](https://github.com/sireum/hamr-codegen/commit/19ac373) update build props

* [50e9078](https://github.com/sireum/hamr-codegen/commit/50e9078) ensure ostate/gumbo derived binary expressions have a position for the operator

* [fe6f516](https://github.com/sireum/hamr-codegen/commit/fe6f516) update submodule

* [ad20c0a](https://github.com/sireum/hamr-codegen/commit/ad20c0a) update build prop

* [fabd72e](https://github.com/sireum/hamr-codegen/commit/fabd72e) update submodule

* [38738e4](https://github.com/sireum/hamr-codegen/commit/38738e4) t

* [c947be8](https://github.com/sireum/hamr-codegen/commit/c947be8) update keys

* [a88f419](https://github.com/sireum/hamr-codegen/commit/a88f419) Regenerated datatype-examples results

* [99e93c4](https://github.com/sireum/hamr-codegen/commit/99e93c4) Ignoring user code retention tests (relied on results directory, which isn't part of the git repo)

* [51da299](https://github.com/sireum/hamr-codegen/commit/51da299) Removed code that caused type-check error

* [a2f4e86](https://github.com/sireum/hamr-codegen/commit/a2f4e86) Added ROS2 codegen option (invertTopicBinding) and added topic inversion option into generator

* [1c8078b](https://github.com/sireum/hamr-codegen/commit/1c8078b) Added sections for user-added content that won't be overwritten during code regeneration

* [e529aa3](https://github.com/sireum/hamr-codegen/commit/e529aa3) Overhauled launch files; also bug fixes

* [8273c7f](https://github.com/sireum/hamr-codegen/commit/8273c7f) Corrected multidimensional array handling, edited enum converter

* [6e10923](https://github.com/sireum/hamr-codegen/commit/6e10923) Added enum-to-string converter

* [a506d4c](https://github.com/sireum/hamr-codegen/commit/a506d4c) Removed 'empty' message from event port handlers

* [bf093fb](https://github.com/sireum/hamr-codegen/commit/bf093fb) Fixed incorrect method name

* [ec1aab7](https://github.com/sireum/hamr-codegen/commit/ec1aab7) Removed top-level system name from generated code

* [0693885](https://github.com/sireum/hamr-codegen/commit/0693885) Added datatype handling

* [174b4df](https://github.com/sireum/hamr-codegen/commit/174b4df) update submdoule

* [26983cb](https://github.com/sireum/hamr-codegen/commit/26983cb) update submodule

* [e90f8e9](https://github.com/sireum/hamr-codegen/commit/e90f8e9) update submodule

* [444acde](https://github.com/sireum/hamr-codegen/commit/444acde) update submodule

* [507e570](https://github.com/sireum/hamr-codegen/commit/507e570) update submodule
</details>
<br>


# [4.20250211.0cf652c](https://github.com/sireum/kekinian/releases/tag/4.20250211.0cf652c)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250211.0cf652c https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [ce35f8d](https://github.com/sireum/hamr-codegen/commit/ce35f8d) update submodule

* [be3729f](https://github.com/sireum/hamr-codegen/commit/be3729f) microkit: handle strings in c/rust

* [4a7198f](https://github.com/sireum/hamr-codegen/commit/4a7198f) update submodule

* [f78ecac](https://github.com/sireum/hamr-codegen/commit/f78ecac) update submodule

* [44d733f](https://github.com/sireum/hamr-codegen/commit/44d733f) add microkit tests to workflows

* [9b5532c](https://github.com/sireum/hamr-codegen/commit/9b5532c) update submodule

* [3904970](https://github.com/sireum/hamr-codegen/commit/3904970) update submodule

* [edce9ac](https://github.com/sireum/hamr-codegen/commit/edce9ac) add microkit rust support, update versions

* [0245c03](https://github.com/sireum/hamr-codegen/commit/0245c03) correct ros location

* [36ad9f4](https://github.com/sireum/hamr-codegen/commit/36ad9f4) install ros2 on linux

* [6c1d1b6](https://github.com/sireum/hamr-codegen/commit/6c1d1b6) update submodule
</details>
<br>


# [4.20250124.83153a7](https://github.com/sireum/kekinian/releases/tag/4.20250124.83153a7)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250124.83153a7 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [3a43023](https://github.com/sireum/hamr-codegen/commit/3a43023) Fixing syntax

* [404b230](https://github.com/sireum/hamr-codegen/commit/404b230) Created Runner and User files

* [f42c421](https://github.com/sireum/hamr-codegen/commit/f42c421) Created GeneratorPy
</details>
<br>


# [4.20250115.20107b2](https://github.com/sireum/kekinian/releases/tag/4.20250115.20107b2)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20250115.20107b2 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [70dccd5](https://github.com/sireum/hamr-codegen/commit/70dccd5) increase default compute exeution time

* [4cd27b7](https://github.com/sireum/hamr-codegen/commit/4cd27b7) Updated copyright years.

* [1563cde](https://github.com/sireum/hamr-codegen/commit/1563cde) tipe fixes

* [1d5f100](https://github.com/sireum/hamr-codegen/commit/1d5f100) microkit: handle unconnected ports

* [dd4efd0](https://github.com/sireum/hamr-codegen/commit/dd4efd0) update submodule

* [77d7e27](https://github.com/sireum/hamr-codegen/commit/77d7e27) update build props

* [a3d4fad](https://github.com/sireum/hamr-codegen/commit/a3d4fad) update submodule

* [2398299](https://github.com/sireum/hamr-codegen/commit/2398299) update codegen cli args

* [5f5a280](https://github.com/sireum/hamr-codegen/commit/5f5a280) use same cli group for camkes and microkit

* [573c63c](https://github.com/sireum/hamr-codegen/commit/573c63c) refactor process binding resolution

* [e2c01bf](https://github.com/sireum/hamr-codegen/commit/e2c01bf) vm tweak

* [868e405](https://github.com/sireum/hamr-codegen/commit/868e405) disallow unbound int and float port payloads

* [4d05930](https://github.com/sireum/hamr-codegen/commit/4d05930) disallow unbound int and float port payloads
</details>
<br>


# [4.20241212.3e630db](https://github.com/sireum/kekinian/releases/tag/4.20241212.3e630db)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241212.3e630db https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [899f874](https://github.com/sireum/hamr-codegen/commit/899f874) add initial microkit vm support
</details>
<br>


# [4.20241202.c94661b](https://github.com/sireum/kekinian/releases/tag/4.20241202.c94661b)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241202.c94661b https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [550bc8e](https://github.com/sireum/hamr-codegen/commit/550bc8e) use environment's sireum.jar if build.cmd is called via 'sireum slang run'

* [e402270](https://github.com/sireum/hamr-codegen/commit/e402270) fix microkit handling of array and struct payloads
</details>
<br>


# [4.20241122.a3b711d](https://github.com/sireum/kekinian/releases/tag/4.20241122.a3b711d)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241122.a3b711d https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [592762f](https://github.com/sireum/hamr-codegen/commit/592762f) emit report in build dir

* [0c9cca1](https://github.com/sireum/hamr-codegen/commit/0c9cca1) template tweak

* [6017b78](https://github.com/sireum/hamr-codegen/commit/6017b78) add stack size support

* [657c08e](https://github.com/sireum/hamr-codegen/commit/657c08e) microkit aadl type and port queue support

* [37806f4](https://github.com/sireum/hamr-codegen/commit/37806f4) use pos of symbol rather than the resolved type's pos

* [3d5899d](https://github.com/sireum/hamr-codegen/commit/3d5899d) improve error message

* [1b440b5](https://github.com/sireum/hamr-codegen/commit/1b440b5) remove debug

* [ff2be50](https://github.com/sireum/hamr-codegen/commit/ff2be50) update phantom versions

* [1050c39](https://github.com/sireum/hamr-codegen/commit/1050c39) debug cache

* [b7dd0ca](https://github.com/sireum/hamr-codegen/commit/b7dd0ca) debug cache

* [68eb338](https://github.com/sireum/hamr-codegen/commit/68eb338) update message

* [725b0d2](https://github.com/sireum/hamr-codegen/commit/725b0d2) update submdoule

* [9a31f70](https://github.com/sireum/hamr-codegen/commit/9a31f70) update submodule

* [bf5e855](https://github.com/sireum/hamr-codegen/commit/bf5e855) update build props

* [72b3592](https://github.com/sireum/hamr-codegen/commit/72b3592) cleanup

* [f40a99f](https://github.com/sireum/hamr-codegen/commit/f40a99f) don't write out resources when errors found

* [673dbb8](https://github.com/sireum/hamr-codegen/commit/673dbb8) convert halts to error reports
</details>
<br>


# [4.20241104.1c0dea6](https://github.com/sireum/kekinian/releases/tag/4.20241104.1c0dea6)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241104.1c0dea6 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [76b379b](https://github.com/sireum/hamr-codegen/commit/76b379b) update submodule

* [06f8e78](https://github.com/sireum/hamr-codegen/commit/06f8e78) update build prop

* [4e4dd06](https://github.com/sireum/hamr-codegen/commit/4e4dd06) update submodule

* [11cf193](https://github.com/sireum/hamr-codegen/commit/11cf193) update build props

* [c63a932](https://github.com/sireum/hamr-codegen/commit/c63a932) update build props

* [c4e0a00](https://github.com/sireum/hamr-codegen/commit/c4e0a00) update submodule

* [3f1edad](https://github.com/sireum/hamr-codegen/commit/3f1edad) change default jvm package name to 'base'

* [f34ff45](https://github.com/sireum/hamr-codegen/commit/f34ff45) correct package names
</details>
<br>


# [4.20241028.60464f2](https://github.com/sireum/kekinian/releases/tag/4.20241028.60464f2)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241028.60464f2 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [7db2280](https://github.com/sireum/hamr-codegen/commit/7db2280) move prop files back

* [62de0d4](https://github.com/sireum/hamr-codegen/commit/62de0d4) update submodule

* [bf25e29](https://github.com/sireum/hamr-codegen/commit/bf25e29) correct location of prop files

* [7001396](https://github.com/sireum/hamr-codegen/commit/7001396) update versions

* [9af22c5](https://github.com/sireum/hamr-codegen/commit/9af22c5) add output-dir option, update submodule
</details>
<br>


# [4.20241024.6da7a54](https://github.com/sireum/kekinian/releases/tag/4.20241024.6da7a54)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241024.6da7a54 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [455f6ff](https://github.com/sireum/hamr-codegen/commit/455f6ff) tipe fix

* [f5991d0](https://github.com/sireum/hamr-codegen/commit/f5991d0) checkpoint for cli utils

* [001b924](https://github.com/sireum/hamr-codegen/commit/001b924) update submodule

* [65fd36f](https://github.com/sireum/hamr-codegen/commit/65fd36f) ensure sel4 app name is unique

* [c90de33](https://github.com/sireum/hamr-codegen/commit/c90de33) remove camkes z3 version mod

* [9d13729](https://github.com/sireum/hamr-codegen/commit/9d13729) Updated CI.

* [61c5030](https://github.com/sireum/hamr-codegen/commit/61c5030) update submodule

* [83e4230](https://github.com/sireum/hamr-codegen/commit/83e4230) include the bridge and component directories when building the camkes slang type libraries as datatypes may refer to gumbo functions
</details>
<br>


# [4.20241017.f6e1eff](https://github.com/sireum/kekinian/releases/tag/4.20241017.f6e1eff)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241017.f6e1eff https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [1e95192](https://github.com/sireum/hamr-codegen/commit/1e95192) update submodule

* [9eaebfe](https://github.com/sireum/hamr-codegen/commit/9eaebfe) cleanup

* [28f45f0](https://github.com/sireum/hamr-codegen/commit/28f45f0) only observe the dispatched event port for sporadic components
</details>
<br>


# [4.20241016.60c1a61](https://github.com/sireum/kekinian/releases/tag/4.20241016.60c1a61)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241016.60c1a61 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [2a1ed62](https://github.com/sireum/hamr-codegen/commit/2a1ed62) tipe fix

* [4b09663](https://github.com/sireum/hamr-codegen/commit/4b09663) update submodule

* [25fc740](https://github.com/sireum/hamr-codegen/commit/25fc740) don't set default path

* [433b552](https://github.com/sireum/hamr-codegen/commit/433b552) add ros2 changes from test submodule

* [c2bfdb7](https://github.com/sireum/hamr-codegen/commit/c2bfdb7) Added datatype handling (#2)
</details>
<br>


# [4.20241011.21df31a](https://github.com/sireum/kekinian/releases/tag/4.20241011.21df31a)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241011.21df31a https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [b4d2a1e](https://github.com/sireum/hamr-codegen/commit/b4d2a1e) update readme

* [1dd8e21](https://github.com/sireum/hamr-codegen/commit/1dd8e21) remove unneeded imports

* [560c9e2](https://github.com/sireum/hamr-codegen/commit/560c9e2) resolve gumbo methods via fully qualified name

* [c622240](https://github.com/sireum/hamr-codegen/commit/c622240) handle case where model has lib annexes but first processed component does not have initialize contracts

* [1c8d618](https://github.com/sireum/hamr-codegen/commit/1c8d618) correct isInObject params, return updated type/name maps in the slang type hierarchy
</details>
<br>


# [4.20241003.c5bebe8](https://github.com/sireum/kekinian/releases/tag/4.20241003.c5bebe8)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20241003.c5bebe8 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [41b5eaf](https://github.com/sireum/hamr-codegen/commit/41b5eaf) update scripts

* [51a1868](https://github.com/sireum/hamr-codegen/commit/51a1868) moved arsit and common resources to codegen

* [f415f43](https://github.com/sireum/hamr-codegen/commit/f415f43) moved act resources to codegen

* [9a295e4](https://github.com/sireum/hamr-codegen/commit/9a295e4) update stable camkes workflow to use current latest camkes image

* [587ce3d](https://github.com/sireum/hamr-codegen/commit/587ce3d) Streamlined access to Slang runtime library type hierarchy.

* [2e1b383](https://github.com/sireum/hamr-codegen/commit/2e1b383) reject runtime monitoring when model doesn't contain threads

* [d3158fd](https://github.com/sireum/hamr-codegen/commit/d3158fd) update submodules

* [4bf4ab2](https://github.com/sireum/hamr-codegen/commit/4bf4ab2) update build props

* [3845d22](https://github.com/sireum/hamr-codegen/commit/3845d22) update coursier call

* [0fa4c35](https://github.com/sireum/hamr-codegen/commit/0fa4c35) add ability to set proyek ive options from osate

* [baaf4db](https://github.com/sireum/hamr-codegen/commit/baaf4db) dot updates

* [b65d5ea](https://github.com/sireum/hamr-codegen/commit/b65d5ea) update submodules

* [64fd117](https://github.com/sireum/hamr-codegen/commit/64fd117) create data struct for xml system description, emit dot arch diagram

* [3df089e](https://github.com/sireum/hamr-codegen/commit/3df089e) Adapted to Slang changes.
</details>
<br>


# [4.20240927.ff60856](https://github.com/sireum/kekinian/releases/tag/4.20240927.ff60856)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240927.ff60856 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [d03697b](https://github.com/sireum/hamr-codegen/commit/d03697b) update submodules

* [b2205bb](https://github.com/sireum/hamr-codegen/commit/b2205bb) add datatype support

* [0493680](https://github.com/sireum/hamr-codegen/commit/0493680) add mk type support, revert changes to class name

* [6e84b0a](https://github.com/sireum/hamr-codegen/commit/6e84b0a) Updated init sireum.jar location.
</details>
<br>


# [4.20240924.9739da6](https://github.com/sireum/kekinian/releases/tag/4.20240924.9739da6)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240924.9739da6 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [f365f31](https://github.com/sireum/hamr-codegen/commit/f365f31) use compute execution time to gen schedule

* [06d070a](https://github.com/sireum/hamr-codegen/commit/06d070a) remove TestOs

* [3b70988](https://github.com/sireum/hamr-codegen/commit/3b70988) Refactored Os.Proc API.

* [0963d54](https://github.com/sireum/hamr-codegen/commit/0963d54) update submodules

* [93e34e1](https://github.com/sireum/hamr-codegen/commit/93e34e1) update build props

* [86361ec](https://github.com/sireum/hamr-codegen/commit/86361ec) adapt to option changes, microkit support

* [7f70132](https://github.com/sireum/hamr-codegen/commit/7f70132) update submodules

* [3e52664](https://github.com/sireum/hamr-codegen/commit/3e52664) move hamr cli artifacts to common
</details>
<br>


# [4.20240916.58541b9](https://github.com/sireum/kekinian/releases/tag/4.20240916.58541b9)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240916.58541b9 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [01072ed](https://github.com/sireum/hamr-codegen/commit/01072ed) update submodules

* [624a4a2](https://github.com/sireum/hamr-codegen/commit/624a4a2) update build props

* [83e8f59](https://github.com/sireum/hamr-codegen/commit/83e8f59) add new changes from clint branch

* [5a5e5d3](https://github.com/sireum/hamr-codegen/commit/5a5e5d3) add ros2 updates from clint branch, tipe fixes
</details>
<br>


# [4.20240913.333bacf](https://github.com/sireum/kekinian/releases/tag/4.20240913.333bacf)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240913.333bacf https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [0e73b3b](https://github.com/sireum/hamr-codegen/commit/0e73b3b) Regen expected.

* [3aaaec0](https://github.com/sireum/hamr-codegen/commit/3aaaec0) Addressed whitespace issues in SIREUM_HOME path.

* [93f3db0](https://github.com/sireum/hamr-codegen/commit/93f3db0) update submodules

* [ca26cf4](https://github.com/sireum/hamr-codegen/commit/ca26cf4) Addressed whitespace issues in SIREUM_HOME path.

* [3ad602e](https://github.com/sireum/hamr-codegen/commit/3ad602e) Updated submodules.

* [91eec81](https://github.com/sireum/hamr-codegen/commit/91eec81) Addressed whitespace issue in SIREUM_HOME path.

* [ddec69b](https://github.com/sireum/hamr-codegen/commit/ddec69b) remove sysml dep

* [84c8935](https://github.com/sireum/hamr-codegen/commit/84c8935) update submodules

* [aec99c6](https://github.com/sireum/hamr-codegen/commit/aec99c6) change hamr option names

* [f0fa5c7](https://github.com/sireum/hamr-codegen/commit/f0fa5c7) update submodules

* [44fbec5](https://github.com/sireum/hamr-codegen/commit/44fbec5) update build props

* [f4cdc51](https://github.com/sireum/hamr-codegen/commit/f4cdc51) update submodules

* [8ff0636](https://github.com/sireum/hamr-codegen/commit/8ff0636) update submodules

* [1228e74](https://github.com/sireum/hamr-codegen/commit/1228e74) add system position info

* [f42ada4](https://github.com/sireum/hamr-codegen/commit/f42ada4) update submodules

* [e72c4bf](https://github.com/sireum/hamr-codegen/commit/e72c4bf) ensure valid package name

* [881f171](https://github.com/sireum/hamr-codegen/commit/881f171) factor out common codegen options

* [0b93689](https://github.com/sireum/hamr-codegen/commit/0b93689) report error

* [0a35f87](https://github.com/sireum/hamr-codegen/commit/0a35f87) also return connection ref id path

* [81c7e4b](https://github.com/sireum/hamr-codegen/commit/81c7e4b) helper functions

* [ba7b175](https://github.com/sireum/hamr-codegen/commit/ba7b175) fix enum names

* [8d16790](https://github.com/sireum/hamr-codegen/commit/8d16790) treat aadl threads as singletons when type checking

* [03cc902](https://github.com/sireum/hamr-codegen/commit/03cc902) map resolved integration constraint exp to itself

* [3d25554](https://github.com/sireum/hamr-codegen/commit/3d25554) add slang type hierarchy to gcl sym results

* [07ef203](https://github.com/sireum/hamr-codegen/commit/07ef203) update submodules

* [b96a99b](https://github.com/sireum/hamr-codegen/commit/b96a99b) add sysml type check support

* [a699bc2](https://github.com/sireum/hamr-codegen/commit/a699bc2) Added --parseable-messages cli option.

* [f008745](https://github.com/sireum/hamr-codegen/commit/f008745) update submodules

* [917714b](https://github.com/sireum/hamr-codegen/commit/917714b) add sysmlv2 options

* [7c62d28](https://github.com/sireum/hamr-codegen/commit/7c62d28) update submodules

* [fadc745](https://github.com/sireum/hamr-codegen/commit/fadc745) version updates

* [013a9b3](https://github.com/sireum/hamr-codegen/commit/013a9b3) adapt to gumbo changes
</details>
<br>


# [4.20240811.6c1a63d](https://github.com/sireum/kekinian/releases/tag/4.20240811.6c1a63d)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240811.6c1a63d https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [58e7b40](https://github.com/sireum/hamr-codegen/commit/58e7b40) update submodules

* [421fbc8](https://github.com/sireum/hamr-codegen/commit/421fbc8) schedule ci tests for 2am sundays
</details>
<br>


# [4.20240717.e27fd06](https://github.com/sireum/kekinian/releases/tag/4.20240717.e27fd06)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240717.e27fd06 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [5430568](https://github.com/sireum/hamr-codegen/commit/5430568) add ros2 tests

* [41e147c](https://github.com/sireum/hamr-codegen/commit/41e147c) update submodule

* [c656145](https://github.com/sireum/hamr-codegen/commit/c656145) add ros2
</details>
<br>


# [4.20240613.a209452](https://github.com/sireum/kekinian/releases/tag/4.20240613.a209452)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240613.a209452 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [4d40eb3](https://github.com/sireum/hamr-codegen/commit/4d40eb3) update submodules

* [27bc757](https://github.com/sireum/hamr-codegen/commit/27bc757) update build props
</details>
<br>


# [4.20240508.f1c262c](https://github.com/sireum/kekinian/releases/tag/4.20240508.f1c262c)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240508.f1c262c https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [51022f9](https://github.com/sireum/hamr-codegen/commit/51022f9) update submodules

* [dc84ae9](https://github.com/sireum/hamr-codegen/commit/dc84ae9) re-enable vm tests

* [c6e3e85](https://github.com/sireum/hamr-codegen/commit/c6e3e85) update submodules

* [d90d922](https://github.com/sireum/hamr-codegen/commit/d90d922) update submodules

* [fcdaa24](https://github.com/sireum/hamr-codegen/commit/fcdaa24) update submodules

* [59a6ef6](https://github.com/sireum/hamr-codegen/commit/59a6ef6) add gumbox support for handler clauses -- still need to do the testing infrastructure

* [016699e](https://github.com/sireum/hamr-codegen/commit/016699e) update submodules

* [49f06aa](https://github.com/sireum/hamr-codegen/commit/49f06aa) update phantom versions

* [6090a19](https://github.com/sireum/hamr-codegen/commit/6090a19) update submodules

* [d48fbf7](https://github.com/sireum/hamr-codegen/commit/d48fbf7) update build props

* [699befe](https://github.com/sireum/hamr-codegen/commit/699befe) update submodules

* [5537268](https://github.com/sireum/hamr-codegen/commit/5537268) update build props
</details>
<br>


# [4.20240409.bb9a698](https://github.com/sireum/kekinian/releases/tag/4.20240409.bb9a698)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240409.bb9a698 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [63c2e7c](https://github.com/sireum/hamr-codegen/commit/63c2e7c) Removed mill configs.

* [1c67e59](https://github.com/sireum/hamr-codegen/commit/1c67e59) update submodules

* [97b3b9c](https://github.com/sireum/hamr-codegen/commit/97b3b9c) @ induct support.

* [5e2e7bc](https://github.com/sireum/hamr-codegen/commit/5e2e7bc) @ induct support.

* [eb589f0](https://github.com/sireum/hamr-codegen/commit/eb589f0) update submodules

* [c650499](https://github.com/sireum/hamr-codegen/commit/c650499) update submodules

* [a5265eb](https://github.com/sireum/hamr-codegen/commit/a5265eb) update build props

* [d2c7624](https://github.com/sireum/hamr-codegen/commit/d2c7624) disable assertion in order to build bootstrap

* [3a18dc2](https://github.com/sireum/hamr-codegen/commit/3a18dc2) update submodules

* [57de8ef](https://github.com/sireum/hamr-codegen/commit/57de8ef) update build props

* [9950033](https://github.com/sireum/hamr-codegen/commit/9950033) update submodules

* [4cd019a](https://github.com/sireum/hamr-codegen/commit/4cd019a) update build props

* [aee9858](https://github.com/sireum/hamr-codegen/commit/aee9858) update submdoules

* [5788f07](https://github.com/sireum/hamr-codegen/commit/5788f07) update build props

* [70edf38](https://github.com/sireum/hamr-codegen/commit/70edf38) update build props

* [85178cc](https://github.com/sireum/hamr-codegen/commit/85178cc) update workflows

* [391ab01](https://github.com/sireum/hamr-codegen/commit/391ab01) update submodules

* [e434216](https://github.com/sireum/hamr-codegen/commit/e434216) switch to fullPosOpt
</details>
<br>


# [4.20240229.03709b7](https://github.com/sireum/kekinian/releases/tag/4.20240229.03709b7)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240229.03709b7 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e9116d3](https://github.com/sireum/hamr-codegen/commit/e9116d3) Qualified scala.List.

* [c81b8aa](https://github.com/sireum/hamr-codegen/commit/c81b8aa) update submodules

* [65b9a35](https://github.com/sireum/hamr-codegen/commit/65b9a35) update phantom versions

* [9d5eba5](https://github.com/sireum/hamr-codegen/commit/9d5eba5) Adapted to Slang changes.

* [928a710](https://github.com/sireum/hamr-codegen/commit/928a710) update submodules

* [e79df7d](https://github.com/sireum/hamr-codegen/commit/e79df7d) update phantom props

* [abcaa69](https://github.com/sireum/hamr-codegen/commit/abcaa69) Rewriting system.
</details>
<br>


# [4.20240219.9701d83](https://github.com/sireum/kekinian/releases/tag/4.20240219.9701d83)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240219.9701d83 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [2b85e1e](https://github.com/sireum/hamr-codegen/commit/2b85e1e) update submodules

* [c7de604](https://github.com/sireum/hamr-codegen/commit/c7de604) t
</details>
<br>


# [4.20240219.acd4b33](https://github.com/sireum/kekinian/releases/tag/4.20240219.acd4b33)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240219.acd4b33 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e6a884d](https://github.com/sireum/hamr-codegen/commit/e6a884d) update submdoules

* [e25f4f7](https://github.com/sireum/hamr-codegen/commit/e25f4f7) add verbose gh action option

* [c812b1d](https://github.com/sireum/hamr-codegen/commit/c812b1d) update submodules

* [141e7b3](https://github.com/sireum/hamr-codegen/commit/141e7b3) Update CI-camkes.yml

* [f968906](https://github.com/sireum/hamr-codegen/commit/f968906) debug logging

* [69c6b48](https://github.com/sireum/hamr-codegen/commit/69c6b48) disable camkes vm tests

* [4b9ae82](https://github.com/sireum/hamr-codegen/commit/4b9ae82) Fixed cli opt.

* [d01f4c4](https://github.com/sireum/hamr-codegen/commit/d01f4c4) Added rewriting set definition and definitional/abstract strictpure method support.
</details>
<br>


# [4.20240126.221b644](https://github.com/sireum/kekinian/releases/tag/4.20240126.221b644)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20240126.221b644 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [549b14d](https://github.com/sireum/hamr-codegen/commit/549b14d) update submodules

* [bc87b6a](https://github.com/sireum/hamr-codegen/commit/bc87b6a) update build props

* [74b7763](https://github.com/sireum/hamr-codegen/commit/74b7763) update submodules

* [5fbd7e5](https://github.com/sireum/hamr-codegen/commit/5fbd7e5) update build props

* [fafb85c](https://github.com/sireum/hamr-codegen/commit/fafb85c) update submodules

* [7becff1](https://github.com/sireum/hamr-codegen/commit/7becff1) update build props

* [9be6b3a](https://github.com/sireum/hamr-codegen/commit/9be6b3a) Updated copyright years.
</details>
<br>


# [4.20231221.a9e7c51](https://github.com/sireum/kekinian/releases/tag/4.20231221.a9e7c51)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231221.a9e7c51 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [bb79f5f](https://github.com/sireum/hamr-codegen/commit/bb79f5f) update submodules

* [0c4b669](https://github.com/sireum/hamr-codegen/commit/0c4b669) use posOpt to uniquely id expressions
</details>
<br>


# [4.20231212.3ca2940](https://github.com/sireum/kekinian/releases/tag/4.20231212.3ca2940)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231212.3ca2940 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [fe3cd83](https://github.com/sireum/hamr-codegen/commit/fe3cd83) update submodules

* [dcb10bd](https://github.com/sireum/hamr-codegen/commit/dcb10bd) update submodules

* [af448fa](https://github.com/sireum/hamr-codegen/commit/af448fa) update build props

* [cc9d693](https://github.com/sireum/hamr-codegen/commit/cc9d693) add version checks for fmide cli

* [3f11ca0](https://github.com/sireum/hamr-codegen/commit/3f11ca0) update submodules

* [6255679](https://github.com/sireum/hamr-codegen/commit/6255679) update submodules

* [4149d06](https://github.com/sireum/hamr-codegen/commit/4149d06) update build props

* [77cb57f](https://github.com/sireum/hamr-codegen/commit/77cb57f) path fix

* [85048ac](https://github.com/sireum/hamr-codegen/commit/85048ac) update submodules

* [46a8cf3](https://github.com/sireum/hamr-codegen/commit/46a8cf3) update submodules

* [58bfa40](https://github.com/sireum/hamr-codegen/commit/58bfa40) update build props

* [3e9e1bb](https://github.com/sireum/hamr-codegen/commit/3e9e1bb) update submodules

* [b116389](https://github.com/sireum/hamr-codegen/commit/b116389) update build props

* [a1a4cda](https://github.com/sireum/hamr-codegen/commit/a1a4cda) update submodules

* [05169ba](https://github.com/sireum/hamr-codegen/commit/05169ba) update submmodule

* [fbeb4a3](https://github.com/sireum/hamr-codegen/commit/fbeb4a3) disallow transpiling + runtime monitoring for now

* [73a90e8](https://github.com/sireum/hamr-codegen/commit/73a90e8) update submodules

* [75f3817](https://github.com/sireum/hamr-codegen/commit/75f3817) update build props
</details>
<br>


# [4.20231109.1a23e8e](https://github.com/sireum/kekinian/releases/tag/4.20231109.1a23e8e)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231109.1a23e8e https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8d1b99f](https://github.com/sireum/hamr-codegen/commit/8d1b99f) update build props

* [9eb0037](https://github.com/sireum/hamr-codegen/commit/9eb0037) add codegen runtime monitoring option

* [7b5c779](https://github.com/sireum/hamr-codegen/commit/7b5c779) add runtime monitoring option

* [8210d84](https://github.com/sireum/hamr-codegen/commit/8210d84) update build props

* [0d0eab9](https://github.com/sireum/hamr-codegen/commit/0d0eab9) update .gitignore
</details>
<br>


# [4.20231025.3258446](https://github.com/sireum/kekinian/releases/tag/4.20231025.3258446)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231025.3258446 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [d526b3c](https://github.com/sireum/hamr-codegen/commit/d526b3c) Updated project-standalone.cmd.

* [7befd64](https://github.com/sireum/hamr-codegen/commit/7befd64) Updated build.cmd.

* [d079872](https://github.com/sireum/hamr-codegen/commit/d079872) Updated build.cmd.

* [a6a2530](https://github.com/sireum/hamr-codegen/commit/a6a2530) Updated project-standalone.cmd.

* [16f0b13](https://github.com/sireum/hamr-codegen/commit/16f0b13) Added hamr-sysml-parser as a dependency of hamr-common.

* [71ede46](https://github.com/sireum/hamr-codegen/commit/71ede46) update art

* [543092b](https://github.com/sireum/hamr-codegen/commit/543092b) update build props

* [2eb14c5](https://github.com/sireum/hamr-codegen/commit/2eb14c5) update submodules

* [70d34c7](https://github.com/sireum/hamr-codegen/commit/70d34c7) update submodules

* [a52043d](https://github.com/sireum/hamr-codegen/commit/a52043d) update submodules
</details>
<br>


# [4.20231019.967e9e3](https://github.com/sireum/kekinian/releases/tag/4.20231019.967e9e3)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231019.967e9e3 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [d796953](https://github.com/sireum/hamr-codegen/commit/d796953) update submodule
</details>
<br>


# [4.20231012.254e926](https://github.com/sireum/kekinian/releases/tag/4.20231012.254e926)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20231012.254e926 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [bfd8cfd](https://github.com/sireum/hamr-codegen/commit/bfd8cfd) update submodules

* [5c7a54b](https://github.com/sireum/hamr-codegen/commit/5c7a54b) update build props

* [ac7f2f9](https://github.com/sireum/hamr-codegen/commit/ac7f2f9) Added inline and tailrec annotation support.

* [d86bd8d](https://github.com/sireum/hamr-codegen/commit/d86bd8d) update submodules

* [40a61df](https://github.com/sireum/hamr-codegen/commit/40a61df) update submodules

* [84cc286](https://github.com/sireum/hamr-codegen/commit/84cc286) update build props

* [3e14b97](https://github.com/sireum/hamr-codegen/commit/3e14b97) add isDatatype field

* [b59778f](https://github.com/sireum/hamr-codegen/commit/b59778f) update submodules

* [e3b615d](https://github.com/sireum/hamr-codegen/commit/e3b615d) rename to project-standalone.cmd, remove external sirfur tests

* [a2db3e2](https://github.com/sireum/hamr-codegen/commit/a2db3e2) use idpath as key

* [d021704](https://github.com/sireum/hamr-codegen/commit/d021704) update submodule

* [5b679a9](https://github.com/sireum/hamr-codegen/commit/5b679a9) update submodule

* [d1b1687](https://github.com/sireum/hamr-codegen/commit/d1b1687) update submodules
</details>
<br>


# [4.20230925.abeaec6](https://github.com/sireum/kekinian/releases/tag/4.20230925.abeaec6)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230925.abeaec6 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [35d4f78](https://github.com/sireum/hamr-codegen/commit/35d4f78) update arsit

* [76439cd](https://github.com/sireum/hamr-codegen/commit/76439cd) update arsit plugin

* [348bbe6](https://github.com/sireum/hamr-codegen/commit/348bbe6) update submodules
</details>
<br>


# [4.20230922.4ff94f3](https://github.com/sireum/kekinian/releases/tag/4.20230922.4ff94f3)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230922.4ff94f3 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [c457ea8](https://github.com/sireum/hamr-codegen/commit/c457ea8) add install sbt mill cli option

* [da33a59](https://github.com/sireum/hamr-codegen/commit/da33a59) udpate submodules

* [8257c73](https://github.com/sireum/hamr-codegen/commit/8257c73) update build props
</details>
<br>


# [4.20230921.331f54a](https://github.com/sireum/kekinian/releases/tag/4.20230921.331f54a)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230921.331f54a https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [9a77e24](https://github.com/sireum/hamr-codegen/commit/9a77e24) Adapted to Slang changes.

* [cb96cf5](https://github.com/sireum/hamr-codegen/commit/cb96cf5) update submodules

* [eed4743](https://github.com/sireum/hamr-codegen/commit/eed4743) update arsit

* [6348295](https://github.com/sireum/hamr-codegen/commit/6348295) Adapted to Slang changes.

* [c9331a7](https://github.com/sireum/hamr-codegen/commit/c9331a7) update arsit

* [03056a1](https://github.com/sireum/hamr-codegen/commit/03056a1) Added Old(...) spec-only expression.

* [69bf679](https://github.com/sireum/hamr-codegen/commit/69bf679) update submodules

* [1e09e28](https://github.com/sireum/hamr-codegen/commit/1e09e28) add create marker util

* [21effa7](https://github.com/sireum/hamr-codegen/commit/21effa7) update arsit

* [a8682fe](https://github.com/sireum/hamr-codegen/commit/a8682fe) st hack

* [a4a7101](https://github.com/sireum/hamr-codegen/commit/a4a7101) update submodules

* [b14bbd6](https://github.com/sireum/hamr-codegen/commit/b14bbd6) update props

* [0a24616](https://github.com/sireum/hamr-codegen/commit/0a24616) update art

* [44d7f04](https://github.com/sireum/hamr-codegen/commit/44d7f04) update submodules

* [4faa308](https://github.com/sireum/hamr-codegen/commit/4faa308) add comment template

* [c4e653f](https://github.com/sireum/hamr-codegen/commit/c4e653f) update submodules

* [6710e91](https://github.com/sireum/hamr-codegen/commit/6710e91) rewrite port name to 'api' in modify clauses
</details>
<br>


# [4.20230815.c628ec4](https://github.com/sireum/kekinian/releases/tag/4.20230815.c628ec4)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230815.c628ec4 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [22e9f20](https://github.com/sireum/hamr-codegen/commit/22e9f20) update submodules

* [1164c12](https://github.com/sireum/hamr-codegen/commit/1164c12) update submodules

* [c6b3ff9](https://github.com/sireum/hamr-codegen/commit/c6b3ff9) modify gumbo readmes

* [19894ba](https://github.com/sireum/hamr-codegen/commit/19894ba) remove memoize

* [ada84a7](https://github.com/sireum/hamr-codegen/commit/ada84a7) update submodules

* [7710b69](https://github.com/sireum/hamr-codegen/commit/7710b69) update submodules

* [ebe67b4](https://github.com/sireum/hamr-codegen/commit/ebe67b4) update submodule

* [3552f3e](https://github.com/sireum/hamr-codegen/commit/3552f3e) make incoming event port spec vars optional

* [20c862d](https://github.com/sireum/hamr-codegen/commit/20c862d) add hasEvent

* [7467416](https://github.com/sireum/hamr-codegen/commit/7467416) update submodules

* [b652aac](https://github.com/sireum/hamr-codegen/commit/b652aac) prevent gumbox on models with array types until it fully supports by slangcheck

* [623f887](https://github.com/sireum/hamr-codegen/commit/623f887) remove halt for unhandled types

* [21b337c](https://github.com/sireum/hamr-codegen/commit/21b337c) better reporting

* [eabc8b2](https://github.com/sireum/hamr-codegen/commit/eabc8b2) remove remaining references to old slangcheck

* [efb9a75](https://github.com/sireum/hamr-codegen/commit/efb9a75) update submodules

* [b683aa4](https://github.com/sireum/hamr-codegen/commit/b683aa4) enable sergen and slangcheck in workflows

* [1d273c0](https://github.com/sireum/hamr-codegen/commit/1d273c0) update submodules

* [b72124d](https://github.com/sireum/hamr-codegen/commit/b72124d) update submodules

* [be232b4](https://github.com/sireum/hamr-codegen/commit/be232b4) checkpoint

* [7743fcd](https://github.com/sireum/hamr-codegen/commit/7743fcd) update submodules

* [80eff36](https://github.com/sireum/hamr-codegen/commit/80eff36) slangcheck integration

* [7fe3c74](https://github.com/sireum/hamr-codegen/commit/7fe3c74) update submodule

* [84fd07a](https://github.com/sireum/hamr-codegen/commit/84fd07a) add configuration plugin

* [ca9a298](https://github.com/sireum/hamr-codegen/commit/ca9a298) update arsit

* [cb71aa8](https://github.com/sireum/hamr-codegen/commit/cb71aa8) allow for extra port,component,connection ids

* [ff15a78](https://github.com/sireum/hamr-codegen/commit/ff15a78) update submodule

* [d3a798d](https://github.com/sireum/hamr-codegen/commit/d3a798d) update submodules

* [719e828](https://github.com/sireum/hamr-codegen/commit/719e828) update submodules

* [0d8392a](https://github.com/sireum/hamr-codegen/commit/0d8392a) update build props

* [efc2087](https://github.com/sireum/hamr-codegen/commit/efc2087) fix crlf issue when weaving contract code
</details>
<br>


# [4.20230523.a5432f0](https://github.com/sireum/kekinian/releases/tag/4.20230523.a5432f0)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230523.a5432f0 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [5ac03f8](https://github.com/sireum/hamr-codegen/commit/5ac03f8) update submodule

* [f11ebb8](https://github.com/sireum/hamr-codegen/commit/f11ebb8) slang check integration

* [3de4db7](https://github.com/sireum/hamr-codegen/commit/3de4db7) update submodule

* [ff351eb](https://github.com/sireum/hamr-codegen/commit/ff351eb) update submodules

* [b63b092](https://github.com/sireum/hamr-codegen/commit/b63b092) win workflow fix

* [f7d4165](https://github.com/sireum/hamr-codegen/commit/f7d4165) update workflows

* [aeebb39](https://github.com/sireum/hamr-codegen/commit/aeebb39) update submodules

* [e38fecf](https://github.com/sireum/hamr-codegen/commit/e38fecf) update build

* [5f3bf88](https://github.com/sireum/hamr-codegen/commit/5f3bf88) remove this

* [ffc3767](https://github.com/sireum/hamr-codegen/commit/ffc3767) use set

* [03e3e7d](https://github.com/sireum/hamr-codegen/commit/03e3e7d) update submodules

* [823791c](https://github.com/sireum/hamr-codegen/commit/823791c) disable slangcheck when not embeddeding art

* [32c0706](https://github.com/sireum/hamr-codegen/commit/32c0706) update submoule

* [e8bb338](https://github.com/sireum/hamr-codegen/commit/e8bb338) remove debug

* [822cda7](https://github.com/sireum/hamr-codegen/commit/822cda7) debug

* [536bd29](https://github.com/sireum/hamr-codegen/commit/536bd29) update submodule

* [4a75913](https://github.com/sireum/hamr-codegen/commit/4a75913) slang-check

* [809dcfe](https://github.com/sireum/hamr-codegen/commit/809dcfe) verbose

* [1facc3a](https://github.com/sireum/hamr-codegen/commit/1facc3a) ci

* [2af96ef](https://github.com/sireum/hamr-codegen/commit/2af96ef) echo

* [7a0e175](https://github.com/sireum/hamr-codegen/commit/7a0e175) fix check

* [7e2c1f7](https://github.com/sireum/hamr-codegen/commit/7e2c1f7) echo slangcheck

* [deb1ec7](https://github.com/sireum/hamr-codegen/commit/deb1ec7) update submodules

* [84b9ae4](https://github.com/sireum/hamr-codegen/commit/84b9ae4) update build props, dont' run slang check if model contains array types

* [83ce955](https://github.com/sireum/hamr-codegen/commit/83ce955) update submodules

* [26b5a27](https://github.com/sireum/hamr-codegen/commit/26b5a27) update submodules

* [811fd92](https://github.com/sireum/hamr-codegen/commit/811fd92) refine slang check call

* [9183ccf](https://github.com/sireum/hamr-codegen/commit/9183ccf) hack to call sergen
</details>
<br>


# [4.20230504.2861e45](https://github.com/sireum/kekinian/releases/tag/4.20230504.2861e45)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230504.2861e45 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [3f56c7a](https://github.com/sireum/hamr-codegen/commit/3f56c7a) update submodule

* [bc258fb](https://github.com/sireum/hamr-codegen/commit/bc258fb) add names

* [0099bd4](https://github.com/sireum/hamr-codegen/commit/0099bd4) update submodules

* [f882e22](https://github.com/sireum/hamr-codegen/commit/f882e22) update submodules

* [896e7c4](https://github.com/sireum/hamr-codegen/commit/896e7c4) update arsit

* [9c21483](https://github.com/sireum/hamr-codegen/commit/9c21483) update submodules

* [60023bf](https://github.com/sireum/hamr-codegen/commit/60023bf) remove slang check option

* [74c7f0c](https://github.com/sireum/hamr-codegen/commit/74c7f0c) add isDatatype field

* [1d4c408](https://github.com/sireum/hamr-codegen/commit/1d4c408) update arsit

* [b6edff4](https://github.com/sireum/hamr-codegen/commit/b6edff4) integrate slang check during build

* [63f8fe7](https://github.com/sireum/hamr-codegen/commit/63f8fe7) update arsit

* [fd2844f](https://github.com/sireum/hamr-codegen/commit/fd2844f) update arsit

* [1e0d082](https://github.com/sireum/hamr-codegen/commit/1e0d082) check if slang check jar exists

* [954c78a](https://github.com/sireum/hamr-codegen/commit/954c78a) update arsit

* [dcdc2c6](https://github.com/sireum/hamr-codegen/commit/dcdc2c6) disallow assume statements from referring to outgoing ports

* [43b31bb](https://github.com/sireum/hamr-codegen/commit/43b31bb) update arsit

* [8ffa1e5](https://github.com/sireum/hamr-codegen/commit/8ffa1e5) slangcheck integration

* [1fea84d](https://github.com/sireum/hamr-codegen/commit/1fea84d) update arsit

* [d65dc9c](https://github.com/sireum/hamr-codegen/commit/d65dc9c) update submodules

* [6af7045](https://github.com/sireum/hamr-codegen/commit/6af7045) add slang base type to aadl base type

* [e5dad5b](https://github.com/sireum/hamr-codegen/commit/e5dad5b) update arsit

* [e0ae07e](https://github.com/sireum/hamr-codegen/commit/e0ae07e) add seq name versions

* [da5dc70](https://github.com/sireum/hamr-codegen/commit/da5dc70) add sequence name methods

* [dae5213](https://github.com/sireum/hamr-codegen/commit/dae5213) update submodules
</details>
<br>


# [4.20230417.061c405](https://github.com/sireum/kekinian/releases/tag/4.20230417.061c405)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230417.061c405 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8d77a54](https://github.com/sireum/hamr-codegen/commit/8d77a54) update submodules

* [513e39e](https://github.com/sireum/hamr-codegen/commit/513e39e) switch to mutable plugins

* [fdc27f3](https://github.com/sireum/hamr-codegen/commit/fdc27f3) Added a check on strictpure methods/blocks to ensure they do not invoke non-strictpure methods.

* [51a3c2e](https://github.com/sireum/hamr-codegen/commit/51a3c2e) Added spec-only strictly pure expression block.

* [643b7c2](https://github.com/sireum/hamr-codegen/commit/643b7c2) update submodules

* [d3a97d4](https://github.com/sireum/hamr-codegen/commit/d3a97d4) update arsit

* [a642a15](https://github.com/sireum/hamr-codegen/commit/a642a15) add helper method

* [046ac4c](https://github.com/sireum/hamr-codegen/commit/046ac4c) update submodules

* [45fe4db](https://github.com/sireum/hamr-codegen/commit/45fe4db) update build props

* [b556aef](https://github.com/sireum/hamr-codegen/commit/b556aef) update submodules

* [cd687c4](https://github.com/sireum/hamr-codegen/commit/cd687c4) update build props

* [9995f00](https://github.com/sireum/hamr-codegen/commit/9995f00) fetch only when updating

* [d870f6c](https://github.com/sireum/hamr-codegen/commit/d870f6c) remove halt
</details>
<br>


# [4.20230330.51aa33e](https://github.com/sireum/kekinian/releases/tag/4.20230330.51aa33e)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230330.51aa33e https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [110166f](https://github.com/sireum/hamr-codegen/commit/110166f) add initialize flow support

* [2bc1a14](https://github.com/sireum/hamr-codegen/commit/2bc1a14) update build props

* [03b0b65](https://github.com/sireum/hamr-codegen/commit/03b0b65) update submodules

* [0f5d767](https://github.com/sireum/hamr-codegen/commit/0f5d767) add gumbo flow support

* [6a03b45](https://github.com/sireum/hamr-codegen/commit/6a03b45) update submodules

* [2b50759](https://github.com/sireum/hamr-codegen/commit/2b50759) gumbox

* [f8c6baf](https://github.com/sireum/hamr-codegen/commit/f8c6baf) update submodules

* [4d877f8](https://github.com/sireum/hamr-codegen/commit/4d877f8) update submodules

* [b3bcc0f](https://github.com/sireum/hamr-codegen/commit/b3bcc0f) update build props

* [0404fac](https://github.com/sireum/hamr-codegen/commit/0404fac) update submodules

* [1094960](https://github.com/sireum/hamr-codegen/commit/1094960) update build props

* [51ad07c](https://github.com/sireum/hamr-codegen/commit/51ad07c) update build props
</details>
<br>


# [4.20230228.964dbb2](https://github.com/sireum/kekinian/releases/tag/4.20230228.964dbb2)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230228.964dbb2 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [17d643a](https://github.com/sireum/hamr-codegen/commit/17d643a) update submodules

* [4dc6087](https://github.com/sireum/hamr-codegen/commit/4dc6087) update submodules

* [2e6d544](https://github.com/sireum/hamr-codegen/commit/2e6d544) update build props

* [0eb8c71](https://github.com/sireum/hamr-codegen/commit/0eb8c71) Added missing return keyword.

* [f552166](https://github.com/sireum/hamr-codegen/commit/f552166) sed linux fix

* [9b3a15d](https://github.com/sireum/hamr-codegen/commit/9b3a15d) update submodules

* [fa1cc60](https://github.com/sireum/hamr-codegen/commit/fa1cc60) update build props

* [f5f18f2](https://github.com/sireum/hamr-codegen/commit/f5f18f2) use z3 4.11.2. on camkes

* [7f8f602](https://github.com/sireum/hamr-codegen/commit/7f8f602) revert win ci

* [2e53d80](https://github.com/sireum/hamr-codegen/commit/2e53d80) update submodules

* [27ed9ac](https://github.com/sireum/hamr-codegen/commit/27ed9ac) update build prop

* [a294de6](https://github.com/sireum/hamr-codegen/commit/a294de6) update submodules

* [74bc0d0](https://github.com/sireum/hamr-codegen/commit/74bc0d0) add bleeding edge option

* [7337fc3](https://github.com/sireum/hamr-codegen/commit/7337fc3) update workflows

* [3a69695](https://github.com/sireum/hamr-codegen/commit/3a69695) update submodules

* [50e90ce](https://github.com/sireum/hamr-codegen/commit/50e90ce) update build props

* [6c4901c](https://github.com/sireum/hamr-codegen/commit/6c4901c) matrix rest of cis

* [ff94dbb](https://github.com/sireum/hamr-codegen/commit/ff94dbb) Update CI-windows.yml

* [feb4e14](https://github.com/sireum/hamr-codegen/commit/feb4e14) don't fail fast

* [46fe7d4](https://github.com/sireum/hamr-codegen/commit/46fe7d4) update submodules

* [949b730](https://github.com/sireum/hamr-codegen/commit/949b730) update submodules

* [b963539](https://github.com/sireum/hamr-codegen/commit/b963539) partition vms

* [ce617c2](https://github.com/sireum/hamr-codegen/commit/ce617c2) update submodule

* [33e0d4f](https://github.com/sireum/hamr-codegen/commit/33e0d4f) use mill.bat on windows

* [1af2944](https://github.com/sireum/hamr-codegen/commit/1af2944) update submodule

* [c13a4ce](https://github.com/sireum/hamr-codegen/commit/c13a4ce) ci tweaks

* [b148253](https://github.com/sireum/hamr-codegen/commit/b148253) update submodules

* [4da2f77](https://github.com/sireum/hamr-codegen/commit/4da2f77) update build props

* [dc568de](https://github.com/sireum/hamr-codegen/commit/dc568de) update submodules

* [5062fef](https://github.com/sireum/hamr-codegen/commit/5062fef) update submodules

* [5cdb201](https://github.com/sireum/hamr-codegen/commit/5cdb201) update build props

* [82e357b](https://github.com/sireum/hamr-codegen/commit/82e357b) update arsit

* [fe5b174](https://github.com/sireum/hamr-codegen/commit/fe5b174) switch to range types

* [0f88bb1](https://github.com/sireum/hamr-codegen/commit/0f88bb1) update urls

* [a5c6d7a](https://github.com/sireum/hamr-codegen/commit/a5c6d7a) Updated trafo.

* [033af99](https://github.com/sireum/hamr-codegen/commit/033af99) Added index annotation on type parameters.

* [87046a0](https://github.com/sireum/hamr-codegen/commit/87046a0) update submodule

* [8568947](https://github.com/sireum/hamr-codegen/commit/8568947) fix yml

* [96c4b27](https://github.com/sireum/hamr-codegen/commit/96c4b27) Update CI-windows.yml

* [3759801](https://github.com/sireum/hamr-codegen/commit/3759801) Update CI-windows.yml

* [2308ebd](https://github.com/sireum/hamr-codegen/commit/2308ebd) add win ci matrix

* [1b0dbb7](https://github.com/sireum/hamr-codegen/commit/1b0dbb7) update submodules

* [c61ec5c](https://github.com/sireum/hamr-codegen/commit/c61ec5c) remove unused method

* [701d8a1](https://github.com/sireum/hamr-codegen/commit/701d8a1) update build props
</details>
<br>


# [4.20230111.54ac097](https://github.com/sireum/kekinian/releases/tag/4.20230111.54ac097)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20230111.54ac097 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [f965944](https://github.com/sireum/hamr-codegen/commit/f965944) update submodules

* [9278897](https://github.com/sireum/hamr-codegen/commit/9278897) update submodule

* [220519e](https://github.com/sireum/hamr-codegen/commit/220519e) update submodule

* [ae1c895](https://github.com/sireum/hamr-codegen/commit/ae1c895) update submodule

* [1cd5b29](https://github.com/sireum/hamr-codegen/commit/1cd5b29) update submodules

* [65d0493](https://github.com/sireum/hamr-codegen/commit/65d0493) update build deps

* [6ee72b7](https://github.com/sireum/hamr-codegen/commit/6ee72b7) update submodule

* [6b83124](https://github.com/sireum/hamr-codegen/commit/6b83124) update submodule

* [683ea6b](https://github.com/sireum/hamr-codegen/commit/683ea6b) update submodules

* [ecc2ead](https://github.com/sireum/hamr-codegen/commit/ecc2ead) allow array defs without dims

* [8bfa39c](https://github.com/sireum/hamr-codegen/commit/8bfa39c) update arsit

* [bcc129c](https://github.com/sireum/hamr-codegen/commit/bcc129c) Updated commit years.

* [e2a026f](https://github.com/sireum/hamr-codegen/commit/e2a026f) add fully qualified sergen/transpilier names
</details>
<br>


# [4.20221216.cf6bed3](https://github.com/sireum/kekinian/releases/tag/4.20221216.cf6bed3)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20221216.cf6bed3 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [aae111e](https://github.com/sireum/hamr-codegen/commit/aae111e) emit errors when array type definitions are ill formed

* [a250911](https://github.com/sireum/hamr-codegen/commit/a250911) update submodules

* [70874c6](https://github.com/sireum/hamr-codegen/commit/70874c6) remove requirement that root component's identifier ends in _Instance

* [3f165fa](https://github.com/sireum/hamr-codegen/commit/3f165fa) update build props and expected

* [680948d](https://github.com/sireum/hamr-codegen/commit/680948d) update build props

* [81c3a6d](https://github.com/sireum/hamr-codegen/commit/81c3a6d) update submodule

* [6a7aae8](https://github.com/sireum/hamr-codegen/commit/6a7aae8) fix osate hash

* [c58391e](https://github.com/sireum/hamr-codegen/commit/c58391e) update submodule

* [f21ab91](https://github.com/sireum/hamr-codegen/commit/f21ab91) update submodule

* [4deeec7](https://github.com/sireum/hamr-codegen/commit/4deeec7) allow sireum init to install ive, install osate under bin dir, better usage info

* [2b2927d](https://github.com/sireum/hamr-codegen/commit/2b2927d) update submodule

* [590fc8a](https://github.com/sireum/hamr-codegen/commit/590fc8a) update submodule

* [3effe43](https://github.com/sireum/hamr-codegen/commit/3effe43) only check versions when kekinian repo is present

* [5db5f25](https://github.com/sireum/hamr-codegen/commit/5db5f25) move osate plugin vers to separate file

* [b132d11](https://github.com/sireum/hamr-codegen/commit/b132d11) update submodule

* [f2abc87](https://github.com/sireum/hamr-codegen/commit/f2abc87) add version checks

* [81180ef](https://github.com/sireum/hamr-codegen/commit/81180ef) Merged init.ps1 into updated init.bat.

* [580c0c0](https://github.com/sireum/hamr-codegen/commit/580c0c0) update submodule

* [ff9b704](https://github.com/sireum/hamr-codegen/commit/ff9b704) Updated Slash scripts to use the new Slash preamble format.

* [54b7623](https://github.com/sireum/hamr-codegen/commit/54b7623) update submodules

* [f54fb12](https://github.com/sireum/hamr-codegen/commit/f54fb12) update submodules

* [ede744a](https://github.com/sireum/hamr-codegen/commit/ede744a) install mill
</details>
<br>


# [4.20221130.c350258](https://github.com/sireum/kekinian/releases/tag/4.20221130.c350258)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20221130.c350258 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [6d1cacf](https://github.com/sireum/hamr-codegen/commit/6d1cacf) update submodules

* [dd9e75b](https://github.com/sireum/hamr-codegen/commit/dd9e75b) install sbt

* [37d35d6](https://github.com/sireum/hamr-codegen/commit/37d35d6) update submodules

* [30e0dfe](https://github.com/sireum/hamr-codegen/commit/30e0dfe) add all slang interpolates

* [5c4abcf](https://github.com/sireum/hamr-codegen/commit/5c4abcf) update submodules

* [44b2201](https://github.com/sireum/hamr-codegen/commit/44b2201) add dimensions field to array types

* [7f29f87](https://github.com/sireum/hamr-codegen/commit/7f29f87) require name provider for aadl types

* [e145a6d](https://github.com/sireum/hamr-codegen/commit/e145a6d) update submodules

* [9dc227a](https://github.com/sireum/hamr-codegen/commit/9dc227a) install tools via kekinian's build.cmd

* [93e3f73](https://github.com/sireum/hamr-codegen/commit/93e3f73) update submodules

* [845de5b](https://github.com/sireum/hamr-codegen/commit/845de5b) use stable camkes image if latest causes failures

* [4675b57](https://github.com/sireum/hamr-codegen/commit/4675b57) update submodules

* [0ff60da](https://github.com/sireum/hamr-codegen/commit/0ff60da) update build.cmd

* [3573bf0](https://github.com/sireum/hamr-codegen/commit/3573bf0) Updated build.sc.
</details>
<br>


# [4.20221110.3001643](https://github.com/sireum/kekinian/releases/tag/4.20221110.3001643)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20221110.3001643 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [3c2c75f](https://github.com/sireum/hamr-codegen/commit/3c2c75f) update submodules

* [e88f4fe](https://github.com/sireum/hamr-codegen/commit/e88f4fe) add string to st helper

* [5368789](https://github.com/sireum/hamr-codegen/commit/5368789) update submodule

* [d5f4be3](https://github.com/sireum/hamr-codegen/commit/d5f4be3) update submodules

* [ddbf7f9](https://github.com/sireum/hamr-codegen/commit/ddbf7f9) add plugins

* [5859ec2](https://github.com/sireum/hamr-codegen/commit/5859ec2) Updated GitHub Action workflow files.

* [7156de2](https://github.com/sireum/hamr-codegen/commit/7156de2) Adapted to Slang changes.

* [98b3b49](https://github.com/sireum/hamr-codegen/commit/98b3b49) update submodule

* [b57a26b](https://github.com/sireum/hamr-codegen/commit/b57a26b) update submodule

* [5d7151a](https://github.com/sireum/hamr-codegen/commit/5d7151a) rename scripts

* [c13e21f](https://github.com/sireum/hamr-codegen/commit/c13e21f) update submodules

* [896ffe6](https://github.com/sireum/hamr-codegen/commit/896ffe6) update submodule

* [4ddd871](https://github.com/sireum/hamr-codegen/commit/4ddd871) update submodule

* [5316f0d](https://github.com/sireum/hamr-codegen/commit/5316f0d) continue on next line when end marker has been found
</details>
<br>


# [4.20221006.c8a44f5](https://github.com/sireum/kekinian/releases/tag/4.20221006.c8a44f5)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20221006.c8a44f5 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [9bba045](https://github.com/sireum/hamr-codegen/commit/9bba045) update submodule

* [f172b3c](https://github.com/sireum/hamr-codegen/commit/f172b3c) return cached results for structurally equiv annexes

* [a6bce8d](https://github.com/sireum/hamr-codegen/commit/a6bce8d) Adapted to Slang AST changes.
</details>
<br>


# [4.20220926.e3e8203](https://github.com/sireum/kekinian/releases/tag/4.20220926.e3e8203)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220926.e3e8203 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [9738d4a](https://github.com/sireum/hamr-codegen/commit/9738d4a) update submodule

* [76d249c](https://github.com/sireum/hamr-codegen/commit/76d249c) adapt to ive config changes

* [e1b0920](https://github.com/sireum/hamr-codegen/commit/e1b0920) update submodules

* [950dc2d](https://github.com/sireum/hamr-codegen/commit/950dc2d) expose name providers

* [bcac65a](https://github.com/sireum/hamr-codegen/commit/bcac65a) add osate api

* [455897c](https://github.com/sireum/hamr-codegen/commit/455897c) update submodules

* [a38f7d7](https://github.com/sireum/hamr-codegen/commit/a38f7d7) allow multiple visitors to handle annex

* [66a84a6](https://github.com/sireum/hamr-codegen/commit/66a84a6) add tipe and ive modes

* [b312c99](https://github.com/sireum/hamr-codegen/commit/b312c99) sym rename

* [d208f62](https://github.com/sireum/hamr-codegen/commit/d208f62) Update CI-camkes.yml

* [94dc904](https://github.com/sireum/hamr-codegen/commit/94dc904) stop after first transpiler error

* [9ab0d0a](https://github.com/sireum/hamr-codegen/commit/9ab0d0a) update submodule

* [243a71d](https://github.com/sireum/hamr-codegen/commit/243a71d) pass separate reporter to transpiler callback method

* [f29f437](https://github.com/sireum/hamr-codegen/commit/f29f437) update submodules

* [4b7bb88](https://github.com/sireum/hamr-codegen/commit/4b7bb88) switch to name provider

* [d350bae](https://github.com/sireum/hamr-codegen/commit/d350bae) Update CI-windows.yml

* [5b29cd2](https://github.com/sireum/hamr-codegen/commit/5b29cd2) update submodule
</details>
<br>


# [4.20220822.8603817](https://github.com/sireum/kekinian/releases/tag/4.20220822.8603817)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220822.8603817 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e625646](https://github.com/sireum/hamr-codegen/commit/e625646) update submodules

* [926b757](https://github.com/sireum/hamr-codegen/commit/926b757) ignore ext projects

* [20624e3](https://github.com/sireum/hamr-codegen/commit/20624e3) update script

* [4cb228f](https://github.com/sireum/hamr-codegen/commit/4cb228f) update submodule

* [9e3c6b0](https://github.com/sireum/hamr-codegen/commit/9e3c6b0) add support for slang binary op precedence

* [b279255](https://github.com/sireum/hamr-codegen/commit/b279255) Added support for In(this).

* [fe956b0](https://github.com/sireum/hamr-codegen/commit/fe956b0) update submodules

* [0cd2ddc](https://github.com/sireum/hamr-codegen/commit/0cd2ddc) update submodule

* [fdb118d](https://github.com/sireum/hamr-codegen/commit/fdb118d) Changed GclResolver.libraryReporter to memoize def instead of val to prevent issues with Graal native-image.

* [bd54c77](https://github.com/sireum/hamr-codegen/commit/bd54c77) update arsit

* [ad6262f](https://github.com/sireum/hamr-codegen/commit/ad6262f) update submodule

* [a83d560](https://github.com/sireum/hamr-codegen/commit/a83d560) Updated arsit.

* [d08326e](https://github.com/sireum/hamr-codegen/commit/d08326e) Added support for qualified name in reads/modifies clauses. Removed Old(...) spec exp. Added At(...) spec exp.

* [34d65c8](https://github.com/sireum/hamr-codegen/commit/34d65c8) update submodules

* [4fe5dda](https://github.com/sireum/hamr-codegen/commit/4fe5dda) add support for osate domain_mapping

* [9c43f07](https://github.com/sireum/hamr-codegen/commit/9c43f07) Added type variable annotations.

* [cc3d9e6](https://github.com/sireum/hamr-codegen/commit/cc3d9e6) Update arsit submodule for John's simple comment additions

* [fda4723](https://github.com/sireum/hamr-codegen/commit/fda4723) update submodules

* [5af55a5](https://github.com/sireum/hamr-codegen/commit/5af55a5) update submodules

* [93508d7](https://github.com/sireum/hamr-codegen/commit/93508d7) add support for pure method's Res symbol

* [45e6518](https://github.com/sireum/hamr-codegen/commit/45e6518) update submodules

* [3cd47d2](https://github.com/sireum/hamr-codegen/commit/3cd47d2) add library visitor
</details>
<br>


# [4.20220701.e3f6d0e](https://github.com/sireum/kekinian/releases/tag/4.20220701.e3f6d0e)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220701.e3f6d0e https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [3eb6137](https://github.com/sireum/hamr-codegen/commit/3eb6137) add gcl library

* [4abdce6](https://github.com/sireum/hamr-codegen/commit/4abdce6) allow MustSend without expected value for event data ports

* [f691982](https://github.com/sireum/hamr-codegen/commit/f691982) update submodules

* [ebb9144](https://github.com/sireum/hamr-codegen/commit/ebb9144) only allow state vars in In exprs

* [669c6e5](https://github.com/sireum/hamr-codegen/commit/669c6e5) resolve uifs

* [35dc033](https://github.com/sireum/hamr-codegen/commit/35dc033) update submodules

* [426fe29](https://github.com/sireum/hamr-codegen/commit/426fe29) resolve general assume/guarantee

* [5196740](https://github.com/sireum/hamr-codegen/commit/5196740) Update CI-camkes.yml

* [d9f3e25](https://github.com/sireum/hamr-codegen/commit/d9f3e25) Update CI-camkes.yml

* [a5f0217](https://github.com/sireum/hamr-codegen/commit/a5f0217) update submodule

* [b455eb6](https://github.com/sireum/hamr-codegen/commit/b455eb6) update submodule

* [6dcb3b9](https://github.com/sireum/hamr-codegen/commit/6dcb3b9) update submodules

* [7c8df57](https://github.com/sireum/hamr-codegen/commit/7c8df57) update submodules

* [11f45ad](https://github.com/sireum/hamr-codegen/commit/11f45ad) update submodule

* [622b615](https://github.com/sireum/hamr-codegen/commit/622b615) update submodule

* [f523456](https://github.com/sireum/hamr-codegen/commit/f523456) add logika test options

* [7c1bbd9](https://github.com/sireum/hamr-codegen/commit/7c1bbd9) update submodules

* [5cd7607](https://github.com/sireum/hamr-codegen/commit/5cd7607) add compute entrypoint support

* [1f7cd03](https://github.com/sireum/hamr-codegen/commit/1f7cd03) add compute entrypoint support

* [2a85510](https://github.com/sireum/hamr-codegen/commit/2a85510) rewrite api calls

* [c88f6dd](https://github.com/sireum/hamr-codegen/commit/c88f6dd) use uri

* [77b09e8](https://github.com/sireum/hamr-codegen/commit/77b09e8) handle empty case

* [ba6de67](https://github.com/sireum/hamr-codegen/commit/ba6de67) update tests

* [f085e17](https://github.com/sireum/hamr-codegen/commit/f085e17) update submodule

* [a11ba5a](https://github.com/sireum/hamr-codegen/commit/a11ba5a) add slang package info

* [761adb5](https://github.com/sireum/hamr-codegen/commit/761adb5) refine type hierarchy, add support for enums

* [1b8a791](https://github.com/sireum/hamr-codegen/commit/1b8a791) update submodules

* [30a39eb](https://github.com/sireum/hamr-codegen/commit/30a39eb) gumbo scripts

* [6b4f16a](https://github.com/sireum/hamr-codegen/commit/6b4f16a) update submodules

* [ca4e065](https://github.com/sireum/hamr-codegen/commit/ca4e065) adapt to gcl changes

* [4ba11b2](https://github.com/sireum/hamr-codegen/commit/4ba11b2) update version tags

* [c828c2e](https://github.com/sireum/hamr-codegen/commit/c828c2e) update script

* [de2b882](https://github.com/sireum/hamr-codegen/commit/de2b882) update submodules
</details>
<br>


# [4.20220526.4bb6bfe](https://github.com/sireum/kekinian/releases/tag/4.20220526.4bb6bfe)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220526.4bb6bfe https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [53edb9c](https://github.com/sireum/hamr-codegen/commit/53edb9c) add module names

* [99e96d3](https://github.com/sireum/hamr-codegen/commit/99e96d3) update submodules
</details>
<br>


# [4.20220513.725f0c2](https://github.com/sireum/kekinian/releases/tag/4.20220513.725f0c2)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220513.725f0c2 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [4a59f4f](https://github.com/sireum/hamr-codegen/commit/4a59f4f) Update CI-camkes.yml

* [d453e19](https://github.com/sireum/hamr-codegen/commit/d453e19) update test submodule

* [983db91](https://github.com/sireum/hamr-codegen/commit/983db91) try emacs hack

* [1116c5a](https://github.com/sireum/hamr-codegen/commit/1116c5a) Update CI-camkes.yml

* [3ec855a](https://github.com/sireum/hamr-codegen/commit/3ec855a) recursive checkout

* [fdd98ad](https://github.com/sireum/hamr-codegen/commit/fdd98ad) update

* [c9cfb4b](https://github.com/sireum/hamr-codegen/commit/c9cfb4b) update camkes ci

* [b9ce765](https://github.com/sireum/hamr-codegen/commit/b9ce765) add camkes ci

* [42fa428](https://github.com/sireum/hamr-codegen/commit/42fa428) Merge branch 'master' of https://github.com/sireum/hamr-codegen

* [2d03677](https://github.com/sireum/hamr-codegen/commit/2d03677) Merge test submodule (updates to TempSensor behavior tests)

* [5b05c60](https://github.com/sireum/hamr-codegen/commit/5b05c60) correct var name

* [9177278](https://github.com/sireum/hamr-codegen/commit/9177278) Use both CVC4 and CVC5.

* [54ece80](https://github.com/sireum/hamr-codegen/commit/54ece80) fix file name

* [12281b7](https://github.com/sireum/hamr-codegen/commit/12281b7) update submodules

* [2c855ae](https://github.com/sireum/hamr-codegen/commit/2c855ae) install osate gumbo when ci

* [755c133](https://github.com/sireum/hamr-codegen/commit/755c133) Merge branch 'master' of https://github.com/sireum/hamr-codegen

* [bfb5fb0](https://github.com/sireum/hamr-codegen/commit/bfb5fb0) test submodule (adding TempControl)
</details>
<br>


# [4.20220502.83e1a78](https://github.com/sireum/kekinian/releases/tag/4.20220502.83e1a78)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220502.83e1a78 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [52037fd](https://github.com/sireum/hamr-codegen/commit/52037fd) update instructions

* [a3a7626](https://github.com/sireum/hamr-codegen/commit/a3a7626) sbt/mill project gen optional

* [b426809](https://github.com/sireum/hamr-codegen/commit/b426809) preserve final empty segment

* [f9d32de](https://github.com/sireum/hamr-codegen/commit/f9d32de) update submodule

* [29ab0aa](https://github.com/sireum/hamr-codegen/commit/29ab0aa) Add Temp Control Simple Temp to Behavior Tests (updating submodule)

* [50e718c](https://github.com/sireum/hamr-codegen/commit/50e718c) update submodule

* [28c0d46](https://github.com/sireum/hamr-codegen/commit/28c0d46) install osate gumbo when ci

* [49a9dc9](https://github.com/sireum/hamr-codegen/commit/49a9dc9) update submodule

* [adf6534](https://github.com/sireum/hamr-codegen/commit/adf6534) Update testing submodule

* [01c7b61](https://github.com/sireum/hamr-codegen/commit/01c7b61) update submodule

* [5d6bce8](https://github.com/sireum/hamr-codegen/commit/5d6bce8) update submodule

* [787e31f](https://github.com/sireum/hamr-codegen/commit/787e31f) update submodule

* [3c4d79c](https://github.com/sireum/hamr-codegen/commit/3c4d79c) Update CI-windows.yml

* [4c9cf0d](https://github.com/sireum/hamr-codegen/commit/4c9cf0d) update submodule

* [7d34f33](https://github.com/sireum/hamr-codegen/commit/7d34f33) move to gumbo package

* [2f54c3b](https://github.com/sireum/hamr-codegen/commit/2f54c3b) update submodule

* [cdbd60b](https://github.com/sireum/hamr-codegen/commit/cdbd60b) update submodule

* [ee893a2](https://github.com/sireum/hamr-codegen/commit/ee893a2) remove gumbo_models

* [ab0db06](https://github.com/sireum/hamr-codegen/commit/ab0db06) update preamble

* [1b13d70](https://github.com/sireum/hamr-codegen/commit/1b13d70) add repos

* [d4ae63a](https://github.com/sireum/hamr-codegen/commit/d4ae63a) update submodule

* [35ace04](https://github.com/sireum/hamr-codegen/commit/35ace04) update submodules

* [38a43e1](https://github.com/sireum/hamr-codegen/commit/38a43e1) add cligen for testing

* [fe3502f](https://github.com/sireum/hamr-codegen/commit/fe3502f) update submodules

* [45812e1](https://github.com/sireum/hamr-codegen/commit/45812e1) add external tests

* [487d1bd](https://github.com/sireum/hamr-codegen/commit/487d1bd) update submodules

* [be325b7](https://github.com/sireum/hamr-codegen/commit/be325b7) switch to test-ext
</details>
<br>


# [4.20220414.3ed18e3](https://github.com/sireum/kekinian/releases/tag/4.20220414.3ed18e3)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220414.3ed18e3 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [6ea2656](https://github.com/sireum/hamr-codegen/commit/6ea2656) update submodule

* [ffec56e](https://github.com/sireum/hamr-codegen/commit/ffec56e) add back verbose check

* [6dfa78f](https://github.com/sireum/hamr-codegen/commit/6dfa78f) move test_tmp to test

* [cd3be2a](https://github.com/sireum/hamr-codegen/commit/cd3be2a) move test to tmp

* [a489b52](https://github.com/sireum/hamr-codegen/commit/a489b52) update submodules

* [eed4a46](https://github.com/sireum/hamr-codegen/commit/eed4a46) update submodules

* [e00b3b2](https://github.com/sireum/hamr-codegen/commit/e00b3b2) update submodules

* [35c86cc](https://github.com/sireum/hamr-codegen/commit/35c86cc) refine message printing

* [8176e7a](https://github.com/sireum/hamr-codegen/commit/8176e7a) update submodules

* [cf3950f](https://github.com/sireum/hamr-codegen/commit/cf3950f) add slang frontend to common's mill module

* [6f65fac](https://github.com/sireum/hamr-codegen/commit/6f65fac) check for dir

* [130a55e](https://github.com/sireum/hamr-codegen/commit/130a55e) update submodules

* [d29ecf6](https://github.com/sireum/hamr-codegen/commit/d29ecf6) allow for different test roots

* [5c223c6](https://github.com/sireum/hamr-codegen/commit/5c223c6) allow for different test roots

* [dce59d5](https://github.com/sireum/hamr-codegen/commit/dce59d5) resolve modifies for initializes

* [ba95ca1](https://github.com/sireum/hamr-codegen/commit/ba95ca1) add extra test resources

* [62aa126](https://github.com/sireum/hamr-codegen/commit/62aa126) load library reporter in object

* [02830c0](https://github.com/sireum/hamr-codegen/commit/02830c0) update submodules

* [91369b9](https://github.com/sireum/hamr-codegen/commit/91369b9) add empty type

* [53306d4](https://github.com/sireum/hamr-codegen/commit/53306d4) update submodules

* [9c6300c](https://github.com/sireum/hamr-codegen/commit/9c6300c) add lit interpolation support

* [e4254d5](https://github.com/sireum/hamr-codegen/commit/e4254d5) update submodules

* [6b33e9a](https://github.com/sireum/hamr-codegen/commit/6b33e9a) update submodules

* [7aecf60](https://github.com/sireum/hamr-codegen/commit/7aecf60) remove redundant error msg

* [f46ca5d](https://github.com/sireum/hamr-codegen/commit/f46ca5d) update submodules

* [751d9fd](https://github.com/sireum/hamr-codegen/commit/751d9fd) update submodules

* [5251b97](https://github.com/sireum/hamr-codegen/commit/5251b97) update submodule

* [a53df6c](https://github.com/sireum/hamr-codegen/commit/a53df6c) remove slang when cleaning

* [9540b59](https://github.com/sireum/hamr-codegen/commit/9540b59) remove symlink

* [9621a27](https://github.com/sireum/hamr-codegen/commit/9621a27) update submodules

* [62f4eac](https://github.com/sireum/hamr-codegen/commit/62f4eac) switch gcl to use slang exp

* [851d527](https://github.com/sireum/hamr-codegen/commit/851d527) switch to using IdPath
</details>
<br>


# [4.20220321.26aca77](https://github.com/sireum/kekinian/releases/tag/4.20220321.26aca77)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220321.26aca77 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [5c6cbdf](https://github.com/sireum/hamr-codegen/commit/5c6cbdf) update arsit

* [fbe8aac](https://github.com/sireum/hamr-codegen/commit/fbe8aac) update arsit

* [ac132a7](https://github.com/sireum/hamr-codegen/commit/ac132a7) update act

* [433760b](https://github.com/sireum/hamr-codegen/commit/433760b) update act
</details>
<br>


# [4.20220302.7ee2ecc](https://github.com/sireum/kekinian/releases/tag/4.20220302.7ee2ecc)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220302.7ee2ecc https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e482e0d](https://github.com/sireum/hamr-codegen/commit/e482e0d) update submodules

* [0da270a](https://github.com/sireum/hamr-codegen/commit/0da270a) update act to support camkes-vm

* [9b425b0](https://github.com/sireum/hamr-codegen/commit/9b425b0) remove virt processor period requirement

* [d97b031](https://github.com/sireum/hamr-codegen/commit/d97b031) add api check flags

* [6cd7677](https://github.com/sireum/hamr-codegen/commit/6cd7677) update submodules
</details>
<br>


# [4.20220211.d12a474](https://github.com/sireum/kekinian/releases/tag/4.20220211.d12a474)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220211.d12a474 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [9a6cdb2](https://github.com/sireum/hamr-codegen/commit/9a6cdb2) update submodule

* [55d039d](https://github.com/sireum/hamr-codegen/commit/55d039d) update submodules

* [a3905c9](https://github.com/sireum/hamr-codegen/commit/a3905c9) update submodules

* [c131680](https://github.com/sireum/hamr-codegen/commit/c131680) resolve integration clauses
</details>
<br>


# [4.20220128.7d1855b](https://github.com/sireum/kekinian/releases/tag/4.20220128.7d1855b)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20220128.7d1855b https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [80e9199](https://github.com/sireum/hamr-codegen/commit/80e9199) update submodules

* [a2f99d6](https://github.com/sireum/hamr-codegen/commit/a2f99d6) update submodules

* [cf99cd3](https://github.com/sireum/hamr-codegen/commit/cf99cd3) Merge branch 'master' of https://github.com/sireum/hamr-codegen

* [e33069d](https://github.com/sireum/hamr-codegen/commit/e33069d) update submodules

* [a264ac3](https://github.com/sireum/hamr-codegen/commit/a264ac3) Upgraded mill.

* [187bd82](https://github.com/sireum/hamr-codegen/commit/187bd82) Updated copyright years.

* [22b0a1a](https://github.com/sireum/hamr-codegen/commit/22b0a1a) Upgraded mill.

* [8def53c](https://github.com/sireum/hamr-codegen/commit/8def53c) Updated copyright years.
</details>
<br>


# [4.20211223.c717627](https://github.com/sireum/kekinian/releases/tag/4.20211223.c717627)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20211223.c717627 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [c3cae70](https://github.com/sireum/hamr-codegen/commit/c3cae70) update submodules

* [a9a3ad9](https://github.com/sireum/hamr-codegen/commit/a9a3ad9) add annex visitor trait

* [da78930](https://github.com/sireum/hamr-codegen/commit/da78930) add annex visitor trait

* [f988858](https://github.com/sireum/hamr-codegen/commit/f988858) update submodules

* [faffbc1](https://github.com/sireum/hamr-codegen/commit/faffbc1) add bool lit support

* [b24bb39](https://github.com/sireum/hamr-codegen/commit/b24bb39) update submodules

* [38ca7c1](https://github.com/sireum/hamr-codegen/commit/38ca7c1) tipe fixes

* [4a1cef5](https://github.com/sireum/hamr-codegen/commit/4a1cef5) update submodules

* [5c0094e](https://github.com/sireum/hamr-codegen/commit/5c0094e) diff off name

* [23d38dc](https://github.com/sireum/hamr-codegen/commit/23d38dc) update submodules

* [81553b3](https://github.com/sireum/hamr-codegen/commit/81553b3) added gumbo gen

* [5a85ef0](https://github.com/sireum/hamr-codegen/commit/5a85ef0) Merge branch 'dev' of https://github.com/sireum/hamr-codegen into dev

* [42fbeb6](https://github.com/sireum/hamr-codegen/commit/42fbeb6) added gumbo

* [3159829](https://github.com/sireum/hamr-codegen/commit/3159829) update submodules

* [10277c7](https://github.com/sireum/hamr-codegen/commit/10277c7) added gumbo

* [b81cd5a](https://github.com/sireum/hamr-codegen/commit/b81cd5a) Adapted to changes in runtime library.

* [54294f3](https://github.com/sireum/hamr-codegen/commit/54294f3) Generalized cvc4 to cvc to support both cvc4 and cvc5.

* [0ea7ab8](https://github.com/sireum/hamr-codegen/commit/0ea7ab8) Generalized cvc4 to cvc to support both cvc4 and cvc5.
</details>
<br>


# [4.20211207.54c5b35](https://github.com/sireum/kekinian/releases/tag/4.20211207.54c5b35)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20211207.54c5b35 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [58b40f7](https://github.com/sireum/hamr-codegen/commit/58b40f7) Adapted to changes in runtime library.

* [4d75e95](https://github.com/sireum/hamr-codegen/commit/4d75e95) update submodules

* [308c233](https://github.com/sireum/hamr-codegen/commit/308c233) update submodules

* [6707535](https://github.com/sireum/hamr-codegen/commit/6707535) remove vm rewriter

* [a533f94](https://github.com/sireum/hamr-codegen/commit/a533f94) add secmf support
</details>
<br>


# [4.20211103.09b1e32](https://github.com/sireum/kekinian/releases/tag/4.20211103.09b1e32)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20211103.09b1e32 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [a4ac29f](https://github.com/sireum/hamr-codegen/commit/a4ac29f) update submodules

* [699ef25](https://github.com/sireum/hamr-codegen/commit/699ef25) update submodules

* [f75d1f5](https://github.com/sireum/hamr-codegen/commit/f75d1f5) don't compile c code under window github ci

* [ee74fdd](https://github.com/sireum/hamr-codegen/commit/ee74fdd) try older cygwin

* [8eeec9b](https://github.com/sireum/hamr-codegen/commit/8eeec9b) try older cygwin

* [84121a0](https://github.com/sireum/hamr-codegen/commit/84121a0) update submodules

* [310c5cc](https://github.com/sireum/hamr-codegen/commit/310c5cc) update submodules

* [c5fa032](https://github.com/sireum/hamr-codegen/commit/c5fa032) update submodules

* [4945e58](https://github.com/sireum/hamr-codegen/commit/4945e58) update submodules

* [0818ad6](https://github.com/sireum/hamr-codegen/commit/0818ad6) add features to all components

* [048e4b5](https://github.com/sireum/hamr-codegen/commit/048e4b5) update submodules

* [8472166](https://github.com/sireum/hamr-codegen/commit/8472166) add period

* [b3b84df](https://github.com/sireum/hamr-codegen/commit/b3b84df) add dispatchable trait, drop support for hamr vm annotation

* [36fe0c1](https://github.com/sireum/hamr-codegen/commit/36fe0c1) update submodules

* [ba4a7da](https://github.com/sireum/hamr-codegen/commit/ba4a7da) add features to processes, add aadl maps

* [5ae3d8d](https://github.com/sireum/hamr-codegen/commit/5ae3d8d) update submodules

* [7c4ac66](https://github.com/sireum/hamr-codegen/commit/7c4ac66) update submodules

* [75581e2](https://github.com/sireum/hamr-codegen/commit/75581e2) update expected

* [0aba07d](https://github.com/sireum/hamr-codegen/commit/0aba07d) update submodules

* [50fe555](https://github.com/sireum/hamr-codegen/commit/50fe555) update submodules

* [970037a](https://github.com/sireum/hamr-codegen/commit/970037a) update submodules

* [ce115ec](https://github.com/sireum/hamr-codegen/commit/ce115ec) update expected
</details>
<br>


# [4.20211020.ddd6008](https://github.com/sireum/kekinian/releases/tag/4.20211020.ddd6008)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20211020.ddd6008 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [62e7d64](https://github.com/sireum/hamr-codegen/commit/62e7d64) update submodules

* [6e5c503](https://github.com/sireum/hamr-codegen/commit/6e5c503) update submodules

* [b640fa2](https://github.com/sireum/hamr-codegen/commit/b640fa2) update submodules

* [059433d](https://github.com/sireum/hamr-codegen/commit/059433d) update submodules

* [5b7b9a7](https://github.com/sireum/hamr-codegen/commit/5b7b9a7) update submodules

* [0cb3f8b](https://github.com/sireum/hamr-codegen/commit/0cb3f8b) Tipe fixes.

* [e4e3505](https://github.com/sireum/hamr-codegen/commit/e4e3505) update submodules

* [3eebe26](https://github.com/sireum/hamr-codegen/commit/3eebe26) Update README.md

* [5a99eee](https://github.com/sireum/hamr-codegen/commit/5a99eee) update submodules

* [8b120bf](https://github.com/sireum/hamr-codegen/commit/8b120bf) use proyek tipe

* [4f505d5](https://github.com/sireum/hamr-codegen/commit/4f505d5) fix event data port issue, add smt solver downloads, add smt2 test mode

* [893280c](https://github.com/sireum/hamr-codegen/commit/893280c) remove github test

* [6d315af](https://github.com/sireum/hamr-codegen/commit/6d315af) github win debug

* [29664ec](https://github.com/sireum/hamr-codegen/commit/29664ec) tipe fixes

* [b938617](https://github.com/sireum/hamr-codegen/commit/b938617) update submodules

* [b99d69f](https://github.com/sireum/hamr-codegen/commit/b99d69f) pacer checks

* [3ba370f](https://github.com/sireum/hamr-codegen/commit/3ba370f) update submodules

* [b5bdb8c](https://github.com/sireum/hamr-codegen/commit/b5bdb8c) Update README.md

* [7be35e8](https://github.com/sireum/hamr-codegen/commit/7be35e8) update submodules

* [a4080cf](https://github.com/sireum/hamr-codegen/commit/a4080cf) split ci's
</details>
<br>


# [4.20210930.abd2495](https://github.com/sireum/kekinian/releases/tag/4.20210930.abd2495)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210930.abd2495 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [cda3e08](https://github.com/sireum/hamr-codegen/commit/cda3e08) update submodules

* [b003db2](https://github.com/sireum/hamr-codegen/commit/b003db2) update submodules

* [a3a233f](https://github.com/sireum/hamr-codegen/commit/a3a233f) remove window hacks

* [40aa356](https://github.com/sireum/hamr-codegen/commit/40aa356) update submodules

* [0a064e3](https://github.com/sireum/hamr-codegen/commit/0a064e3) win fix

* [357632a](https://github.com/sireum/hamr-codegen/commit/357632a) update submodules

* [27f8cc5](https://github.com/sireum/hamr-codegen/commit/27f8cc5) update submodules

* [21a6475](https://github.com/sireum/hamr-codegen/commit/21a6475) update submodules

* [57aa4f2](https://github.com/sireum/hamr-codegen/commit/57aa4f2) update submodules

* [75885f7](https://github.com/sireum/hamr-codegen/commit/75885f7) try copying cache to c drive under windows

* [d71e722](https://github.com/sireum/hamr-codegen/commit/d71e722) update submodules

* [1a98962](https://github.com/sireum/hamr-codegen/commit/1a98962) 7z manually

* [d17b49f](https://github.com/sireum/hamr-codegen/commit/d17b49f) win path

* [2f6ecb3](https://github.com/sireum/hamr-codegen/commit/2f6ecb3) 7z testing win

* [03f98cf](https://github.com/sireum/hamr-codegen/commit/03f98cf) swtich to GitHub

* [88c01c2](https://github.com/sireum/hamr-codegen/commit/88c01c2) fix build.cmd

* [6114f32](https://github.com/sireum/hamr-codegen/commit/6114f32) retry fetching releases

* [371c3d0](https://github.com/sireum/hamr-codegen/commit/371c3d0) enable release download

* [32e29ca](https://github.com/sireum/hamr-codegen/commit/32e29ca) update submodules

* [0eef65f](https://github.com/sireum/hamr-codegen/commit/0eef65f) add proyek ive mode

* [75198c4](https://github.com/sireum/hamr-codegen/commit/75198c4) add missing symbols

* [ac120c1](https://github.com/sireum/hamr-codegen/commit/ac120c1) add missing symbols

* [ecf07cd](https://github.com/sireum/hamr-codegen/commit/ecf07cd) update submodules
</details>
<br>


# [4.20210913.ab8f3db](https://github.com/sireum/kekinian/releases/tag/4.20210913.ab8f3db)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210913.ab8f3db https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [7e5b563](https://github.com/sireum/hamr-codegen/commit/7e5b563) update submodules

* [0a626b7](https://github.com/sireum/hamr-codegen/commit/0a626b7) update submodules

* [290404b](https://github.com/sireum/hamr-codegen/commit/290404b) update submodules

* [f2c23ca](https://github.com/sireum/hamr-codegen/commit/f2c23ca) update submodules

* [2063075](https://github.com/sireum/hamr-codegen/commit/2063075) update submodules

* [eb6d41d](https://github.com/sireum/hamr-codegen/commit/eb6d41d) add c arch instance name

* [5d5ee3f](https://github.com/sireum/hamr-codegen/commit/5d5ee3f) update submodules

* [facdc75](https://github.com/sireum/hamr-codegen/commit/facdc75) Update main.yml

* [3825033](https://github.com/sireum/hamr-codegen/commit/3825033) Update main.yml

* [4de5776](https://github.com/sireum/hamr-codegen/commit/4de5776) Update main.yml

* [81526d0](https://github.com/sireum/hamr-codegen/commit/81526d0) switch to gcc

* [490951e](https://github.com/sireum/hamr-codegen/commit/490951e) update submodules

* [d4276c4](https://github.com/sireum/hamr-codegen/commit/d4276c4) update submodules

* [b613486](https://github.com/sireum/hamr-codegen/commit/b613486) update submodules

* [4dd3fd3](https://github.com/sireum/hamr-codegen/commit/4dd3fd3) Update main.yml

* [93eab91](https://github.com/sireum/hamr-codegen/commit/93eab91) update submodules

* [15970f3](https://github.com/sireum/hamr-codegen/commit/15970f3) sfAssert

* [90f2581](https://github.com/sireum/hamr-codegen/commit/90f2581) update submodules

* [602a728](https://github.com/sireum/hamr-codegen/commit/602a728) revert yml

* [f650b8e](https://github.com/sireum/hamr-codegen/commit/f650b8e) update scripts

* [d140f05](https://github.com/sireum/hamr-codegen/commit/d140f05) Update main.yml

* [7318a7c](https://github.com/sireum/hamr-codegen/commit/7318a7c) update expected

* [9a5266c](https://github.com/sireum/hamr-codegen/commit/9a5266c) update submodules

* [7675303](https://github.com/sireum/hamr-codegen/commit/7675303) update submodules

* [95c7b94](https://github.com/sireum/hamr-codegen/commit/95c7b94) use symbol's method

* [ae7f5db](https://github.com/sireum/hamr-codegen/commit/ae7f5db) update submodules

* [f8f38de](https://github.com/sireum/hamr-codegen/commit/f8f38de) update submodules

* [3abf14a](https://github.com/sireum/hamr-codegen/commit/3abf14a) update submodules

* [de446d3](https://github.com/sireum/hamr-codegen/commit/de446d3) tipe fixes

* [08eb255](https://github.com/sireum/hamr-codegen/commit/08eb255) update submodules

* [4e940db](https://github.com/sireum/hamr-codegen/commit/4e940db) use converted content

* [09780ec](https://github.com/sireum/hamr-codegen/commit/09780ec) update arsit

* [447b336](https://github.com/sireum/hamr-codegen/commit/447b336) update arsit
</details>
<br>


# [4.20210901.36f6227](https://github.com/sireum/kekinian/releases/tag/4.20210901.36f6227)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210901.36f6227 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [7c83d20](https://github.com/sireum/hamr-codegen/commit/7c83d20) update expected

* [830329f](https://github.com/sireum/hamr-codegen/commit/830329f) add 'external' resources

* [ece9b3c](https://github.com/sireum/hamr-codegen/commit/ece9b3c) add back tipe

* [9e703b1](https://github.com/sireum/hamr-codegen/commit/9e703b1) update release script

* [4c7a7a3](https://github.com/sireum/hamr-codegen/commit/4c7a7a3) temporarily disable tipe check

* [c1eee1d](https://github.com/sireum/hamr-codegen/commit/c1eee1d) update build props

* [5befad0](https://github.com/sireum/hamr-codegen/commit/5befad0) update submodules

* [74f2778](https://github.com/sireum/hamr-codegen/commit/74f2778) ensure OS property is Linux for virtual machines

* [29dc420](https://github.com/sireum/hamr-codegen/commit/29dc420) update transpiler test expected

* [a4c0d62](https://github.com/sireum/hamr-codegen/commit/a4c0d62) update submodules

* [d2525df](https://github.com/sireum/hamr-codegen/commit/d2525df) also use virtual processors to identify virtual machines

* [3b233bd](https://github.com/sireum/hamr-codegen/commit/3b233bd) update submodules

* [6062f47](https://github.com/sireum/hamr-codegen/commit/6062f47) github hook test

* [3c91172](https://github.com/sireum/hamr-codegen/commit/3c91172) update submodules
</details>
<br>


# [4.20210824.72bece8](https://github.com/sireum/kekinian/releases/tag/4.20210824.72bece8)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210824.72bece8 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [89b15d1](https://github.com/sireum/hamr-codegen/commit/89b15d1) github hook test

* [f17529d](https://github.com/sireum/hamr-codegen/commit/f17529d) update scripts

* [5b06a42](https://github.com/sireum/hamr-codegen/commit/5b06a42) update submodule

* [cb65e34](https://github.com/sireum/hamr-codegen/commit/cb65e34) update submodules

* [7694040](https://github.com/sireum/hamr-codegen/commit/7694040) update submodules

* [f753556](https://github.com/sireum/hamr-codegen/commit/f753556) Updated arsit and art.

* [4097fb9](https://github.com/sireum/hamr-codegen/commit/4097fb9) Updated art and arsit.

* [af979c6](https://github.com/sireum/hamr-codegen/commit/af979c6) update submodules

* [2770cac](https://github.com/sireum/hamr-codegen/commit/2770cac) update submodules

* [231271d](https://github.com/sireum/hamr-codegen/commit/231271d) update submodules
</details>
<br>


# [4.20210816.dd5f584](https://github.com/sireum/kekinian/releases/tag/4.20210816.dd5f584)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210816.dd5f584 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [8aefd61](https://github.com/sireum/hamr-codegen/commit/8aefd61) update submodules

* [63e6050](https://github.com/sireum/hamr-codegen/commit/63e6050) update submodules

* [088a718](https://github.com/sireum/hamr-codegen/commit/088a718) update submodules

* [77af1e6](https://github.com/sireum/hamr-codegen/commit/77af1e6) update submodules

* [6daa23b](https://github.com/sireum/hamr-codegen/commit/6daa23b) update submodules

* [a30d968](https://github.com/sireum/hamr-codegen/commit/a30d968) t

* [3d92d7d](https://github.com/sireum/hamr-codegen/commit/3d92d7d) update submodules

* [9ffe3b1](https://github.com/sireum/hamr-codegen/commit/9ffe3b1) t

* [043fda6](https://github.com/sireum/hamr-codegen/commit/043fda6) update arsit

* [dc93cd5](https://github.com/sireum/hamr-codegen/commit/dc93cd5) scheduling tweaks

* [ee0e7b9](https://github.com/sireum/hamr-codegen/commit/ee0e7b9) update arsit

* [98e0807](https://github.com/sireum/hamr-codegen/commit/98e0807) helper methods

* [c193198](https://github.com/sireum/hamr-codegen/commit/c193198) add crlf

* [a18bf68](https://github.com/sireum/hamr-codegen/commit/a18bf68) update submodules

* [8bca1aa](https://github.com/sireum/hamr-codegen/commit/8bca1aa) update arsit

* [140a2d5](https://github.com/sireum/hamr-codegen/commit/140a2d5) update arsit

* [ec10a98](https://github.com/sireum/hamr-codegen/commit/ec10a98) slash transpile

* [e6197fd](https://github.com/sireum/hamr-codegen/commit/e6197fd) revert resource content back to ST

* [5504f8d](https://github.com/sireum/hamr-codegen/commit/5504f8d) update submodules

* [c53df5f](https://github.com/sireum/hamr-codegen/commit/c53df5f) standardize resource creation

* [7eb92f8](https://github.com/sireum/hamr-codegen/commit/7eb92f8) standardize resource creation

* [b3fd638](https://github.com/sireum/hamr-codegen/commit/b3fd638) update submodules

* [dc3e3e4](https://github.com/sireum/hamr-codegen/commit/dc3e3e4) add update site builder

* [60600e9](https://github.com/sireum/hamr-codegen/commit/60600e9) update subdmodules

* [90aa571](https://github.com/sireum/hamr-codegen/commit/90aa571) add proyek ive support

* [648d55f](https://github.com/sireum/hamr-codegen/commit/648d55f) update submodules

* [b81eaa9](https://github.com/sireum/hamr-codegen/commit/b81eaa9) tweak

* [4b0d8d5](https://github.com/sireum/hamr-codegen/commit/4b0d8d5) update submodules

* [c48f5ac](https://github.com/sireum/hamr-codegen/commit/c48f5ac) update submodules

* [3d9054f](https://github.com/sireum/hamr-codegen/commit/3d9054f) update art

* [05b00ec](https://github.com/sireum/hamr-codegen/commit/05b00ec) update submodules

* [54b95cd](https://github.com/sireum/hamr-codegen/commit/54b95cd) update submodules

* [be79e0c](https://github.com/sireum/hamr-codegen/commit/be79e0c) add pacing method

* [ab79ff0](https://github.com/sireum/hamr-codegen/commit/ab79ff0) disallow in/out ports

* [34c6d89](https://github.com/sireum/hamr-codegen/commit/34c6d89) update arsit

* [7534dc5](https://github.com/sireum/hamr-codegen/commit/7534dc5) handle ba/bless unary exp

* [d814181](https://github.com/sireum/hamr-codegen/commit/d814181) Removed dependency on com.sksamuel.diff.

* [82d701b](https://github.com/sireum/hamr-codegen/commit/82d701b) fix copyright (again)
</details>
<br>


# [4.20210617.8a885a8](https://github.com/sireum/kekinian/releases/tag/4.20210617.8a885a8)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210617.8a885a8 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [fda4caa](https://github.com/sireum/hamr-codegen/commit/fda4caa) update submodules

* [beb026e](https://github.com/sireum/hamr-codegen/commit/beb026e) fix copyright

* [676df90](https://github.com/sireum/hamr-codegen/commit/676df90) update submodules

* [63b00cb](https://github.com/sireum/hamr-codegen/commit/63b00cb) require cakeml components to be periodic

* [9c35dd0](https://github.com/sireum/hamr-codegen/commit/9c35dd0) Uniformized copyright years.

* [15b741b](https://github.com/sireum/hamr-codegen/commit/15b741b) Cleaned up.

* [5e343a1](https://github.com/sireum/hamr-codegen/commit/5e343a1) update submodules

* [b4ab7ac](https://github.com/sireum/hamr-codegen/commit/b4ab7ac) require cakeml components to be periodic

* [b9fa4f6](https://github.com/sireum/hamr-codegen/commit/b9fa4f6) Uniformized copyright years.

* [ba6887e](https://github.com/sireum/hamr-codegen/commit/ba6887e) Cleaned up.

* [fd7c8d6](https://github.com/sireum/hamr-codegen/commit/fd7c8d6) update submodules

* [fc40e11](https://github.com/sireum/hamr-codegen/commit/fc40e11) Merge branch 'master' of https://github.com/sireum/hamr-codegen

* [e4f57c8](https://github.com/sireum/hamr-codegen/commit/e4f57c8) update act
</details>
<br>


# [4.20210528.cf3c649](https://github.com/sireum/kekinian/releases/tag/4.20210528.cf3c649)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210528.cf3c649 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [5d49612](https://github.com/sireum/hamr-codegen/commit/5d49612) update submodules

* [346506c](https://github.com/sireum/hamr-codegen/commit/346506c) update submodules

* [28723a3](https://github.com/sireum/hamr-codegen/commit/28723a3) update submodules

* [54c129a](https://github.com/sireum/hamr-codegen/commit/54c129a) update submodules

* [79894ff](https://github.com/sireum/hamr-codegen/commit/79894ff) tipe fixes

* [39975e6](https://github.com/sireum/hamr-codegen/commit/39975e6) type checking

* [838aa9d](https://github.com/sireum/hamr-codegen/commit/838aa9d) start bts symbol/type resolution
</details>
<br>


# [4.20210512.a968fb3](https://github.com/sireum/kekinian/releases/tag/4.20210512.a968fb3)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210512.a968fb3 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [40a86da](https://github.com/sireum/hamr-codegen/commit/40a86da) tipe fixes

* [f0f8378](https://github.com/sireum/hamr-codegen/commit/f0f8378) rewrite bts port out assignment expressions

* [38f3b86](https://github.com/sireum/hamr-codegen/commit/38f3b86) update submodules

* [462c3f9](https://github.com/sireum/hamr-codegen/commit/462c3f9) relax sel4 restrictions

* [8a0e24b](https://github.com/sireum/hamr-codegen/commit/8a0e24b) update submodules

* [78d320f](https://github.com/sireum/hamr-codegen/commit/78d320f) check for Data_Size for unhandled data components (may be extending a base_type)(

* [e627e23](https://github.com/sireum/hamr-codegen/commit/e627e23) update submodules

* [1328a7d](https://github.com/sireum/hamr-codegen/commit/1328a7d) update submodules

* [49c218c](https://github.com/sireum/hamr-codegen/commit/49c218c) tipe fix

* [7b16d2c](https://github.com/sireum/hamr-codegen/commit/7b16d2c) reject models containing vms, wire protocol, cakeml components and platform is sel4_only or tb, move codegen option to common

* [142ce36](https://github.com/sireum/hamr-codegen/commit/142ce36) remove versions.properties

* [96f2a8e](https://github.com/sireum/hamr-codegen/commit/96f2a8e) update submodules

* [72514a8](https://github.com/sireum/hamr-codegen/commit/72514a8) del cli json

* [8a9bd9a](https://github.com/sireum/hamr-codegen/commit/8a9bd9a) update art

* [a0f87b7](https://github.com/sireum/hamr-codegen/commit/a0f87b7) update submodules

* [9557f15](https://github.com/sireum/hamr-codegen/commit/9557f15) update art

* [3d78eaa](https://github.com/sireum/hamr-codegen/commit/3d78eaa) update submodules

* [61a9e21](https://github.com/sireum/hamr-codegen/commit/61a9e21) update submodules

* [bf3828c](https://github.com/sireum/hamr-codegen/commit/bf3828c) update submodules

* [586a893](https://github.com/sireum/hamr-codegen/commit/586a893) update submodules

* [239bed6](https://github.com/sireum/hamr-codegen/commit/239bed6) restore old version of bit size annot check

* [4e84916](https://github.com/sireum/hamr-codegen/commit/4e84916) backoff strict bit size req

* [a19ff2e](https://github.com/sireum/hamr-codegen/commit/a19ff2e) update submodules
</details>
<br>


# [4.20210427.9cd2a8a](https://github.com/sireum/kekinian/releases/tag/4.20210427.9cd2a8a)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210427.9cd2a8a https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [50c08e9](https://github.com/sireum/hamr-codegen/commit/50c08e9) test updates

* [45ca7b9](https://github.com/sireum/hamr-codegen/commit/45ca7b9) fix return val

* [10e01b9](https://github.com/sireum/hamr-codegen/commit/10e01b9) print error message when not verbose

* [22e612d](https://github.com/sireum/hamr-codegen/commit/22e612d) use verbose flag

* [978ac9e](https://github.com/sireum/hamr-codegen/commit/978ac9e) update submodules

* [814c920](https://github.com/sireum/hamr-codegen/commit/814c920) don't handle unbounded base types when using wire protocol

* [e745b10](https://github.com/sireum/hamr-codegen/commit/e745b10) update submdoules

* [ef472ae](https://github.com/sireum/hamr-codegen/commit/ef472ae) update submodules

* [f34859b](https://github.com/sireum/hamr-codegen/commit/f34859b) add bit size to types

* [3b4077c](https://github.com/sireum/hamr-codegen/commit/3b4077c) update plugin build steps

* [2913121](https://github.com/sireum/hamr-codegen/commit/2913121) update submodules

* [c2104b1](https://github.com/sireum/hamr-codegen/commit/c2104b1) add virtual processor symbol

* [90b548a](https://github.com/sireum/hamr-codegen/commit/90b548a) keep only 5 previous plugin releases

* [f18d0b3](https://github.com/sireum/hamr-codegen/commit/f18d0b3) update submodules

* [f11baa8](https://github.com/sireum/hamr-codegen/commit/f11baa8) switch to component_language to identify cakeml components

* [f623d5b](https://github.com/sireum/hamr-codegen/commit/f623d5b) update submodules

* [6c5f45a](https://github.com/sireum/hamr-codegen/commit/6c5f45a) update submodules

* [ff19405](https://github.com/sireum/hamr-codegen/commit/ff19405) update submodules

* [f0cb6e6](https://github.com/sireum/hamr-codegen/commit/f0cb6e6) move bit codec checks to resolver, add cakeml checks to resolver, resolve port connections

* [11ea448](https://github.com/sireum/hamr-codegen/commit/11ea448) update submodules

* [a114718](https://github.com/sireum/hamr-codegen/commit/a114718) update build props

* [4913df9](https://github.com/sireum/hamr-codegen/commit/4913df9) update submodules

* [f9f1efa](https://github.com/sireum/hamr-codegen/commit/f9f1efa) update submodules

* [64f0d3e](https://github.com/sireum/hamr-codegen/commit/64f0d3e) Merge branch 'master' into dev

* [9c7554f](https://github.com/sireum/hamr-codegen/commit/9c7554f) check in test util

* [9f55050](https://github.com/sireum/hamr-codegen/commit/9f55050) Updated act, arsit, and art.

* [16cbf81](https://github.com/sireum/hamr-codegen/commit/16cbf81) Removed -n.
</details>
<br>


# [4.20210411.c273380](https://github.com/sireum/kekinian/releases/tag/4.20210411.c273380)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210411.c273380 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [9bf099d](https://github.com/sireum/hamr-codegen/commit/9bf099d) Updated act and arsit.

* [4248140](https://github.com/sireum/hamr-codegen/commit/4248140) Added optional usage description in CliOpt.Tool.

* [f88f26a](https://github.com/sireum/hamr-codegen/commit/f88f26a) update build.cmd to use proyek

* [3096997](https://github.com/sireum/hamr-codegen/commit/3096997) update submodules

* [3afd319](https://github.com/sireum/hamr-codegen/commit/3afd319) Updated act, arsit, and art. [skip ci].

* [a46c71c](https://github.com/sireum/hamr-codegen/commit/a46c71c) Updated project.cmd. [skip ci]

* [288bb0a](https://github.com/sireum/hamr-codegen/commit/288bb0a) Updated act, arsit, and art.

* [78bc371](https://github.com/sireum/hamr-codegen/commit/78bc371) Added m2 publish info.

* [261383b](https://github.com/sireum/hamr-codegen/commit/261383b) Updated art, act, and arsit.

* [6bbbd3b](https://github.com/sireum/hamr-codegen/commit/6bbbd3b) Ignored project.json. [skip ci]

* [7f15c23](https://github.com/sireum/hamr-codegen/commit/7f15c23) Updated line separator.

* [339ffb6](https://github.com/sireum/hamr-codegen/commit/339ffb6) Updated act, arsit, and art.

* [627e95c](https://github.com/sireum/hamr-codegen/commit/627e95c) Updated project.cmd.

* [f5bc897](https://github.com/sireum/hamr-codegen/commit/f5bc897) update submodules

* [ff71b12](https://github.com/sireum/hamr-codegen/commit/ff71b12) update submodules

* [e87017b](https://github.com/sireum/hamr-codegen/commit/e87017b) Merge branch 'master' into dev

* [5b54357](https://github.com/sireum/hamr-codegen/commit/5b54357) refine domain scheduling use detection

* [4a17ef2](https://github.com/sireum/hamr-codegen/commit/4a17ef2) Updated project.cmd.

* [40446b6](https://github.com/sireum/hamr-codegen/commit/40446b6) Added project.cmd.

* [0499d3e](https://github.com/sireum/hamr-codegen/commit/0499d3e) Updated art, act, and arsit.

* [11f0a40](https://github.com/sireum/hamr-codegen/commit/11f0a40) Added project.cmd.

* [d2aea55](https://github.com/sireum/hamr-codegen/commit/d2aea55) Added .gitattributes.

* [2ddf880](https://github.com/sireum/hamr-codegen/commit/2ddf880) update build props

* [a51f083](https://github.com/sireum/hamr-codegen/commit/a51f083) api name changes
</details>
<br>


# [4.20210318.5f39324](https://github.com/sireum/kekinian/releases/tag/4.20210318.5f39324)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210318.5f39324 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [cfee8cd](https://github.com/sireum/hamr-codegen/commit/cfee8cd) update submodules

* [88f4298](https://github.com/sireum/hamr-codegen/commit/88f4298) update arsit

* [5bff736](https://github.com/sireum/hamr-codegen/commit/5bff736) write out arsit resources when transpiler not used

* [30fc314](https://github.com/sireum/hamr-codegen/commit/30fc314) update submodules
</details>
<br>


# [4.20210301.ba1a6ba](https://github.com/sireum/kekinian/releases/tag/4.20210301.ba1a6ba)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210301.ba1a6ba https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [fce64fe](https://github.com/sireum/hamr-codegen/commit/fce64fe) update submodules

* [597a3de](https://github.com/sireum/hamr-codegen/commit/597a3de) update cli

* [8faf001](https://github.com/sireum/hamr-codegen/commit/8faf001) update submodules

* [56f4c4a](https://github.com/sireum/hamr-codegen/commit/56f4c4a) update submodules

* [f7f7e5c](https://github.com/sireum/hamr-codegen/commit/f7f7e5c) fix c aux code dirs option
</details>
<br>


# [4.20210223.a67b618](https://github.com/sireum/kekinian/releases/tag/4.20210223.a67b618)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210223.a67b618 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [202c153](https://github.com/sireum/hamr-codegen/commit/202c153) Updated act.
</details>
<br>


# [4.20210208.5e0cfa7](https://github.com/sireum/kekinian/releases/tag/4.20210208.5e0cfa7)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210208.5e0cfa7 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [0ca75a2](https://github.com/sireum/hamr-codegen/commit/0ca75a2) update submodules

* [913bb33](https://github.com/sireum/hamr-codegen/commit/913bb33) update submodules

* [581710a](https://github.com/sireum/hamr-codegen/commit/581710a) add aadl id

* [fba3d1c](https://github.com/sireum/hamr-codegen/commit/fba3d1c) update submodules

* [9613bd4](https://github.com/sireum/hamr-codegen/commit/9613bd4) update submodules

* [004083b](https://github.com/sireum/hamr-codegen/commit/004083b) update submodules

* [7f0217a](https://github.com/sireum/hamr-codegen/commit/7f0217a) add bts generator
</details>
<br>


# [4.20210120.9bbeb24](https://github.com/sireum/kekinian/releases/tag/4.20210120.9bbeb24)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20210120.9bbeb24 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [6d613ed](https://github.com/sireum/hamr-codegen/commit/6d613ed) update submodules

* [92ac71c](https://github.com/sireum/hamr-codegen/commit/92ac71c) update submodules

* [b610522](https://github.com/sireum/hamr-codegen/commit/b610522) update submodules

* [ba6e762](https://github.com/sireum/hamr-codegen/commit/ba6e762) update dir location

* [d48a74a](https://github.com/sireum/hamr-codegen/commit/d48a74a) update submodules

* [832ca6c](https://github.com/sireum/hamr-codegen/commit/832ca6c) add feature group ids

* [b102db4](https://github.com/sireum/hamr-codegen/commit/b102db4) remove feature group disambiguator

* [4e6096b](https://github.com/sireum/hamr-codegen/commit/4e6096b) update submodules

* [614d346](https://github.com/sireum/hamr-codegen/commit/614d346) switch back to main update site

* [2ad2965](https://github.com/sireum/hamr-codegen/commit/2ad2965) update submodules

* [ae8bb6f](https://github.com/sireum/hamr-codegen/commit/ae8bb6f) update submodules

* [8ffde7c](https://github.com/sireum/hamr-codegen/commit/8ffde7c) update submodules

* [3898d23](https://github.com/sireum/hamr-codegen/commit/3898d23) update submodules

* [9900ff5](https://github.com/sireum/hamr-codegen/commit/9900ff5) update submodules

* [7146adc](https://github.com/sireum/hamr-codegen/commit/7146adc) use sergen friendly type name
</details>
<br>


# [4.20201221.b159c6f](https://github.com/sireum/kekinian/releases/tag/4.20201221.b159c6f)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201221.b159c6f https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [939cca7](https://github.com/sireum/hamr-codegen/commit/939cca7) update submodules

* [0a5c832](https://github.com/sireum/hamr-codegen/commit/0a5c832) update submodules

* [4b3692d](https://github.com/sireum/hamr-codegen/commit/4b3692d) update submodules

* [bb7710b](https://github.com/sireum/hamr-codegen/commit/bb7710b) merge dev

* [53540e6](https://github.com/sireum/hamr-codegen/commit/53540e6) update submodules

* [35d7050](https://github.com/sireum/hamr-codegen/commit/35d7050) update submodules
</details>
<br>


# [4.20201207.0a0c727](https://github.com/sireum/kekinian/releases/tag/4.20201207.0a0c727)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201207.0a0c727 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [2907889](https://github.com/sireum/hamr-codegen/commit/2907889) update submodules

* [19a3d49](https://github.com/sireum/hamr-codegen/commit/19a3d49) update submodules

* [426a116](https://github.com/sireum/hamr-codegen/commit/426a116) c bit-codec helper api
</details>
<br>


# [4.20201203.6ebbf64](https://github.com/sireum/kekinian/releases/tag/4.20201203.6ebbf64)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201203.6ebbf64 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [a5d290a](https://github.com/sireum/hamr-codegen/commit/a5d290a) update submodules

* [f5aab84](https://github.com/sireum/hamr-codegen/commit/f5aab84) update submodules

* [de181c5](https://github.com/sireum/hamr-codegen/commit/de181c5) update submodules

* [d39303d](https://github.com/sireum/hamr-codegen/commit/d39303d) reporter fix
</details>
<br>


# [4.20201120.058f5f5](https://github.com/sireum/kekinian/releases/tag/4.20201120.058f5f5)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201120.058f5f5 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e3f23ca](https://github.com/sireum/hamr-codegen/commit/e3f23ca) Updated arsit.

* [5ef626b](https://github.com/sireum/hamr-codegen/commit/5ef626b) Updated CI configs.

* [f80bb2e](https://github.com/sireum/hamr-codegen/commit/f80bb2e) Update main.yml
</details>
<br>


# [4.20201116.092a7d9](https://github.com/sireum/kekinian/releases/tag/4.20201116.092a7d9)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201116.092a7d9 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [ffd6ffd](https://github.com/sireum/hamr-codegen/commit/ffd6ffd) Updated arsit.

* [4c44151](https://github.com/sireum/hamr-codegen/commit/4c44151) Adapted to Os.Proc changes.

* [e001da7](https://github.com/sireum/hamr-codegen/commit/e001da7) update submodules

* [da12cbd](https://github.com/sireum/hamr-codegen/commit/da12cbd) update submodules

* [65972d0](https://github.com/sireum/hamr-codegen/commit/65972d0) move testos to common
</details>
<br>


# [4.20201109.deb4e5d](https://github.com/sireum/kekinian/releases/tag/4.20201109.deb4e5d)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201109.deb4e5d https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [e557518](https://github.com/sireum/hamr-codegen/commit/e557518) add testApi name

* [392bd2f](https://github.com/sireum/hamr-codegen/commit/392bd2f) update submodules

* [5cdb8b5](https://github.com/sireum/hamr-codegen/commit/5cdb8b5) update submodules

* [9f25d71](https://github.com/sireum/hamr-codegen/commit/9f25d71) move transformers to common

* [e9fc8b7](https://github.com/sireum/hamr-codegen/commit/e9fc8b7) update submodules

* [29af596](https://github.com/sireum/hamr-codegen/commit/29af596) update submodules

* [2b29a98](https://github.com/sireum/hamr-codegen/commit/2b29a98) update submodules

* [b3097dd](https://github.com/sireum/hamr-codegen/commit/b3097dd) update submodules

* [68dd86b](https://github.com/sireum/hamr-codegen/commit/68dd86b) update submodules

* [e14a116](https://github.com/sireum/hamr-codegen/commit/e14a116) update submodules

* [22b1c44](https://github.com/sireum/hamr-codegen/commit/22b1c44) remove printlns

* [d615108](https://github.com/sireum/hamr-codegen/commit/d615108) add pca-pump test case
</details>
<br>


# [4.20201009.ec05fb7](https://github.com/sireum/kekinian/releases/tag/4.20201009.ec05fb7)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20201009.ec05fb7 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [3b80f21](https://github.com/sireum/hamr-codegen/commit/3b80f21) update submodules

* [48e816c](https://github.com/sireum/hamr-codegen/commit/48e816c) remove space

* [bb2c988](https://github.com/sireum/hamr-codegen/commit/bb2c988) tipe fix, update tests

* [fd3d512](https://github.com/sireum/hamr-codegen/commit/fd3d512) Revert "move resources to resolve compilation issue"

* [8d6add7](https://github.com/sireum/hamr-codegen/commit/8d6add7) move resources to resolve compilation issue

* [719251b](https://github.com/sireum/hamr-codegen/commit/719251b) update submodules

* [9af2fcc](https://github.com/sireum/hamr-codegen/commit/9af2fcc) update submodules

* [11f2f74](https://github.com/sireum/hamr-codegen/commit/11f2f74) rename resource

* [07c822e](https://github.com/sireum/hamr-codegen/commit/07c822e) update submodules

* [ccf7a0a](https://github.com/sireum/hamr-codegen/commit/ccf7a0a) singleton

* [7e90c06](https://github.com/sireum/hamr-codegen/commit/7e90c06) update submodules

* [517d2f6](https://github.com/sireum/hamr-codegen/commit/517d2f6) update submodules

* [c2a71f5](https://github.com/sireum/hamr-codegen/commit/c2a71f5) update submodules
</details>
<br>


# [4.20200915.72510af](https://github.com/sireum/kekinian/releases/tag/4.20200915.72510af)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20200915.72510af https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [55f0f3c](https://github.com/sireum/hamr-codegen/commit/55f0f3c) point to 2.13 update site

* [08491d7](https://github.com/sireum/hamr-codegen/commit/08491d7) scala fix

* [9fa1e52](https://github.com/sireum/hamr-codegen/commit/9fa1e52) update arsit

* [d62848d](https://github.com/sireum/hamr-codegen/commit/d62848d) Merge branch 'master' into dev

* [b745159](https://github.com/sireum/hamr-codegen/commit/b745159) update submodules
</details>
<br>


# [4.20200902.3184e0c](https://github.com/sireum/kekinian/releases/tag/4.20200902.3184e0c)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20200902.3184e0c https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [f0e4ad0](https://github.com/sireum/hamr-codegen/commit/f0e4ad0) update submodules

* [b50ab45](https://github.com/sireum/hamr-codegen/commit/b50ab45) Updated submodules.

* [974aa9f](https://github.com/sireum/hamr-codegen/commit/974aa9f) Updated act, arsit, and art.

* [3d2592e](https://github.com/sireum/hamr-codegen/commit/3d2592e) Removed obsolete Slang Runner -s option.
</details>
<br>


# [4.20200829.aeb1500](https://github.com/sireum/kekinian/releases/tag/4.20200829.aeb1500)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20200829.aeb1500 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [29a2a56](https://github.com/sireum/hamr-codegen/commit/29a2a56) Updated art.

* [7a312ef](https://github.com/sireum/hamr-codegen/commit/7a312ef) update submodules

* [e15e463](https://github.com/sireum/hamr-codegen/commit/e15e463) update submodules

* [f142e24](https://github.com/sireum/hamr-codegen/commit/f142e24) Update README.md

* [2de66ad](https://github.com/sireum/hamr-codegen/commit/2de66ad) Update main.yml

* [a5a36b0](https://github.com/sireum/hamr-codegen/commit/a5a36b0) update submodules

* [1d328ce](https://github.com/sireum/hamr-codegen/commit/1d328ce) update submodules

* [63f767f](https://github.com/sireum/hamr-codegen/commit/63f767f) path util

* [047e69d](https://github.com/sireum/hamr-codegen/commit/047e69d) full hash

* [6642c2b](https://github.com/sireum/hamr-codegen/commit/6642c2b) test util

* [4482e1b](https://github.com/sireum/hamr-codegen/commit/4482e1b) tipe fix

* [9ff832f](https://github.com/sireum/hamr-codegen/commit/9ff832f) write contents when transpiling

* [cd3619b](https://github.com/sireum/hamr-codegen/commit/cd3619b) update submodules

* [399e413](https://github.com/sireum/hamr-codegen/commit/399e413) clean subprojects

* [1ae7dbd](https://github.com/sireum/hamr-codegen/commit/1ae7dbd) update submodules

* [2f76c11](https://github.com/sireum/hamr-codegen/commit/2f76c11) move resource

* [6d66fa6](https://github.com/sireum/hamr-codegen/commit/6d66fa6) update submodules

* [30024d5](https://github.com/sireum/hamr-codegen/commit/30024d5) Update README.md

* [95216d2](https://github.com/sireum/hamr-codegen/commit/95216d2) Update main.yml

* [a6fe3d3](https://github.com/sireum/hamr-codegen/commit/a6fe3d3) Update main.yml

* [25b5de2](https://github.com/sireum/hamr-codegen/commit/25b5de2) update submodules

* [569f99a](https://github.com/sireum/hamr-codegen/commit/569f99a) cleanup

* [0259aa4](https://github.com/sireum/hamr-codegen/commit/0259aa4) update submodules

* [2c47205](https://github.com/sireum/hamr-codegen/commit/2c47205) update submodules

* [3e4ac6e](https://github.com/sireum/hamr-codegen/commit/3e4ac6e) update submodules

* [c5ad267](https://github.com/sireum/hamr-codegen/commit/c5ad267) update submodules

* [c12266f](https://github.com/sireum/hamr-codegen/commit/c12266f) udpate submodules

* [87e1d0b](https://github.com/sireum/hamr-codegen/commit/87e1d0b) update submodules

* [2dd40c7](https://github.com/sireum/hamr-codegen/commit/2dd40c7) remove dev

* [1078b18](https://github.com/sireum/hamr-codegen/commit/1078b18) Update README.md

* [9025430](https://github.com/sireum/hamr-codegen/commit/9025430) Create README.md

* [4458ccf](https://github.com/sireum/hamr-codegen/commit/4458ccf) Update main.yml

* [5d6e5f7](https://github.com/sireum/hamr-codegen/commit/5d6e5f7) Create main.yml

* [e322454](https://github.com/sireum/hamr-codegen/commit/e322454) cleanup

* [9df8574](https://github.com/sireum/hamr-codegen/commit/9df8574) cleanup

* [41c578b](https://github.com/sireum/hamr-codegen/commit/41c578b) cleanup

* [76cc4e8](https://github.com/sireum/hamr-codegen/commit/76cc4e8) update submodules

* [55d9186](https://github.com/sireum/hamr-codegen/commit/55d9186) update submodules

* [b0a5b72](https://github.com/sireum/hamr-codegen/commit/b0a5b72) update submodules

* [d661ec9](https://github.com/sireum/hamr-codegen/commit/d661ec9) update act

* [79adbb2](https://github.com/sireum/hamr-codegen/commit/79adbb2) change cli desc

* [4349e93](https://github.com/sireum/hamr-codegen/commit/4349e93) Merge branch 'dev'

* [55659c6](https://github.com/sireum/hamr-codegen/commit/55659c6) update submodules

* [7ae9900](https://github.com/sireum/hamr-codegen/commit/7ae9900) update submodules

* [dfd91d9](https://github.com/sireum/hamr-codegen/commit/dfd91d9) update submodules

* [fc4edba](https://github.com/sireum/hamr-codegen/commit/fc4edba) t

* [d73a20f](https://github.com/sireum/hamr-codegen/commit/d73a20f) t

* [5b90455](https://github.com/sireum/hamr-codegen/commit/5b90455) add experimental options

* [f1d253a](https://github.com/sireum/hamr-codegen/commit/f1d253a) update submodules

* [96ac594](https://github.com/sireum/hamr-codegen/commit/96ac594) Merge branch 'dev'

* [a02dbf5](https://github.com/sireum/hamr-codegen/commit/a02dbf5) update submodules

* [1f1c450](https://github.com/sireum/hamr-codegen/commit/1f1c450) Merge branch 'dev'

* [6d7ad88](https://github.com/sireum/hamr-codegen/commit/6d7ad88) update submodules

* [f136e7c](https://github.com/sireum/hamr-codegen/commit/f136e7c) Merge branch 'dev'

* [b0bfa4c](https://github.com/sireum/hamr-codegen/commit/b0bfa4c) update submodules

* [81c2dab](https://github.com/sireum/hamr-codegen/commit/81c2dab) update submdoules

* [c62d228](https://github.com/sireum/hamr-codegen/commit/c62d228) update submodules

* [02d8a6b](https://github.com/sireum/hamr-codegen/commit/02d8a6b) update act

* [e79c9be](https://github.com/sireum/hamr-codegen/commit/e79c9be) add cakeml detection

* [f7094f6](https://github.com/sireum/hamr-codegen/commit/f7094f6) Merge branch 'dev'

* [b4dee0d](https://github.com/sireum/hamr-codegen/commit/b4dee0d) update submodules

* [1b144ed](https://github.com/sireum/hamr-codegen/commit/1b144ed) Merge branch 'dev'

* [db02b59](https://github.com/sireum/hamr-codegen/commit/db02b59) ffi fixes

* [60576d5](https://github.com/sireum/hamr-codegen/commit/60576d5) Merge branch 'dev'

* [896340e](https://github.com/sireum/hamr-codegen/commit/896340e) update submodules

* [e72afe8](https://github.com/sireum/hamr-codegen/commit/e72afe8) update submodules

* [f65b875](https://github.com/sireum/hamr-codegen/commit/f65b875) update submodules

* [bada908](https://github.com/sireum/hamr-codegen/commit/bada908) correct macro usage

* [d5a3aab](https://github.com/sireum/hamr-codegen/commit/d5a3aab) update arsit

* [e5b3132](https://github.com/sireum/hamr-codegen/commit/e5b3132) update arsit

* [3dc7516](https://github.com/sireum/hamr-codegen/commit/3dc7516) update submodules

* [99859df](https://github.com/sireum/hamr-codegen/commit/99859df) add cakeml ffis

* [b58d923](https://github.com/sireum/hamr-codegen/commit/b58d923) update submodules

* [ffa213b](https://github.com/sireum/hamr-codegen/commit/ffa213b) update submodules

* [6d8b70b](https://github.com/sireum/hamr-codegen/commit/6d8b70b) update submodules
</details>
<br>


# [4.20200614.8787cf0](https://github.com/sireum/kekinian/releases/tag/4.20200614.8787cf0)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20200614.8787cf0 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [935e92e](https://github.com/sireum/hamr-codegen/commit/935e92e) update submodules

* [62684e9](https://github.com/sireum/hamr-codegen/commit/62684e9) update submodules

* [0e5d821](https://github.com/sireum/hamr-codegen/commit/0e5d821) t

* [1f2190f](https://github.com/sireum/hamr-codegen/commit/1f2190f) t

* [7193448](https://github.com/sireum/hamr-codegen/commit/7193448) update submodules

* [d23efb0](https://github.com/sireum/hamr-codegen/commit/d23efb0) remove message queue

* [e216b86](https://github.com/sireum/hamr-codegen/commit/e216b86) update submodules

* [bfa1057](https://github.com/sireum/hamr-codegen/commit/bfa1057) update submodules

* [52609a1](https://github.com/sireum/hamr-codegen/commit/52609a1) add vm support

* [b23ef20](https://github.com/sireum/hamr-codegen/commit/b23ef20) t

* [d17cfd5](https://github.com/sireum/hamr-codegen/commit/d17cfd5) update submodules

* [d6024e3](https://github.com/sireum/hamr-codegen/commit/d6024e3) update submodules

* [334662c](https://github.com/sireum/hamr-codegen/commit/334662c) update submodules

* [7acea4d](https://github.com/sireum/hamr-codegen/commit/7acea4d) update submodules

* [87f4075](https://github.com/sireum/hamr-codegen/commit/87f4075) update submodules

* [3f63da2](https://github.com/sireum/hamr-codegen/commit/3f63da2) update build.properties

* [ac7c535](https://github.com/sireum/hamr-codegen/commit/ac7c535) add common object - byte arrays

* [cbb5206](https://github.com/sireum/hamr-codegen/commit/cbb5206) location change

* [febe2d4](https://github.com/sireum/hamr-codegen/commit/febe2d4) update submodules

* [1b988b5](https://github.com/sireum/hamr-codegen/commit/1b988b5) update act

* [32f594b](https://github.com/sireum/hamr-codegen/commit/32f594b) update submodules

* [9ae2714](https://github.com/sireum/hamr-codegen/commit/9ae2714) update submodules

* [a0c9c33](https://github.com/sireum/hamr-codegen/commit/a0c9c33) update submodules

* [9acae0b](https://github.com/sireum/hamr-codegen/commit/9acae0b) update submodules

* [76c3599](https://github.com/sireum/hamr-codegen/commit/76c3599) update arsit

* [42fb015](https://github.com/sireum/hamr-codegen/commit/42fb015) update submodules

* [3a21919](https://github.com/sireum/hamr-codegen/commit/3a21919) update submodules

* [5bc43f5](https://github.com/sireum/hamr-codegen/commit/5bc43f5) update submodules

* [7a1c15d](https://github.com/sireum/hamr-codegen/commit/7a1c15d) update submodules

* [7c26017](https://github.com/sireum/hamr-codegen/commit/7c26017) update submodules

* [22063c9](https://github.com/sireum/hamr-codegen/commit/22063c9) update submodules

* [6bf9ae6](https://github.com/sireum/hamr-codegen/commit/6bf9ae6) x

* [33a6d51](https://github.com/sireum/hamr-codegen/commit/33a6d51) x

* [1785adc](https://github.com/sireum/hamr-codegen/commit/1785adc) update submodules

* [d76a182](https://github.com/sireum/hamr-codegen/commit/d76a182) update tests

* [951deca](https://github.com/sireum/hamr-codegen/commit/951deca) update submodules

* [431e895](https://github.com/sireum/hamr-codegen/commit/431e895) x

* [204c054](https://github.com/sireum/hamr-codegen/commit/204c054) update submodules

* [5057ace](https://github.com/sireum/hamr-codegen/commit/5057ace) use slang run

* [1e4953d](https://github.com/sireum/hamr-codegen/commit/1e4953d) update runtime

* [86e1176](https://github.com/sireum/hamr-codegen/commit/86e1176) update submodules

* [12eac33](https://github.com/sireum/hamr-codegen/commit/12eac33) update submodules

* [b022c8d](https://github.com/sireum/hamr-codegen/commit/b022c8d) props from file

* [801f3b8](https://github.com/sireum/hamr-codegen/commit/801f3b8) update submodules

* [e8d1646](https://github.com/sireum/hamr-codegen/commit/e8d1646) add art as submodule

* [6beca8b](https://github.com/sireum/hamr-codegen/commit/6beca8b) add mill

* [e9d49ca](https://github.com/sireum/hamr-codegen/commit/e9d49ca) update submodules

* [c891881](https://github.com/sireum/hamr-codegen/commit/c891881) update submodules

* [0a95e36](https://github.com/sireum/hamr-codegen/commit/0a95e36) update submodules, add bin dir

* [52b890a](https://github.com/sireum/hamr-codegen/commit/52b890a) slash script
</details>
<br>


# [4.20200214.5d56812](https://github.com/sireum/kekinian/releases/tag/4.20200214.5d56812)

<details><summary>How to build</summary>

```
git clone --rec --depth 1 --branch 4.20200214.5d56812 https://github.com/sireum/kekinian
cd kekinian
bin/build.cmd
```

</details>

<details><summary>Commits</summary>

* [60d36de](https://github.com/sireum/hamr-codegen/commit/60d36de) update submodules

* [a9e444c](https://github.com/sireum/hamr-codegen/commit/a9e444c) update submodules

* [5f84ed1](https://github.com/sireum/hamr-codegen/commit/5f84ed1) update submodules

* [2aa4f7b](https://github.com/sireum/hamr-codegen/commit/2aa4f7b) update submodules

* [2e3de08](https://github.com/sireum/hamr-codegen/commit/2e3de08) update tests

* [821d64f](https://github.com/sireum/hamr-codegen/commit/821d64f) update submodules

* [a3dbd03](https://github.com/sireum/hamr-codegen/commit/a3dbd03) Updated act.

* [992a7a8](https://github.com/sireum/hamr-codegen/commit/992a7a8) update tests

* [6b455ff](https://github.com/sireum/hamr-codegen/commit/6b455ff) update sumodules

* [cd3805f](https://github.com/sireum/hamr-codegen/commit/cd3805f) Build module tweak.

* [2c804e0](https://github.com/sireum/hamr-codegen/commit/2c804e0) Updated publish module names.

* [47bdaab](https://github.com/sireum/hamr-codegen/commit/47bdaab) update submodules

* [abb7d99](https://github.com/sireum/hamr-codegen/commit/abb7d99) model cleanup

* [640cf6c](https://github.com/sireum/hamr-codegen/commit/640cf6c) update tests

* [545e889](https://github.com/sireum/hamr-codegen/commit/545e889) update submodules

* [996248d](https://github.com/sireum/hamr-codegen/commit/996248d) update submodules

* [2490601](https://github.com/sireum/hamr-codegen/commit/2490601) update expected

* [3beb0ca](https://github.com/sireum/hamr-codegen/commit/3beb0ca) update submodules

* [b7e121a](https://github.com/sireum/hamr-codegen/commit/b7e121a) update expected

* [67d35fa](https://github.com/sireum/hamr-codegen/commit/67d35fa) update tests

* [82b12ab](https://github.com/sireum/hamr-codegen/commit/82b12ab) update act

* [6718489](https://github.com/sireum/hamr-codegen/commit/6718489) support programming prop

* [e7178aa](https://github.com/sireum/hamr-codegen/commit/e7178aa) add act/arsit as submods

* [733cdca](https://github.com/sireum/hamr-codegen/commit/733cdca) update test submodule

* [29ecdd6](https://github.com/sireum/hamr-codegen/commit/29ecdd6) initial
</details>
<br>


