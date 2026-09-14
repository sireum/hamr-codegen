import os
import re
import sys

# script detects test suites that the platform github actions don't run. Every ScalaTest
# suite under the test submodule has to be named by each platform workflow -- either in a
# matrix's testSuffix rows or in a direct '--suffixes' invocation -- otherwise it silently
# stops being exercised there. R2U2MonitorTests, R2U2MonitorBehaviorTests,
# SymlinkAuxCodeTest and SlangExp2RustTests were all unrun for exactly that reason.

TEST_SRC = os.path.join("jvm", "src", "test", "scala")
WORKFLOW_DIR = os.path.join(".github", "workflows")

# the workflows expected to run the full set of suites
PLATFORMS = ("CI_linux.yml", "CI-macOS.yml", "CI-windows.yml")

# exempts a suite from every platform
ALL = "*"

# suites deliberately not run, keyed by suite and then by the platform exempted, with the
# reason. A suite exempted via ALL may still be run by a non-platform workflow -- the report
# shows where each suite actually runs.
EXCLUDED = {
    "VersionCheck": {
        ALL: "gated on TestUtil.inIVE, so it registers a no-op outside the IVE",
    },
    "MicroRosTests": {
        ALL: "run by its own path-triggered CI-microROS.yml",
    },
    "SymlinkAuxCodeTest": {
        "CI-windows.yml": "Os_Ext.mklink makes an NTFS junction for directory targets on "
                          "Windows, which Files.isSymbolicLink reports as false",
    },
}

# a class is treated as a test suite when its extends clause names one of these
SUITE_BASES = ("TestSuite", "CodegenTest", "CodegenBehaviorTest", "CodegenTestSuite")

CLASS = re.compile(r"^class\s+(\w+)\s+extends\s+([^{]*)", re.M)
MATRIX_KEY = re.compile(r"^\s*testSuffix:\s*$")
MATRIX_ROW = re.compile(r"^\s*-\s*\"([^\"]*)\"\s*$")
SUFFIXES_ARG = re.compile(r"--suffixes\s+([\w,]+)")


def findSuites():
    """Maps each test suite's simple name to the file that declares it."""
    suites = {}
    for root, _, files in os.walk(TEST_SRC):
        for f in files:
            if not f.endswith(".scala"):
                continue
            path = os.path.join(root, f)
            with open(path, encoding="utf-8", errors="replace") as fh:
                for name, bases in CLASS.findall(fh.read()):
                    if any(b in bases for b in SUITE_BASES):
                        suites[name] = path
    return suites


def findListed():
    """Maps each workflow to the set of names it passes to --suffixes."""
    listed = {}
    for f in sorted(os.listdir(WORKFLOW_DIR)):
        if not (f.endswith(".yml") or f.endswith(".yaml")):
            continue
        tokens = set()
        inMatrix = False
        for line in open(os.path.join(WORKFLOW_DIR, f), encoding="utf-8").read().splitlines():
            if MATRIX_KEY.match(line):
                inMatrix = True
                continue
            if inMatrix:
                row = MATRIX_ROW.match(line)
                if row:
                    tokens |= {t.strip() for t in row.group(1).split(",")}
                    continue
                if line.strip() and not line.strip().startswith("#"):
                    inMatrix = False
            for m in SUFFIXES_ARG.finditer(line):
                tokens |= {t.strip() for t in m.group(1).split(",")}
        if tokens:
            listed[f] = tokens
    return listed


def exemption(suite, platform):
    """The reason suite is exempt on platform, or None if it is required there."""
    entry = EXCLUDED.get(suite)
    if entry is None:
        return None
    return entry.get(ALL) or entry.get(platform)


def main():
    suites = findSuites()
    listed = findListed()
    if not suites:
        print(f"::error::no test suites found under {TEST_SRC} -- is the test submodule checked out?")
        return 1

    # proyek matches --suffixes entries against the tail of a class name
    runsOn = dict((name, {wf for wf, ts in listed.items() if any(name.endswith(t) for t in ts)})
                  for name in suites)

    failed = False

    for platform in PLATFORMS:
        if platform not in listed:
            print(f"::error::{platform} names no test suites -- expected a testSuffix matrix or a --suffixes call")
            failed = True
            continue
        for name in sorted(suites):
            if platform in runsOn[name] or exemption(name, platform):
                continue
            print(f"::error file={suites[name]}::{name} is not run by {platform}. Add it to that "
                  f"matrix, or exempt it for {platform} in EXCLUDED in this script with the reason.")
            failed = True

    for wf in sorted(listed):
        for token in sorted(listed[wf]):
            if not any(name.endswith(token) for name in suites):
                print(f"::error::'{token}' is passed to --suffixes by {wf} but matches no test "
                      f"suite -- renamed or misspelled?")
                failed = True

    for name in sorted(EXCLUDED):
        if name not in suites:
            print(f"::warning::{name} is listed in EXCLUDED but no longer exists -- drop it from this script")
            continue
        for platform in sorted(EXCLUDED[name]):
            if platform != ALL and platform not in PLATFORMS:
                print(f"::warning::{name} is exempted for '{platform}', which is not one of {', '.join(PLATFORMS)}")
            elif platform != ALL and platform in runsOn[name]:
                print(f"::warning::{name} is exempted for {platform} but that workflow runs it anyway")

    labels = [p.replace("CI_", "").replace("CI-", "").replace(".yml", "") for p in PLATFORMS]
    width = max(len(n) for n in suites)
    print(f"\n{len(suites)} test suites across {len(PLATFORMS)} platform workflows\n")
    print(f"  {'':{width}}  " + "  ".join(f"{l:>7s}" for l in labels) + "   also run by")
    for name in sorted(suites):
        cells = []
        for platform in PLATFORMS:
            cells.append("ok" if platform in runsOn[name] else ("skip" if exemption(name, platform) else "MISS"))
        others = sorted(runsOn[name] - set(PLATFORMS))
        print(f"  {name:{width}}  " + "  ".join(f"{c:>7s}" for c in cells) + "   " + ", ".join(others))

    for name in sorted(EXCLUDED):
        for platform, reason in sorted(EXCLUDED[name].items()):
            scope = "all platforms" if platform == ALL else platform
            print(f"\n  {name} skipped on {scope}: {reason}")

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
