import os
import urllib.request
import json
import sys

# script detects when a github action that is scheduled to run periodically via cron
# is disabled due to inactivity
REPOS_TO_CHECK = [
    # https://github.com/santoslab/isolette-artifacts/actions
    ("santoslab", "isolette-artifacts", "CI-macOS.yml"),
    ("santoslab", "isolette-artifacts", "CI-windows.yml"),
    ("santoslab", "isolette-artifacts", "CI_linux.yml"),    
    # https://github.com/santoslab/hamr-examples/actions
    ("santoslab", "hamr-examples", "CI-macOS.yml"),
    ("santoslab", "hamr-examples", "CI-windows.yml"),
    ("santoslab", "hamr-examples", "CI_linux.yml"),
    # https://github.com/santoslab/sysmlv2-models/actions
    ("santoslab", "sysmlv2-models", "CI-camkes.yml"),
    ("santoslab", "sysmlv2-models", "CI-linux.yml"),
    ("santoslab", "sysmlv2-models", "CI-macOS.yml"),
    ("santoslab", "hamr-examples", "CI-windows.yml"),
    # https://github.com/santoslab/hamr-tutorials/actions
    ("santoslab", "hamr-tutorials", "ci-linux.yml"),
    ("santoslab", "hamr-tutorials", "ci-mac.yml"),
    ("santoslab", "hamr-tutorials", "ci-root.yml"),
    ("santoslab", "hamr-tutorials", "ci-windows.yml"),    
    # https://github.com/santoslab/gumbox-case-studies/actions
    ("santoslab", "gumbox-case-studies", "CI-macOS.yml"),
    ("santoslab", "gumbox-case-studies", "CI-windows.yml"),
    ("santoslab", "gumbox-case-studies", "CI_linux.yml"),
    # https://github.com/santoslab/rts-showcase/actions
    ("santoslab", "rts-showcase", "CI-camkes.yml"),
    ("santoslab", "rts-showcase", "CI-macOS.yml"),
    ("santoslab", "rts-showcase", "CI-windows.yml"),
    ("santoslab", "rts-showcase", "CI_linux.yml"),
    # https://github.com/loonwerks/INSPECTA-models/actions
    ("loonwerks", "INSPECTA-models", "ci-hamr.yml"),
]

GITHUB_TOKEN = os.getenv("GITHUB_TOKEN")

if not GITHUB_TOKEN:
    print("❌ GITHUB_TOKEN environment variable is not set.")
    sys.exit(1)

had_problems = False

# === Helpers ===
def check_workflow(owner, repo, workflow_filename):
    global had_problems
    
    url = f"https://api.github.com/repos/{owner}/{repo}/actions/workflows"
    req = urllib.request.Request(url)
    req.add_header("Authorization", f"Bearer {GITHUB_TOKEN}")
    req.add_header("Accept", "application/vnd.github+json")

    try:
        with urllib.request.urlopen(req) as response:
            if response.status != 200:
                print(f"❌ [{owner}/{repo}] Failed to fetch workflows. Status: {response.status}")
                had_problems = True
                return

            data = json.load(response)
            workflows = data.get("workflows", [])

            matched = next((wf for wf in workflows if wf.get("path", "").endswith(workflow_filename)), None)

            if matched:
                state = matched.get("state")
                name = matched.get("name")
                if state.startswith("disabled"):
                    print(f"⚠️ [{owner}/{repo}] Workflow '{name}' is DISABLED (state: {state})")
                    had_problems = True
                else:
                    print(f"✅ [{owner}/{repo}] Workflow '{name}' is enabled (state: {state})")
            else:
                print(f"❌ [{owner}/{repo}] No workflow found ending in '{workflow_filename}'.")
                had_problems = True

    except urllib.error.HTTPError as e:
        print(f"❌ [{owner}/{repo}] HTTP error: {e.code} {e.reason}")
        had_problems = True
    except urllib.error.URLError as e:
        print(f"❌ [{owner}/{repo}] URL error: {e.reason}")
        had_problems = True
    except Exception as e:
        print(f"❌ [{owner}/{repo}] Unexpected error: {e}")
        had_problems = True

# === Main ===
def main():
    for (owner, repo, workflow_filename) in REPOS_TO_CHECK:
        check_workflow(owner, repo, workflow_filename)

    if had_problems:
        sys.exit(1)
    else:
        sys.exit(0)
    
if __name__ == "__main__":
    main()
