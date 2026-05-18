# PR Workflow

This workflow defines the shared pre-PR process for humans and AI agents.
It is intentionally agent-neutral: use these instructions from Codex, Claude,
GitHub Copilot, a terminal, or any future assistant.

## Entry Points

- **Prepare PR**: make the branch ready. This may modify files.
- **Check PR**: audit readiness and report. This must not intentionally modify files.
- **Publish PR**: commit, push, and create or update the GitHub PR after approval.

Use the helper commands from the repository root:

```sh
dev/prepare-pr
dev/check-pr
dev/publish-pr
```

The helper scripts automate repeatable command-line work. They do not replace
AI judgment. An agent running this workflow must still inspect the diff, apply
the style and documentation rules from `AGENTS.md`, and report the result in
chat.

## Prepare PR

Run this when the user asks to prepare work for a PR or make the branch ready.
This stage may edit files.

### Prepare Scope

1. Inspect the current branch and changed files with Git before editing.
2. Classify every changed file into one or more scopes:
   - R package code: `R/`, `src/`
   - R tests and snapshots: `tests/testthat/`, `tests/testthat/_snaps/`
   - R documentation: roxygen blocks, `man/`, `README.Rmd`, vignettes, examples
   - R package metadata: `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `.Rbuildignore`
   - Python wrapper code: `python/src/`
   - Python tests and snapshots: `python/tests/`
   - Python documentation and metadata: `python/README.md`, `python/CHANGELOG.md`, `python/pyproject.toml`,
     `python/uv.lock`
   - CI and development tooling: `.github/`, `.vscode/`, `AGENTS.md`, `dev/`, `inst/devel/`
3. State the detected scope in chat before making non-mechanical edits if the scope is unclear or broad.

### Mechanical Preparation

Run `dev/prepare-pr` for mechanical readiness edits:

- style changed R-like files with `styler`
- run `devtools::document()` when R package files indicate documentation may need regeneration
- format Python code with `ruff format` when Python files changed

After running it, inspect and summarize any files modified by formatting or documentation generation.

### Required AI Review And Edits

After mechanical preparation, review the diff against `AGENTS.md`. Do not duplicate those rules here; use these
sections as the detailed specification:

- `R Package Conventions` for R package code, Rcpp-facing changes, documentation, output, performance, and tests.
- `Local R Test Workflow` for snapshot-safe R testing and snapshot review.
- `Python Wrapper (shaprpy) Conventions` for Python wrapper code.
- `PR Readiness Checklist` for the final cross-check before publishing.

Apply relevant edits instead of only reporting them when the fix is straightforward and within scope. Typical
prepare-stage edits include:

- updating `NEWS.md` for R-facing or developer-facing changes
- updating `python/CHANGELOG.md` for Python-facing changes
- updating README, examples, or vignettes when public usage changed
- updating `DESCRIPTION` or `python/pyproject.toml` versions only for release/version-preparation work or when the user
  explicitly requests it
- inspecting snapshot diffs and asking before accepting, deleting, or replacing generated snapshots
- asking whether to run `Rscript dev/rebuild-long-running-vignettes.R --webp` when executable vignette sources
  (`vignettes/*.Rmd.orig`) changed; this can be time-consuming and is not part of the default workflow

### Prepare Report

Stop after the preparation edits and report in chat:

- detected scope
- commands run
- files changed by preparation
- NEWS/changelog/version edits made or intentionally skipped
- snapshot decisions still needed
- whether long-running vignettes were changed and whether rebuild was run or deferred
- recommended next step

Ask whether the user wants to run `dev/check-pr` next. Do not run it automatically unless the user requested a full
prepare-and-check workflow in the original instruction.

Do not commit, push, create a PR, update a PR title/body, or accept snapshots unless the user explicitly asks.

## Check PR

Run this when the user asks whether a branch is ready, asks for a pre-PR audit,
or asks for a status report. This stage should be read-only.

1. Run `dev/check-pr`.
2. Inspect the diff manually against `AGENTS.md`.
3. Report in chat using this structure:

```md
## PR Readiness Report

Branch: `<branch>`
Base: `<base>`
Scope: R / Python / docs / tests / snapshots / tooling

### Passed
- ...

### Failed
- ...

### Needs Attention
- ...

### Not Run
- ...

### Files Changed By This Workflow
- ...

### Recommendation
...
```

Optional checks require explicit user approval before running:

- `dev/check-pr --with-r-tests`
- `dev/check-pr --with-r-check`
- `dev/check-pr --with-python-localonly`
- `dev/check-pr --check-updates`

When vignettes changed, `dev/check-pr` also scans rendered vignette `.Rmd` files for common embedded R execution
errors. This is a lightweight guard only; it does not replace rebuilding the long-running vignettes when the source
changes require it.

## Publish PR

Run this only when the user asks to publish, push, create a PR, or update an
existing PR.

1. Run `dev/check-pr` or confirm that a recent check report exists.
2. Inspect the current branch and Git status.
3. Commit changes only after the user approves the exact scope and commit message.
4. **Before pushing any commits**, present a summary of what will be pushed (commits and files) and ask the user
   to explicitly confirm. Do not push until the user says yes.
5. Run `dev/publish-pr` to push and create or update the PR.
6. If an open PR already exists for the branch:
   - reuse it
   - push new commits to the existing branch
   - review whether the PR title still matches the branch contents
   - review whether the PR description needs a summary, test updates, changelog notes, or changed risk notes
   - **Before updating the PR title/body**, show the proposed changes and ask the user to confirm.
7. If no PR exists:
   - draft the title/body from the branch name, commit summary, and latest readiness report
   - **Before creating the PR**, show the draft title and body and ask the user to confirm.
   - create a draft PR by default; ask the user before creating a non-draft PR
8. Report the final PR URL and exactly what changed on GitHub.

## Version And Changelog Guidance

Do not bump versions mechanically for every PR.

- `DESCRIPTION` should usually stay at the current development version during normal development.
- Bump the R package version only when preparing a release, beginning a new development cycle, or when the user asks.
- `python/pyproject.toml` should usually stay unchanged during normal development.
- Bump the Python package version only when preparing a Python package release or when the user asks.
- `NEWS.md` and `python/CHANGELOG.md` should describe **significant** user-facing changes, developer-facing workflow
  changes, bug fixes, and notable compatibility changes. Aim for one entry per meaningful change, not one entry per
  file touched. Omit internal refactors, config tweaks, and mechanical moves that do not affect how contributors or
  users interact with the package.
- When the PR number is not yet known, end each new bullet with `(branch: <branch-name>)` as a placeholder
  (e.g., `(branch: my-feature-branch)`). This uniquely identifies which bullets belong to the same future PR
  and makes them easy to find and replace once the PR number is assigned during the Publish PR step.
  Replace `(branch: <branch-name>)` with the standard `([#NNN](url))` link when the PR number is known.

## Dependency Freshness

Dependency freshness checks are not part of the default workflow. They require
network access and can create noisy results. Run them only when requested:

```sh
dev/check-pr --check-updates
```
