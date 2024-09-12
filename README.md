# Diff Match Patch for Nim

This is the **WIP** Nim implementation of Neil Fraser's [Diff Match and Patch](https://github.com/google/diff-match-patch.git) for Google Docs.

### From the original description:

The Diff Match and Patch libraries offer robust algorithms to perform the
operations required for synchronizing plain text.

1. Diff:
   * Compare two blocks of plain text and efficiently return a list of differences.
   * [Diff Demo](https://neil.fraser.name/software/diff_match_patch/demos/diff.html)
2. Match:
   * Given a search string, find its best fuzzy match in a block of plain text. Weighted for both accuracy and location.
   * [Match Demo](https://neil.fraser.name/software/diff_match_patch/demos/match.html)
3. Patch:
   * Apply a list of patches onto plain text. Use best-effort to apply patch even when the underlying text doesn't match.
   * [Patch Demo](https://neil.fraser.name/software/diff_match_patch/demos/patch.html)

### API Differences

This implementation attempts to be more Nim-like and slightly steps away from the implementations in other languages. List of changes (things may be reverted):
- `Diff` is renamed into `StringDiff`
- All function names have `diff_`, `match_` and `patch_` prefixes removed
- `DiffMatchPatch` class is renamed into `DMPConfig` object
- Added `defaultParams` global variable to hold an instance of `DMPConfig` with it's default values
- Functions that access algorithm parameters from `params: DMPConfig = defaultParams`. This way custom parameters could be set explicitly when needed
- _to be continued_

### Tests
Just run
```bash
nimble test
```
