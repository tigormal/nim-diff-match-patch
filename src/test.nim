import diffmatchpatch

proc main() =
  let dmp = newDiffMatchPatch()

  let text1 = "A quick brown fox jumps over the lazy dog"
  let text2 = "A fast brown fox leaps over the sleepy dog"

  echo "\n\nOriginal text: ", text1
  echo "Modified text: ", text2

  # Compute the diff
  let diffs = text1.makeDiffs text2
  echo "\nDiff:"
  for diff in diffs:
    case diff[0]
    of Equal:
      stdout.write diff[1]
    of Insert:
      stdout.write "\e[32m", diff[1], "\e[0m" # Green for insertions
    of Delete:
      stdout.write "\e[31m", diff[1], "\e[0m" # Red for deletions

  # Create a patch
  let patches = makePatches(text1, text2)
  let patchText = $patches
  # echo "\n\nPatch:"
  # echo patchText

  # Apply the patch
  let (patchedText, results) = text1.applyPatches(patches)
  echo "\n\nPatched text: ", patchedText
  echo "Patch results: ", results

  # Reverse patch
  let reversePatch = makePatches(text2, text1)
  let (revertedText, revertResults) = text2.applyPatches(reversePatch)
  echo "\nReverted text: ", revertedText
  echo "Revert results: ", revertResults

when isMainModule:
  main()
