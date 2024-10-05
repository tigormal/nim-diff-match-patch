import std/[nre, strutils, sequtils, times, uri, options, tables]
import std/strformat
import unicode

# {.experimental: "codeReordering".}

const
  NotFound* = -1
  UnicodeMaxCode = 0x10FFFF
let
  BlankLineEnd = re"(?m)\n\r?\n$"
  BlankLineStart = re"(?m)^\r?\n\r?\n"

type
  DiffOp* = enum
    Delete
    Insert
    Equal

  StringDiff = tuple[op: DiffOp, text: string]
  HalfMatch = tuple[text1A, text1B, text2A, text2B, midCommon: string]

  DMPConfig* = ref object
    diffTimeout: float
    diffEditCost: int
    matchThreshold: float
    matchDistance: int
    patchDeleteThreshold: float
    patchMargin: int
    matchMaxBits: int

  Patch* = object
    diffs: seq[StringDiff]
    start1: int
    start2: int
    length1: int
    length2: int

proc newDiffMatchPatch*(): DMPConfig =
  result = DMPConfig(
    diffTimeout: 1.0,
    diffEditCost: 4,
    matchThreshold: 0.5,
    matchDistance: 1000,
    patchDeleteThreshold: 0.5,
    patchMargin: 4,
    matchMaxBits: 32,
  )

let defaultParams {.compileTime.} = DMPConfig(
  diffTimeout: 1.0,
  diffEditCost: 4,
  matchThreshold: 0.5,
  matchDistance: 1000,
  patchDeleteThreshold: 0.5,
  patchMargin: 4,
  matchMaxBits: 32,
)

# PREV: diff_main
proc makeDiffs*(
  text1, text2: string,
  checklines = true,
  deadline: float = 0,
  params: DMPConfig = defaultParams,
): seq[StringDiff]

# CHECKED OK
proc linesToChars(
    text1, text2: string
): tuple[chars1, chars2: string, lineArray: seq[string]] =
  var
    lineArray: seq[string] = @[""]
    lineHash: Table[string, int] = initTable[string, int]()

  proc linesToCharsMunge(text: string, maxLines: int): seq[Rune] =
    var
      chars: seq[Rune] = @[]
      lineStart = 0
      lineEnd = NotFound
      line: string

    while lineEnd < text.len - 1:
      lineEnd = text.find('\n', lineStart)
      if lineEnd == NotFound:
        lineEnd = text.len - 1

      line = text.substr(lineStart, lineEnd)

      if lineHash.hasKey(line):
        chars.add(Rune(lineHash[line]))
      else:
        if lineArray.len == maxLines:
          line = text.substr(lineStart)
          lineEnd = text.len
        lineArray.add(line)
        lineHash[line] = lineArray.len - 1
        chars.add(Rune(lineArray.len - 1))

      lineStart = lineEnd + 1

    return chars

  let
    chars1 = $linesToCharsMunge(text1, UnicodeMaxCode div 3 * 2)
      # NOTE: Python uses 666_666
    chars2 = $linesToCharsMunge(text2, UnicodeMaxCode)

  return (chars1, chars2, lineArray)

# CHECKED OK
proc charsToLines(diffs: var seq[StringDiff], lineArray: seq[string]) =
  for diff in diffs.mitems:
    var text = ""
    for r in diff.text.runes:
      let index = r.int
      if index >= 0 and index < lineArray.len:
        text.add(lineArray[index])
    diff.text = text

# CHECKED OK
proc commonPrefix(text1, text2: string): int =
  if text1.len == 0 or text2.len == 0 or text1[0] != text2[0]:
    #echo fmt"cmn pfx: {text1}, {text2}, 0"
    return 0
  let n = min(text1.len, text2.len)
  for i in 0 ..< n:
    if text1[i] != text2[i]:
      #echo fmt"cmn pfx: {text1}, {text2}, {$i}"
      return i
  #echo fmt"cmn pfx: {text1}, {text2}, {$n}"
  return n

  # var pointermin = 0
  # var pointermax = min(text1.len, text2.len)
  # var pointermid = pointermax
  # var pointerstart = 0
  # while pointermin < pointermid:
  #   if text1[pointerstart ..< pointermid] == text2[pointerstart ..< pointermid]:
  #     pointermin = pointermid
  #     pointerstart = pointermin
  #   else:
  #     pointermax = pointermid
  #   pointermid = (pointermax - pointermin) div 2 + pointermin
  # pointermid

# CHECKED OK
proc commonSuffix(text1, text2: string): int =
  #let
  # text1Length = text1.len
  #text2Length = text2.len
  #n = min(text1Length, text2Length)
  # Quick check for common null cases.
  #if text1.len == 0 or text2.len == 0 or text1[^1] != text2[^1]:
  # return 0
  #for i in 1 .. n:
  # if text1[text1Length - i] != text2[text2Length - i]:
  #  return i - 1
  #return n

  # Binary search.
  # Performance analysis: https://neil.fraser.name/news/2007/10/09/
  var
    pointermin = 0
    pointermax = min(text1.len, text2.len)
    pointermid = pointermax
    pointerend = 0

  while pointermin < pointermid:
    if text1[text1.len - pointermid ..< text1.len - pointerend] ==
        text2[text2.len - pointermid ..< text2.len - pointerend]:
      pointermin = pointermid
      pointerend = pointermin
    else:
      pointermax = pointermid
    pointermid = (pointermax - pointermin) div 2 + pointermin

  #echo fmt"cmn sfx: {text1}, {text2}, {$pointermid}"
  return pointermid

# CHECKED OK
proc commonOverlap(text1, text2: string): int =
  let
    text1Length = text1.len
    text2Length = text2.len
  var
    text1 = text1
    text2 = text2
  if text1Length == 0 or text2Length == 0:
    return 0
  if text1Length > text2Length:
    # return commonOverlap(text2, text1)
    # Added manually:
    text1 = text1[text1Length - text2Length ..^ 1]
  elif text1Length < text2Length:
    text2 = text2[0 ..< text1Length]

  let text_len = min(text1Length, text2Length)
  if text1 == text2:
    return text_len

  var
    best = 0
    length = 1
  while true:
    let pattern = text1[text_len - length ..^ 1]
    let found = text2.find(pattern)
    if found == NotFound:
      return best
    length += found
    if found == 0 or text1[text_len - length ..^ 1] == text2[0 ..< length]:
      best = length
      inc length

# CHECKED SUS
proc cleanupSemanticScore(one, two: string): int =
  if one.len == 0 or two.len == 0:
    return 6

  let
    char1 = one[^1]
    char2 = two[0]
    nonAlphaNumeric1 = not char1.isAlphaNumeric
    nonAlphaNumeric2 = not char2.isAlphaNumeric
    whitespace1 = nonAlphaNumeric1 and char1.isSpaceAscii
      #(char1 in Whitespace - Newlines)
    whitespace2 = nonAlphaNumeric2 and char2.isSpaceAscii
      #(char2 in Whitespace - Newlines)
    lineBreak1 = whitespace1 and (char1 in Newlines)
    lineBreak2 = whitespace2 and (char2 in Newlines)
    blankLine1 = lineBreak1 and find(one, BlankLineEnd).isSome
    blankLine2 = lineBreak2 and find(two, BlankLineStart).isSome

  if blankLine1 or blankLine2:
    return 5
  elif lineBreak1 or lineBreak2:
    return 4
  elif nonAlphaNumeric1 and not whitespace1 and whitespace2:
    return 3
  elif whitespace1 or whitespace2:
    return 2
  elif nonAlphaNumeric1 or nonAlphaNumeric2:
    return 1
  return 0

# CHECKED OK
proc cleanupSemanticLossless(diffs: var seq[StringDiff]) =
  var i = 1
  var
    equality1: string = ""
    edit: string = ""
    equality2: string = ""
    commonString: string
    bestEquality1: string
    bestEdit: string
    bestEquality2: string
    bestScore: int
    score: int

  # Intentionally ignore the first and last element (don't need checking).
  while i < diffs.len - 1:
    if diffs[i - 1].op == Equal and diffs[i + 1].op == Equal:
      # This is a single edit surrounded by equalities.
      equality1 = diffs[i - 1].text
      edit = diffs[i].text
      equality2 = diffs[i + 1].text

      # First, shift the edit as far left as possible.
      let commonOffset = commonSuffix(equality1, edit)
      if commonOffset != 0:
        commonString = edit.substr(edit.len - commonOffset)
        equality1 = equality1.substr(0, equality1.len - commonOffset - 1)
        edit = commonString & edit.substr(0, edit.len - commonOffset - 1)
        equality2 = commonString & equality2

      # Second, step character by character right, looking for the best fit.
      bestEquality1 = equality1
      bestEdit = edit
      bestEquality2 = equality2
      bestScore =
        cleanupSemanticScore(equality1, edit) + cleanupSemanticScore(edit, equality2)

      while edit.len != 0 and equality2.len != 0 and edit[0] == equality2[0]:
        equality1 &= edit[0]
        edit = edit.substr(1) & equality2[0]
        equality2 = equality2.substr(1)
        score =
          cleanupSemanticScore(equality1, edit) + cleanupSemanticScore(edit, equality2)
        # The >= encourages trailing rather than leading whitespace on edits.
        if score >= bestScore:
          bestScore = score
          bestEquality1 = equality1
          bestEdit = edit
          bestEquality2 = equality2

      if diffs[i - 1].text != bestEquality1:
        # We have an improvement, save it back to the diff.
        if bestEquality1.len > 0:
          # diffs[i - 1] = (Equal, bestEquality1)
          diffs[i - 1].text = bestEquality1
        else:
          diffs.delete(i - 1)
          dec i
        # diffs[i] = (diffs[i][0], bestEdit)
        diffs[i].text = bestEdit
        if bestEquality2.len > 0:
          # diffs[i + 1] = (Equal, bestEquality2)
          diffs[i + 1].text = bestEquality2
        else:
          diffs.delete(i + 1)
          dec i

    inc i

proc cleanupMerge(diffs: var seq[StringDiff])

# CHECKED OK
proc cleanupMerge(diffs: var seq[StringDiff]) =
  #echo "cleanupMerge start"
  # Add a dummy entry at the end.
  diffs.add((Equal, ""))
  var
    i = 0
    countDelete: Natural = 0
    countInsert: Natural = 0
    textDelete = ""
    textInsert = ""
    commonLength: Natural

  while i < diffs.len:
    case diffs[i].op
    of Insert:
      inc countInsert
      textInsert &= diffs[i].text
      echo fmt"textInsert: {textInsert}"
      inc i
    of Delete:
      inc countDelete
      textDelete &= diffs[i].text
      echo fmt"textDelete: {textDelete}"
      inc i
    of Equal:
      # Upon reaching an equality, check for prior redundancies.
      if countDelete + countInsert > 1:
        if countDelete != 0 and countInsert != 0:
          # Factor out any common prefixes.
          #echo "cleanupMerge factor out prefixes"
          commonLength = commonPrefix(textInsert, textDelete)
          if commonLength != 0:
            let x = i - countDelete - countInsert - 1
            if x >= 0 and diffs[x].op == Equal:
              diffs[x].text &= textInsert.substr(0, commonLength - 1)
            else:
              diffs.insert((Equal, textInsert.substr(0, commonLength - 1)), 0)
              inc i
            textInsert = textInsert.substr(commonLength)
            textDelete = textDelete.substr(commonLength)
            echo fmt"textInsert: {textInsert}, textDelete: {textDelete}"

          # Factor out any common suffixes.
          #echo "cleanupMerge factor out suffixes"
          commonLength = commonSuffix(textInsert, textDelete)
          if commonLength != 0:
            diffs[i].text =
              textInsert.substr(textInsert.len - commonLength) & diffs[i].text
            textInsert = textInsert.substr(0, textInsert.len - commonLength - 1)
            textDelete = textDelete.substr(0, textDelete.len - commonLength - 1)
            #echo fmt"textInsert: {textInsert}, textDelete: {textDelete}"

        # Delete the offending records and add the merged ones.
        #echo "cleanupMerge delete offending"
        var newOps: seq[StringDiff] = @[]
        if textDelete.len != 0:
          newOps.add((Delete, textDelete))
        if textInsert.len != 0:
          newOps.add((Insert, textInsert))
        #echo newOps
        i -= countDelete + countInsert
        diffs[i ..< i + countDelete + countInsert] = newOps
        i += newOps.len + 1
      elif i != 0 and diffs[i - 1].op == Equal:
        # Merge this equality with the previous one.
        #echo "cleanupMerge merge equalitiles"
        diffs[i - 1].text &= diffs[i].text
        diffs.delete(i)
      else:
        inc i

      echo "reset vars"
      countInsert = 0
      countDelete = 0
      textDelete = ""
      textInsert = ""

  if diffs[^1].text.len == 0:
    diffs.setLen(diffs.len - 1) # Remove the dummy entry at the end.

  # Second pass: look for single edits surrounded on both sides by equalities
  # which can be shifted sideways to eliminate an equality.
  # e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
  var changes = false
  i = 1
  # Intentionally ignore the first and last element (don't need checking).
  while i < diffs.len - 1:
    if diffs[i - 1].op == Equal and diffs[i + 1].op == Equal:
      # This is a single edit surrounded by equalities.
      if diffs[i].text.endsWith(diffs[i - 1].text):
        # Shift the edit over the previous equality.
        if diffs[i - 1].text != "":
          diffs[i].text =
            diffs[i - 1].text &
            diffs[i].text.substr(0, diffs[i].text.len - diffs[i - 1].text.len - 1)
          diffs[i + 1].text = diffs[i - 1].text & diffs[i + 1].text
        diffs.delete(i - 1)
        #echo "changes: true"
        changes = true
      elif diffs[i].text.startsWith(diffs[i + 1].text):
        # Shift the edit over the next equality.
        diffs[i - 1].text &= diffs[i + 1].text
        diffs[i].text = diffs[i].text.substr(diffs[i + 1].text.len) & diffs[i + 1].text
        diffs.delete(i + 1)
        #echo "changes: true"
        changes = true
    inc i

  # If shifts were made, the diff needs reordering and another shift sweep.
  if changes:
    cleanupMerge(diffs)

# CHECKED OK
proc cleanupEfficiency(diffs: var seq[StringDiff], params: DMPConfig) =
  var
    changes = false
    equalities: seq[int] = @[] # Stack of indices where equalities are found.
    lastEquality = "" # Always equal to diffs[equalities[^1]][1]
    i = 0 # Index of current position.
    pre_ins = false # Is there an insertion operation before the last equality.
    pre_del = false # Is there a deletion operation before the last equality.
    post_ins = false # Is there an insertion operation after the last equality.
    post_del = false # Is there a deletion operation after the last equality.

  while i < diffs.len:
    case diffs[i].op
    of Equal: # Equality found.
      if diffs[i][1].len < params.diffEditCost and (post_ins or post_del):
        # Candidate found.
        equalities.add(i)
        pre_ins = post_ins
        pre_del = post_del
        lastEquality = diffs[i].text
      else:
        # Not a candidate, and can never become one.
        equalities.setLen(0)
        lastEquality = ""

      post_ins = false
      post_del = false
    else: # An insertion or deletion.
      if diffs[i].op == Delete:
        post_del = true
      else:
        post_ins = true

      # Five types to be split:
      # <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
      # <ins>A</ins>X<ins>C</ins><del>D</del>
      # <ins>A</ins><del>B</del>X<ins>C</ins>
      # <ins>A</del>X<ins>C</ins><del>D</del>
      # <ins>A</ins><del>B</del>X<del>C</del>

      if lastEquality.len > 0 and (
        (pre_ins and pre_del and post_ins and post_del) or (
          (lastEquality.len < params.diffEditCost div 2) and
          (pre_ins.int + pre_del.int + post_ins.int + post_del.int) == 3
        )
      ): # SUS
        # Duplicate record.
        diffs.insert((Delete, lastEquality), equalities[^1])
        # Change second copy to insert.
        diffs[equalities[^1] + 1] = (Insert, diffs[equalities[^1] + 1][1])
        equalities.setLen(equalities.len - 1) # Throw away the equality we just deleted.
        lastEquality = ""
        if pre_ins and pre_del:
          # No changes made which could affect previous entry, keep going.
          post_ins = true
          post_del = true
          equalities.setLen(0)
        else:
          if equalities.len > 0:
            equalities.setLen(equalities.len - 1) # Throw away the previous equality.
          if equalities.len > 0:
            i = equalities[^1]
          else:
            i = -1
          post_ins = false
          post_del = false
        changes = true

    inc i

  if changes:
    cleanupMerge(diffs)

# CHECKED OK
proc xIndex(diffs: seq[StringDiff], loc: int): int =
  var
    chars1 = 0
    chars2 = 0
    lastChars1 = 0
    lastChars2 = 0
    lastDiff: Option[StringDiff]

  for (op, text) in diffs:
    if op != Insert:
      chars1 += text.len
    if op != Delete:
      chars2 += text.len

    # FIX Rewritten manually:
    if chars1 > loc:
      # return lastChars2
      lastDiff = (op, text).some
      break

    lastChars1 = chars1
    lastChars2 = chars2
  if lastDiff.isSome and lastDiff.get.op == Delete:
    return lastChars2
  return lastChars2 + (loc - lastChars1)
  # return diffs.len

# CHECKED OK
proc cleanupSemantic(diffs: var seq[StringDiff]) =
  var
    changes = false
    equalities: seq[int] = @[]
    lastEquality = ""
    i = 0
    lengthInsertions1 = 0
    lengthDeletions1 = 0
    lengthInsertions2 = 0
    lengthDeletions2 = 0

  while i < diffs.len:
    if diffs[i].op == Equal:
      equalities.add(i)
      lengthInsertions1 = lengthInsertions2
      lengthDeletions1 = lengthDeletions2
      lengthInsertions2 = 0
      lengthDeletions2 = 0
      lastEquality = diffs[i].text
    else:
      if diffs[i].op == Insert:
        lengthInsertions2 += diffs[i].text.len
      else:
        lengthDeletions2 += diffs[i].text.len

      if (lastEquality.len > 0) and (
        lastEquality.len <= max(lengthInsertions1, lengthDeletions1) and
        lastEquality.len <= max(lengthInsertions2, lengthDeletions2)
      ):
        diffs.insert((Delete, lastEquality), equalities[^1])
        diffs[equalities[^1] + 1] = (Insert, diffs[equalities[^1] + 1].text)
        equalities.setLen(equalities.len - 1)
        if equalities.len > 0:
          equalities.setLen(equalities.len - 1)
        i =
          if equalities.len > 0:
            equalities[^1]
          else:
            -1
        lengthInsertions1 = 0
        lengthDeletions1 = 0
        lengthInsertions2 = 0
        lengthDeletions2 = 0
        lastEquality = ""
        changes = true

    inc i

  if changes:
    cleanupMerge(diffs)

  cleanupSemanticLossless(diffs)

  i = 1
  while i < diffs.len:
    if diffs[i - 1].op == Delete and diffs[i].op == Insert:
      let
        deletion = diffs[i - 1].text
        insertion = diffs[i].text
        overlapLen1 = commonOverlap(deletion, insertion)
        overlapLen2 = commonOverlap(insertion, deletion)

      if overlapLen1 >= overlapLen2:
        if float(overlapLen1) >= float(deletion.len) / 2 or
            float(overlapLen1) >= float(insertion.len) / 2:
          diffs.insert(
            (Equal, insertion.substr(0, overlapLen1 - 1)),
          i)
          diffs[i - 1].text = deletion.substr(0, deletion.len - overlapLen1 - 1)
          diffs[i + 1].text = insertion.substr(overlapLen1)
          inc i
      else:
        if overlapLen2 >= deletion.len div 2 or
            overlapLen2 >= insertion.len div 2:
          diffs.insert(
            (Equal, deletion.substr(0, overlapLen2 - 1)),
          i)
          diffs[i - 1] = (Insert, insertion.substr(0, insertion.len - overlapLen2 - 1))
          diffs[i + 1] = (Delete, deletion.substr(overlapLen2))
          inc i
      inc i
    inc i

# CHECKED OK
proc lineMode(
    text1, text2: string, deadline: float, params: DMPConfig = defaultParams
): seq[StringDiff] =
  # Scan the text on a line-by-line basis first.
  let (text1, text2, linearray) = linesToChars(text1, text2)

  var diffs = makeDiffs(text1, text2, false, deadline, params)

  # Convert the diff back to original text.
  charsToLines(diffs, linearray)
  echo "charsToLines", diffs, "\n"
  # Eliminate freak matches (e.g. blank lines)
  cleanupSemantic(diffs)

  # Rediff any replacement blocks, this time character-by-character.
  # Add a dummy entry at the end.
  diffs.add((Equal, ""))
  var i = 0
  var countDelete = 0
  var countInsert = 0
  var textDelete = ""
  var textInsert = ""
  while i < diffs.len:
    case diffs[i].op
    of Insert:
      inc countInsert
      textInsert &= diffs[i].text
    of Delete:
      inc countDelete
      textDelete &= diffs[i].text
    of Equal:
      # Upon reaching an equality, check for prior redundancies.
      if countDelete >= 1 and countInsert >= 1:
        # Delete the offending records and add the merged ones.
        var subDiffs = makeDiffs(textDelete, textInsert, false, deadline, params)
        diffs.delete(i - count_delete - count_insert ..< count_delete + count_insert)
        i = i - countDelete - countInsert
        var insIdx = i
        for diff in subDiffs:
          diffs.insert(diff, insIdx)
          inc insIdx
        i += subDiffs.len
      countInsert = 0
      countDelete = 0
      textDelete = ""
      textInsert = ""
    inc i

  diffs.setLen(diffs.len - 1) # Remove the dummy entry at the end.
  diffs

# CHECKED OK
proc bisectSplit(
    text1, text2: string, x, y: int, deadline: float, params: DMPConfig = defaultParams
): seq[StringDiff] =
  let
    text1a = text1[0 ..< x]
    text2a = text2[0 ..< y]
    text1b = text1[x ..^ 1]
    text2b = text2[y ..^ 1]

  result = makeDiffs(text1a, text2a, false, deadline, params)
  return result & makeDiffs(text1b, text2b, false, deadline, params)

# CHECKED OK
proc bisect(
    text1, text2: string, deadline: float, params: DMPConfig = defaultParams
): seq[StringDiff] =
  # Cache the text lengths to prevent multiple calls.
  let
    text1Length = text1.len
    text2Length = text2.len
    max_d = (text1Length + text2Length + 1) div 2
    v_offset = max_d
    v_length = 2 * max_d
  var
    v1 = newSeqWith[int](v_length, -1)
    v2 = newSeqWith[int](v_length, -1)
  v1[v_offset + 1] = 0
  v2[v_offset + 1] = 0
  let delta = text1Length - text2Length
  # If the total number of characters is odd, then the front path will
  # collide with the reverse path.
  let front = (delta mod 2 != 0)
  # Offsets for start and end of k loop.
  # Prevents mapping of space beyond the grid.
  var
    k1start = 0
    k1end = 0
    k2start = 0
    k2end = 0
  for d in 0 ..< max_d:
    # Bail out if deadline is reached.
    if epochTime() > deadline:
      break

    # Walk the front path one step.
    for k1 in countup(-d + k1start, d - k1end, 2):
      let k1_offset = v_offset + k1
      var x1: int
      if k1 == -d or (k1 != d and v1[k1_offset - 1] < v1[k1_offset + 1]):
        x1 = v1[k1_offset + 1]
      else:
        x1 = v1[k1_offset - 1] + 1
      var y1 = x1 - k1
      while x1 < text1Length and y1 < text2Length and text1[x1] == text2[y1]:
        inc x1
        inc y1
      v1[k1_offset] = x1
      if x1 > text1Length:
        # Ran off the right of the graph.
        k1end += 2
      elif y1 > text2Length:
        # Ran off the bottom of the graph.
        k1start += 2
      elif front:
        let k2_offset = v_offset + delta - k1
        if k2_offset >= 0 and k2_offset < v_length and v2[k2_offset] != -1:
          # Mirror x2 onto top-left coordinate system.
          let x2 = text1Length - v2[k2_offset]
          if x1 >= x2:
            # Overlap detected.
            return bisectSplit(text1, text2, x1, y1, deadline, params)

    # Walk the reverse path one step.
    for k2 in countup(-d + k2start, d - k2end, 2):
      let k2_offset = v_offset + k2
      var x2: int
      if k2 == -d or (k2 != d and v2[k2_offset - 1] < v2[k2_offset + 1]):
        x2 = v2[k2_offset + 1]
      else:
        x2 = v2[k2_offset - 1] + 1
      var y2 = x2 - k2
      while x2 < text1Length and y2 < text2Length and
          text1[text1Length - x2 - 1] == text2[text2Length - y2 - 1]:
        inc x2
        inc y2
      v2[k2_offset] = x2
      if x2 > text1Length:
        # Ran off the left of the graph.
        k2end += 2
      elif y2 > text2Length:
        # Ran off the top of the graph.
        k2start += 2
      elif not front:
        let k1_offset = v_offset + delta - k2
        if k1_offset >= 0 and k1_offset < v_length and v1[k1_offset] != -1:
          let
            x1 = v1[k1_offset]
            y1 = v_offset + x1 - k1_offset
          # Mirror x2 onto top-left coordinate system.
          x2 = text1Length - x2
          if x1 >= x2:
            # Overlap detected.
            return bisectSplit(text1, text2, x1, y1, deadline, params)

  # StringDiff took too long and hit the deadline or
  # number of diffs equals number of characters, no commonality at all.
  return @[(Delete, text1), (Insert, text2)]

# CHECKED OK
proc halfMatch(
    text1, text2: string, params: DMPConfig = defaultParams
): Option[HalfMatch] =
  if params.diffTimeout <= 0:
    return none(HalfMatch)

  let
    longtext = if text1.len > text2.len: text1 else: text2
    shorttext = if text1.len > text2.len: text2 else: text1

  if longtext.len < 4 or shorttext.len * 2 < longtext.len:
    return none(HalfMatch)

  # CHECKED OK
  proc halfMatchI(longtext, shorttext: string, i: int): Option[HalfMatch] =
    let seed = longtext[i ..< i + longtext.len div 4]
    var
      j = NotFound
      bestCommon = ""
      bestLongtextA = ""
      bestLongtextB = ""
      bestShorttextA = ""
      bestShorttextB = ""
    proc run(): int =
      j = shorttext.find(seed, j + 1)
      j

    result = none(HalfMatch)

    while run() != NotFound:
      let
        prefixLength = commonPrefix(longtext[i ..^ 1], shorttext[j ..^ 1])
        suffixLength = commonSuffix(longtext[0 ..< i], shorttext[0 ..< j])

      if bestCommon.len < suffixLength + prefixLength:
        bestCommon =
          shorttext[j - suffixLength ..< j] & shorttext[j ..< j + prefixLength]
        bestLongtextA = longtext[0 ..< i - suffixLength]
        bestLongtextB = longtext[i + prefixLength ..^ 1]
        bestShorttextA = shorttext[0 ..< j - suffixLength]
        bestShorttextB = shorttext[j + prefixLength ..^ 1]
    if bestCommon.len * 2 >= longtext.len:
      result =
        (bestLongtextA, bestLongtextB, bestShorttextA, bestShorttextB, bestCommon).some

  # ------ halfMatchI END -------

  let
    hm1 = halfMatchI(longtext, shorttext, (longtext.len + 3) div 4)
    hm2 = halfMatchI(longtext, shorttext, (longtext.len + 1) div 2)
    # hm: Option[HalfMatch]
    hm =
      if hm1.isNone and hm2.isNone:
        return none(HalfMatch)
      elif hm2.isNone:
        hm1
      elif hm1.isNone:
        hm2
      else:
        if hm1.get.midCommon.len > hm2.get.midCommon.len: hm1 else: hm2

  result =
    if (text1.len > text2.len):
      hm
    else:
      (hm.get[2], hm.get[3], hm.get[0], hm.get[1], hm.get[4]).some

# CHECKED OK
proc computeDiff(
    text1, text2: string,
    checklines: bool,
    deadline: float,
    params: DMPConfig = defaultParams,
): seq[StringDiff] =
  var diffs: seq[StringDiff]

  if text1 == "":
    # Just add some text (speedup).
    return @[(Insert, text2)]

  if text2 == "":
    # Just delete some text (speedup).
    return @[(Delete, text1)]

  let (longtext, shorttext) =
    if text1.len > text2.len:
      (text1, text2)
    else:
      (text2, text1)
  let i = longtext.find(shorttext)
  if i != NotFound:
    # Shorter text is inside the longer text (speedup).
    let op = if text1.len > text2.len: Delete else: Insert
    return
      @[
        (op, longtext.substr(0, i - 1)),
        (Equal, shorttext),
        (op, longtext.substr(i + shorttext.len)),
      ]

  if shorttext.len == 1:
    # Single character string.
    # After the previous speedup, the character can't be an equality.
    return @[(Delete, text1), (Insert, text2)]

  # Check to see if the problem can be split in two.
  let hm = halfMatch(text1, text2, params)
  if hm.isSome:
    # A half-match was found, sort out the return data.
    let
      (text1A, text1B, text2A, text2B, midCommon) = hm.get
      # Send both pairs off for separate processing.
      diffs_a = makeDiffs(text1A, text2A, checklines, deadline, params)
      diffs_b = makeDiffs(text1B, text2B, checklines, deadline, params)
    # Merge the results.
    return diffs_a & @[(Equal, midCommon)] & diffs_b

  if checklines and text1.len > 100 and text2.len > 100:
    return lineMode(text1, text2, deadline, params)
  return bisect(text1, text2, deadline, params)

# CHECKED OK
proc makeDiffs*(
    text1, text2: string,
    checklines = true,
    deadline: float = 0,
    params: DMPConfig = defaultParams,
): seq[StringDiff] =
  var
    deadline = deadline
    text1 = text1
    text2 = text2
  if deadline == 0:
    if params.diffTimeout <= 0:
      deadline = high(float)
    else:
      deadline = epochTime() + params.diffTimeout

  #if text1 == nil or text2 == nil:
  #  raise newException(ValueError, "Null inputs. (makeDiffs)")

  if text1 == text2:
    if text1.len > 0:
      return @[(Equal, text1)]
    return @[]

  # Trim off common prefix
  var commonLength = commonPrefix(text1, text2)
  let commonPrefix = text1.substr(0, commonLength - 1)
  text1 = text1.substr(commonLength)
  text2 = text2.substr(commonLength)

  # Trim off common suffix
  commonLength = commonSuffix(text1, text2)
  let commonsuffix = text1.substr(text1.len - commonLength)
  text1 = text1.substr(0, text1.len - commonLength - 1)
  text2 = text2.substr(0, text2.len - commonLength - 1)

  # Compute the diff on the middle block
  result = computeDiff(text1, text2, checklines, deadline, params)

  # Restore the prefix and suffix
  if commonPrefix.len != 0:
    result.insert((Equal, commonPrefix), 0)
  if commonsuffix.len != 0:
    result.add((Equal, commonsuffix))
  echo "computeDiff", result
  cleanupMerge(result) # kvar

# CHECKED OK
proc prettyHtml*(diffs: seq[StringDiff]): string =
  # var result = ""
  for (op, text) in diffs:
    let data = text
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\n", "&para;<br>")
    case op
    of Insert:
      result &= "<ins style=\"background:#e6ffe6;\">" & data & "</ins>"
    of Delete:
      result &= "<del style=\"background:#ffe6e6;\">" & data & "</del>"
    of Equal:
      result &= "<span>" & data & "</span>"

proc diffText1(diffs: seq[StringDiff]): string =
  for (op, data) in diffs:
    if op != Insert:
      result &= data

#CHECKED
proc diffText2(diffs: seq[StringDiff]): string =
  for (op, data) in diffs:
    if op != Delete:
      result &= data

# CHECKED
func levenshtein(diffs: seq[StringDiff]): int =
  var
    insertions = 0
    deletions = 0
  result = 0 # levenshtein

  for (op, data) in diffs:
    case op
    of Insert:
      insertions += data.len
    of Delete:
      deletions += data.len
    of Equal:
      result += max(insertions, deletions)
      insertions = 0
      deletions = 0

  result += max(insertions, deletions)

# CHECKED MAYBE OK
proc matchAlphabet(
    pattern: string, params: DMPConfig = defaultParams
): Table[char, int] =
  result = initTable[char, int]()
  for c in pattern:
    result[c] = 0
  for i in 0 ..< pattern.len:
    result[pattern[i]] = result[pattern[i]] or (1 shl (pattern.len - i - 1))


# CHECKED OK
func bitap(text, pattern: string, loc: int, params: DMPConfig = defaultParams): int =
  assert(
    params.matchMaxBits == 0 or pattern.len <= params.matchMaxBits,
    "Pattern too long for this application.",
  )

  # Initialise the alphabet.
  let s = matchAlphabet(pattern, params)

  func bitapScore(e, x: int): float =
    let accuracy = float(e) / float(pattern.len)
    let proximity = abs(loc - x)
    if params.matchDistance == 0:
      # Dodge divide by zero error.
      return if proximity == 0: accuracy else: 1.0
    return accuracy + (float(proximity) / float(params.matchDistance))

  # Highest score beyond which we give up.
  var scoreThreshold = params.matchThreshold
  # Is there a nearby exact match? (speedup)
  var bestLoc = text.find(pattern, loc)
  if bestLoc != NotFound:
    scoreThreshold = min(bitapScore(0, bestLoc), scoreThreshold)
    # What about in the other direction? (speedup)
    bestLoc = text.rfind(pattern, loc + pattern.len)
    if bestLoc != NotFound:
      scoreThreshold = min(bitapScore(0, bestLoc), scoreThreshold)

  # Initialise the bit arrays.
  let matchmask = 1 shl (pattern.len - 1)
  bestLoc = NotFound

  var
    binMin, binMid: int
    binMax = pattern.len + text.len
    last_rd: seq[int] = @[]
  for d in 0 ..< pattern.len:
    binMin = 0
    binMid = binMax
    while binMin < binMid:
      if bitapScore(d, loc + binMid) <= scoreThreshold:
        binMin = binMid
      else:
        binMax = binMid
      binMid = (binMax - binMin) div 2 + binMin
    binMax = binMid
    var start = max(1, loc - binMid + 1)
    let finish = min(loc + binMid, text.len) + pattern.len

    var rd = newSeq[int](finish + 2)
    rd[finish + 1] = (1 shl d) - 1
    for j in countdown(finish, start):
      var charMatch: int
      if text.len <= j - 1:
        # Out of range.
        charMatch = 0
      else:
        charMatch = s.getOrDefault(text[j - 1], 0)
      if d == 0:
        # First pass: exact match.
        rd[j] = ((rd[j + 1] shl 1) or 1) and charMatch
      else:
        # Subsequent passes: fuzzy match.
        rd[j] =
          ((((rd[j + 1] shl 1) or 1) and charMatch) or
          (((last_rd[j + 1] or last_rd[j]) shl 1) or 1) or last_rd[j + 1])
      if (rd[j] and matchmask) != 0:
        let score = bitapScore(d, j - 1)
        # This match will almost certainly be better than any existing match.
        # But check anyway.
        if score <= scoreThreshold:
          scoreThreshold = score
          bestLoc = j - 1
          if bestLoc > loc:
            # When passing loc, don't exceed our current distance from loc.
            start = max(1, 2 * loc - bestLoc)
          else:
            # Already passed loc, downhill from here on in.
            break
    if bitapScore(d + 1, loc) > scoreThreshold:
      # No hope for a (better) match at greater error levels.
      break
    last_rd = rd
  return bestLoc

# CHECKED OK # PREV: match_main
proc findMatch(
    text, pattern: string, locInitial: int, params: DMPConfig = defaultParams
): int =
  var loc = max(0, min(locInitial, text.len))
  if text == pattern:
    return 0
  elif text.len == 0:
    return NotFound
  elif loc + pattern.len <= text.len and text[loc ..< loc + pattern.len] == pattern:
    return loc
  else:
    return bitap(text, pattern, loc, params)

# CHECKED OK
proc addContext(patch: var Patch, text: string, params: DMPConfig = defaultParams) =
  if text == "":
    return
  var
    pattern = text.substr(patch.start2, patch.start2 + patch.length1 - 1)
    padding = 0

  # Look for the first and last matches of pattern in text.  If two different
  # matches are found, increase the pattern length.
  while (
    text.find(pattern) != text.rfind(pattern) and
    pattern.len < params.matchMaxBits - params.patchMargin - params.patchMargin
  )
  :
    padding += params.patchMargin
    pattern = text.substr(
      max(0, patch.start2 - padding),
      min(text.len, patch.start2 + patch.length1 + padding) - 1
    )

  # Add one chunk for good luck.
  padding += params.patchMargin

  # Add the prefix.
  let prefix = text.substr(max(0, patch.start2 - padding), patch.start2 - 1)
  if prefix != "":
    patch.diffs.insert((Equal, prefix), 0)

  # Add the suffix.
  let suffix = text.substr(
    patch.start2 + patch.length1,
    min(text.len, patch.start2 + patch.length1 + padding) - 1
  )
  if suffix != "":
    patch.diffs.add((Equal, suffix))

  # Roll back the start points.
  patch.start1 -= prefix.len
  patch.start2 -= prefix.len
  # Extend the lengths.
  patch.length1 += prefix.len + suffix.len
  patch.length2 += prefix.len + suffix.len

# CHECKED OK
proc makePatches(
    text1: string, diffs: seq[StringDiff], params: DMPConfig = defaultParams
): seq[Patch] =
  result = @[]
  if diffs.len == 0:
    return result # Get rid of the null case.
  var
    patch = Patch(diffs: @[], start1: 0, start2: 0, length1: 0, length2: 0)
    charCount1 = 0 # Number of characters into the text1 string.
    charCount2 = 0 # Number of characters into the text2 string.
    # Start with text1 (prepatchText) and apply the diffs until we arrive at
    # text2 (postpatchText). We recreate the patches one by one to determine
    # context info.
    prepatchText = text1
    postpatchText = text1
  for (op, data) in diffs:
    if patch.diffs.len == 0 and op != Equal:
      # A new patch starts here.
      patch.start1 = charCount1
      patch.start2 = charCount2

    case op
    of Insert:
      patch.diffs.add((op, data))
      patch.length2 += data.len
      postpatchText =
        postpatchText.substr(0, charCount2-1) & data & postpatchText.substr(charCount2)
    of Delete:
      patch.length1 += data.len
      patch.diffs.add((op, data))
      postpatchText =
        postpatchText.substr(0, charCount2-1) & postpatchText.substr(charCount2 + data.len)
    of Equal:
      if data.len <= 2 * params.patchMargin and patch.diffs.len != 0 and
          (op, data) != diffs[^1]: # FIXED
        # Small equality inside a patch.
        patch.diffs.add((op, data))
        patch.length1 += data.len
        patch.length2 += data.len
      if data.len >= 2 * params.patchMargin and patch.diffs.len != 0:
        # Time for a new patch.
        if patch.diffs.len != 0:
          addContext(patch, prepatchText, params)
          result.add(patch)
          patch = Patch(
            diffs: @[], start1: charCount1, start2: charCount2, length1: 0, length2: 0
          )
          # FIX Added manually:
          prepatchText = postpatchText
          charCount1 = charCount2

    # Update the current character count.
    if op != Insert:
      charCount1 += data.len
    if op != Delete:
      charCount2 += data.len

  # Pick up the leftover patch if not empty.
  if patch.diffs.len != 0:
    addContext(patch, prepatchText, params)
    result.add(patch)

  # return patches

# CHECKED OK
proc makePatches*(text1, text2: string, params: DMPConfig = defaultParams): seq[Patch] =
  # Check for null inputs.
  if text1 == "" and text2 == "":
    return @[]

  var diffs = makeDiffs(text1, text2, true, params = params)
  if diffs.len > 2:
    cleanupSemantic(diffs)
    cleanupEfficiency(diffs, params)

  return makePatches(text1, diffs, params)

proc makePatches*(diffs: seq[StringDiff], params: DMPConfig = defaultParams): seq[Patch] =
  makePatches(diffs.diffText1, diffs, params)

proc makePatches*(text1, text2: string, diffs: seq[StringDiff], params: DMPConfig = defaultParams): seq[Patch] {.deprecated.} =
  makePatches(text1, diffs, params)

# CHECKED OK
proc deepCopy(patches: seq[Patch]): seq[Patch] =
  result = @[]
  for patch in patches:
    result.add Patch(
      diffs: patch.diffs,
      start1: patch.start1,
      start2: patch.start2,
      length1: patch.length1,
      length2: patch.length2,
    )

# CHECKED OK
proc addPatchPadding(
    patches: var seq[Patch], params: DMPConfig = defaultParams
): string =
  #echo patches
  let paddingLength = params.patchMargin
  var nullPadding = ""
  for i in 1 .. paddingLength:
    nullPadding &= chr(i)

  #echo $nullPadding.len

  # Bump all the patches forward.
  #for patch in patches.mitems:
  for i in 0..<patches.len:
    patches[i].start1 += paddingLength
    patches[i].start2 += paddingLength

  # Add some padding on start of first diff.
  #var patch = patches[0] # TODO: change this
  #var diffs = patch.diffs
  if patches[0].diffs.len == 0 or patches[0].diffs[0].op != Equal:
    # Add nullPadding equality.
    patches[0].diffs.insert((Equal, nullPadding), 0)
    patches[0].start1 -= paddingLength # Should be 0.
    patches[0].start2 -= paddingLength # Should be 0.
    patches[0].length1 += paddingLength
    patches[0].length2 += paddingLength
  elif paddingLength > patches[0].diffs[0].text.len:
    # Grow first equality.
    let extraLength = paddingLength - patches[0].diffs[0].text.len
    patches[0].diffs[0].text = nullPadding.substr(patches[0].diffs[0].text.len) & patches[0].diffs[0].text
    patches[0].start1 -= extraLength
    patches[0].start2 -= extraLength
    patches[0].length1 += extraLength
    patches[0].length2 += extraLength

  # Add some padding on end of last diff.
  #patch = patches[^1]
  #diffs = patch.diffs
  if patches[0].diffs.len == 0 or patches[0].diffs[^1].op != Equal:
    # Add nullPadding equality.
    patches[0].diffs.add((Equal, nullPadding))
    patches[0].length1 += paddingLength
    patches[0].length2 += paddingLength
  elif paddingLength > patches[0].diffs[^1].text.len:
    # Grow last equality.
    let extraLength = paddingLength - patches[0].diffs[^1].text.len
    patches[0].diffs[^1].text &= nullPadding.substr(0, extraLength - 1)
    patches[0].length1 += extraLength
    patches[0].length2 += extraLength

  return nullPadding

# CHECKED MAYBE OK
proc splitMax(patches: var seq[Patch], params: DMPConfig = defaultParams) =
  # echo "Patches, len " & $patches.len & ":"
  # echo patches
  let patchSize = params.matchMaxBits
  var i = 0
  while i < patches.len:
    if patches[i].length1 <= patchSize:
      inc i
      continue
    var bigpatch = patches[i]
    # Remove the big old patch.
    patches.delete(i)
    dec i
    var
      start1 = bigpatch.start1
      start2 = bigpatch.start2
      precontext = ""
      postcontext = ""
    while bigpatch.diffs.len != 0:
      # Create one of several smaller patches.
      var patch =
        Patch(start1: start1 - precontext.len, start2: start2 - precontext.len)
      var empty = true
      if precontext.len != 0:
        patch.length1 = precontext.len
        patch.length2 = precontext.len
        patch.diffs.add((Equal, precontext))

      while bigpatch.diffs.len != 0 and patch.length1 < patchSize - params.patchMargin:
        var (op, text) = bigpatch.diffs[0]
        if op == Insert:
          # Insertions are harmless.
          patch.length2 += text.len
          start2 += text.len
          patch.diffs.add(bigpatch.diffs[0])
          bigpatch.diffs.delete(0)
          empty = false
        elif op == Delete and patch.diffs.len == 1 and patch.diffs[0].op == Equal and
            text.len > 2 * patchSize:
          # This is a large deletion.  Let it pass in one chunk.
          patch.length1 += text.len
          start1 += text.len
          empty = false
          patch.diffs.add((op, text))
          bigpatch.diffs.delete(0)
        else:
          # Deletion or equality.  Only take as much as we can stomach.
          # let text = # SUS
          text = # SUS
            # text.substr(0, min(text.len, patchSize - patch.length1 - params.patchMargin) - 1)
            text.substr(0, patchSize - patch.length1 - params.patchMargin - 1)
          patch.length1 += text.len
          start1 += text.len
          if op == Equal:
            patch.length2 += text.len
            start2 += text.len
          else:
            empty = false

          patch.diffs.add((op, text))
          if text == bigpatch.diffs[0].text:
            bigpatch.diffs.delete(0)
          else:
            bigpatch.diffs[0].text = bigpatch.diffs[0].text.substr(text.len)

      # Compute the head context for the next patch.
      precontext = diffText2(patch.diffs)
      # precontext = precontext.substr(max(0, precontext.len - params.patchMargin))
      precontext = precontext.substr(precontext.len - params.patchMargin)

      # Append the end context for this patch.
      postcontext = diffText1(bigpatch.diffs)
      # postcontext = postcontext[0 ..< min(postcontext.len, params.patchMargin)]
      if postcontext.len > params.patchMargin:
        postcontext = postcontext.substr(0, params.patchMargin - 1)
      if postcontext.len != 0:
        patch.length1 += postcontext.len
        patch.length2 += postcontext.len
        if patch.diffs.len != 0 and patch.diffs[^1].op == Equal:
          patch.diffs[^1].text &= postcontext
        else:
          patch.diffs.add((Equal, postcontext))

      if not empty:
        inc i
        patches.insert(patch, i)
    inc i

# CHECKED OK
proc applyPatches*(
    srcText: string, srcPatches: seq[Patch], params: DMPConfig = defaultParams
): tuple[text: string, results: seq[bool]] =
  if srcPatches.len == 0:
    return (srcText, @[])

  # Deep copy the patches so that no changes are made to originals.
  var
    patches = srcPatches #deepCopy(srcPatches)
    nullPadding = addPatchPadding(patches, params)
    text = nullPadding & srcText & nullPadding

  splitMax(patches, params)

  # delta keeps track of the offset between the expected and actual location
  # of the previous patch.  If there are patches expected at positions 10 and
  # 20, but the first patch was found at 12, delta is 2 and the second patch
  # has an effective expected position of 22.
  var
    x = 0
    delta = 0
    results: seq[bool] = newSeq[bool](patches.len)
  for patch in patches:
    # var patch = patches[patchIndex]
    var
      expected_loc = patch.start2 + delta
      text1 = diffText1(patch.diffs)
      start_loc: int
      end_loc = NotFound
    if text1.len > params.matchMaxBits:
      # splitMax will only provide an oversized pattern in the case of
      # a monster delete.
      start_loc =
        findMatch(text, text1.substr(0, params.matchMaxBits - 1), expected_loc, params)
      if start_loc != NotFound:
        end_loc = findMatch(
          text,
          text1.substr(text1.len - params.matchMaxBits),
          expected_loc + text1.len - params.matchMaxBits,
          params,
        )
        if end_loc == NotFound or start_loc >= end_loc:
          # Can't find valid trailing context.  Drop this patch.
          start_loc = NotFound
    else:
      start_loc = findMatch(text, text1, expected_loc, params)

    if start_loc == NotFound:
      # No match found.  :(
      results[x] = false
      # Subtract the delta for this failed patch from subsequent patches.
      delta -= patch.length2 - patch.length1
    else:
      # Found a match.  :)
      results[x] = true
      delta = start_loc - expected_loc
      var text2: string
      if end_loc == NotFound:
        text2 = text.substr(
          start_loc,
          min(start_loc + text1.len, text.len) - 1
        )
      else:
        text2 = text.substr(
          start_loc,
          min(end_loc + params.matchMaxBits, text.len) - 1
        )
      if text1 == text2:
        # Perfect match, just shove the replacement text in.
        text =
          text.substr(0, start_loc - 1) & diffText2(patch.diffs) &
          text.substr(start_loc + text1.len)
      else:
        # Imperfect match.  Run a diff to get a framework of equivalent indices.
        var diffs = makeDiffs(text1, text2, false, params = params)
        if text1.len > params.matchMaxBits and
            levenshtein(diffs).float / text1.len.float > params.patchDeleteThreshold:
          # The end points match, but the content is unacceptably bad.
          results[x] = false
        else:
          cleanupSemanticLossless(diffs)
          var index1 = 0
          for (op, data) in patch.diffs:
            if op != Equal:
              let index2 = xIndex(diffs, index1)
              case op
              of Insert:
                text =
                  text.substr(0, start_loc + index2 - 1) & data &
                  text.substr(start_loc + index2)
              of Delete:
                text =
                  text.substr(0, start_loc + index2 - 1) &
                  text.substr(start_loc + xIndex(diffs, index1 + data.len))
              else:
                discard
            if op != Delete:
              index1 += data.len
    inc x

  # Strip the padding off.
  text = text.substr(nullPadding.len, text.len - nullPadding.len - 1)
  return (text, results)

proc unescapeEncode(str: string): string =
  str.replace("%21", "!").replace("%7E", "~")
    .replace("%27", "'").replace("%28", "(").replace("%29", ")")
    .replace("%3B", ";").replace("%2F", "/").replace("%3F", "?")
    .replace("%3A", ":").replace("%40", "@").replace("%26", "&")
    .replace("%3D", "=").replace("%2B", "+").replace("%24", "$")
    .replace("%2C", ",").replace("%23", "#").replace("%2A", "*")

# CHECKED OK
proc toString*(patch: Patch): string =
  var coords1, coords2: string
  if patch.length1 == 0:
    coords1 = $patch.start1 & ",0"
  elif patch.length1 == 1:
    coords1 = $(patch.start1 + 1)
  else:
    coords1 = $(patch.start1 + 1) & "," & $patch.length1
  if patch.length2 == 0:
    coords2 = $patch.start2 & ",0"
  elif patch.length2 == 1:
    coords2 = $(patch.start2 + 1)
  else:
    coords2 = $(patch.start2 + 1) & "," & $patch.length2

  var text = "@@ -" & coords1 & " +" & coords2 & " @@\n"
  # Escape the body of the patch with %xx notation.
  for (op, data) in patch.diffs:
    case op
    of Insert:
      text &= "+"
    of Delete:
      text &= "-"
    of Equal:
      text &= " "
    text &= encodeUrl(data, usePlus = true).replace('+', ' ').unescapeEncode & "\n"
  return text

proc `$`*(patch: Patch): string =
  toString(patch)

proc toText*(patches: seq[Patch]): string =
  var text = ""
  for patch in patches:
    text &= toString(patch)
  return text

proc `$`*(patches: seq[Patch]): string =
  toText(patches)

# CHECKED MAYBE OK
# TODO: replace var patches with result
proc patchFromText(textline: string): seq[Patch] =
  var patches: seq[Patch] = @[]
  if textline == "":
    return patches
  var
    text = textline.split('\n')
    #textPointer = 0
    patchHeader = re"^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@$"
  while text.len > 0:
  #while textPointer < text.len:
    var m = text[0].match(patchHeader)
    #echo fmt"text[{textPointer}]: {text[textPointer]}"
    if m.isNone:
      raise newException(ValueError, "Invalid patch string: " & text[0])
    var patch = Patch()
    let
      capt = m.get.captures
      start1 = capt[0]
      length1 = capt[1]
      start2 = capt[2]
      length2 = capt[3]
    patch.start1 = parseInt(start1)
    if length1 == "":
      patch.start1.dec
      patch.length1 = 1
    elif length1 == "0":
      patch.length1 = 0
    else:
      patch.start1.dec
      patch.length1 = parseInt(length1)

    patch.start2 = parseInt(start2)
    if length2 == "":
      patch.start2.dec
      patch.length2 = 1
    elif length2 == "0":
      patch.length2 = 0
    else:
      patch.start2.dec
      patch.length2 = parseInt(length2)

    text.delete(0)
    #inc textPointer

    while text.len > 0:
    #while textPointer < text.len:
      if text[0].len == 0:
        text.delete(0)
        #inc textPointer
        continue
      var sign = text[0][0]
      var line = decodeUrl(text[0].substr(1).replace("+", "%2B"), decodePlus = false) # TODO: maybe runeSubStr?

      case sign
      of '+':
        # Insertion.
        patch.diffs.add((Insert, line))
      of '-':
        # Deletion.
        patch.diffs.add((Delete, line))
      of ' ':
        # Minor equality.
        patch.diffs.add((Equal, line))
      of '@':
        # Start of next patch.
        break
      else:
        # WTF?
        raise newException(ValueError, "Invalid patch mode '" & $sign & "' in: " & line)
      text.delete(0)
      #inc textPointer
    patches.add(patch)

  return patches

# CHECKED OK
proc toDelta(diffs: seq[StringDiff]): string =
  var text: seq[string] = @[]
  for (op, data) in diffs:
    case op
    of Insert:
      # High ascii will raise UnicodeDecodeError. Use Unicode instead.
      # let encodedData = encodeUrl(data, "!~*'();/?:@&=+$,# ")
      let encodedData = encodeUrl(data, usePlus = true).replace("+", " ")
      text.add("+" & encodedData)
    of Delete:
      text.add("-" & $data.runeLen)
    of Equal:
      text.add("=" & $data.runeLen)
  return text.join("\t").unescapeEncode

# CHECKED OK
proc fromDelta(text1: string, delta: string): seq[StringDiff] =
  var diffs: seq[StringDiff] = @[]
  var pointer = 0 # Cursor in text1
  let tokens = delta.split("\t")
  for token in tokens:
    if token == "":
      # Blank tokens are ok (from a trailing \t).
      continue
    # Each token begins with a one character parameter which specifies the
    # operation of this token (delete, insert, equality).
    let param = token.runeSubStr(1)
    case token[0]
    of '+':
      let decodedParam = decodeUrl(param.replace("+", "%2B"), decodePlus = false)
      diffs.add((Insert, decodedParam))
    of '-', '=':
      let n = parseInt(param)
      if n < 0:
        raise newException(ValueError, "Negative number in fromDelta: " & param)
      #let text = text1[pointer ..< pointer + n]
      let text = text1.runeSubStr(pointer, n)
      pointer += n
      if token[0] == '=':
        diffs.add((Equal, text))
      else:
        diffs.add((Delete, text))
    else:
      raise
        newException(ValueError, "Invalid diff operation in fromDelta: " & $token[0])
  if pointer != text1.runeLen:
    raise newException(
      ValueError,
      "Delta length (" & $pointer & ") does not equal source text length (" & $text1.runeLen &
        ").",
    )
  return diffs
