import std/[nre, strutils, sequtils, times, uri, options, tables]
import unicode

# {.experimental: "codeReordering".}

const
  NotFound* = -1
  UnicodeMaxCode = 0x10FFFF

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
    matchTreshold: float
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
    matchTreshold: 0.5,
    matchDistance: 1000,
    patchDeleteThreshold: 0.5,
    patchMargin: 4,
    matchMaxBits: 32,
  )

let defaultParams = newDiffMatchPatch()

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

      line = text[lineStart .. lineEnd]

      if lineHash.hasKey(line):
        chars.add(Rune(lineHash[line]))
      else:
        if lineArray.len == maxLines:
          # TODO: Find Rune() limitations to bail out as in other languages. Or remove the limitation.
          line = text[lineStart .. ^1]
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
    let runes = diff.text.toRunes
    for r in runes:
      let index = r.int
      if index >= 0 and index < lineArray.len:
        text.add(lineArray[index])
    diff.text = text

# CHECKED OK
proc commonPrefix(text1, text2: string): int =
  if text1.len == 0 or text2.len == 0 or text1[0] != text2[0]:
    return 0
  let n = min(text1.len, text2.len)
  for i in 0 ..< n:
    if text1[i] != text2[i]:
      return i
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
  let
    text1Length = text1.len
    text2Length = text2.len
    n = min(text1Length, text2Length)
  # Quick check for common null cases.
  if text1.len == 0 or text2.len == 0 or text1[^1] != text2[^1]:
    return 0
  for i in 1 .. n:
    if text1[text1Length - i] != text2[text2Length - i]:
      return i - 1
  return n

  # Binary search.
  # Performance analysis: https://neil.fraser.name/news/2007/10/09/
  # var
  #   pointermin = 0
  #   pointermax = min(text1.len, text2.len)
  #   pointermid = pointermax
  #   pointerend = 0

  # while pointermin < pointermid:
  #   if text1[^pointermid..^1] == text2[^pointermid..^1]:
  #     pointermin = pointermid
  #     pointerend = pointermin
  #   else:
  #     pointermax = pointermid
  #   pointermid = (pointermax - pointermin) div 2 + pointermin

  # return pointermid

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
    whitespace1 = nonAlphaNumeric1 and (char1 in Whitespace - Newlines)
    whitespace2 = nonAlphaNumeric2 and (char2 in Whitespace - Newlines)
    lineBreak1 = whitespace1 and (char1 in Newlines)
    lineBreak2 = whitespace2 and (char2 in Newlines)
    blankLine1 = lineBreak1 and unicode.strip(one).len == 0 # SUS
    blankLine2 = lineBreak2 and unicode.strip(two).len == 0

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
  var thisPointer = 1
  var
    equality1: string = ""
    edit: string = ""
    equality2: string = ""
    bestEquality1: string
    bestEdit: string
    bestEquality2: string
    bestScore: int
    score: int

  # Intentionally ignore the first and last element (don't need checking).
  while thisPointer < diffs.len - 1:
    if diffs[thisPointer - 1].op == Equal and diffs[thisPointer + 1].op == Equal:
      # This is a single edit surrounded by equalities.
      equality1 = diffs[thisPointer - 1].text
      edit = diffs[thisPointer].text
      equality2 = diffs[thisPointer + 1].text

      # First, shift the edit as far left as possible.
      let commonOffset = commonSuffix(equality1, edit)
      if commonOffset > 0:
        let commonString = edit[^commonOffset ..^ 1]
        equality1 = equality1[0 ..^ (commonOffset + 1)]
        edit = commonString & edit[0 ..^ (commonOffset + 1)]
        equality2 = commonString & equality2

      # Second, step character by character right, looking for the best fit.
      bestEquality1 = equality1
      bestEdit = edit
      bestEquality2 = equality2
      bestScore =
        cleanupSemanticScore(equality1, edit) + cleanupSemanticScore(edit, equality2)

      while edit.len > 0 and equality2.len > 0 and edit[0] == equality2[0]:
        equality1 &= edit[0]
        edit = edit[1 ..^ 1] & equality2[0]
        equality2 = equality2[1 ..^ 1]
        score =
          cleanupSemanticScore(equality1, edit) + cleanupSemanticScore(edit, equality2)
        # The >= encourages trailing rather than leading whitespace on edits.
        if score >= bestScore:
          bestScore = score
          bestEquality1 = equality1
          bestEdit = edit
          bestEquality2 = equality2

      if diffs[thisPointer - 1].text != bestEquality1:
        # We have an improvement, save it back to the diff.
        if bestEquality1.len > 0:
          # diffs[thisPointer - 1] = (Equal, bestEquality1)
          diffs[thisPointer - 1].text = bestEquality1
        else:
          diffs.delete(thisPointer - 1)
          dec thisPointer
        # diffs[thisPointer] = (diffs[thisPointer][0], bestEdit)
        diffs[thisPointer] = (diffs[thisPointer][0], bestEdit)
        if bestEquality2.len > 0:
          # diffs[thisPointer + 1] = (Equal, bestEquality2)
          diffs[thisPointer + 1].text = bestEquality2
        else:
          diffs.delete(thisPointer + 1)
          dec thisPointer

    inc thisPointer

# CHECKED OK
proc cleanupMerge(diffs: var seq[StringDiff]) =
  # Add a dummy entry at the end.
  diffs.add((Equal, ""))
  var
    thisPointer = 0
    countDelete = 0
    countInsert = 0
    textDelete = ""
    textInsert = ""
    commonLength: int # TODO: Ordinal
    newOps: seq[StringDiff] = @[]

  while thisPointer < diffs.len:
    case diffs[thisPointer].op
    of Insert:
      inc countInsert
      textInsert &= diffs[thisPointer].text
      # Added manually:
      inc thisPointer
      break
    of Delete:
      inc countDelete
      textDelete &= diffs[thisPointer].text
      # Added manually:
      inc thisPointer
      break
    of Equal:
      # Upon reaching an equality, check for prior redundancies.
      if countDelete + countInsert > 1:
        if countDelete != 0 and countInsert != 0:
          # Factor out any common prefixes.
          commonLength = commonPrefix(textInsert, textDelete)
          if commonLength != 0:
            let x = thisPointer - countDelete - countInsert - 1
            if x >= 0 and diffs[x].op == Equal:
              diffs[x] = (Equal, diffs[x].text & textInsert[0 ..< commonLength])
            else:
              diffs.insert((Equal, textInsert[0 ..< commonLength]), 0)
              inc thisPointer
            textInsert = textInsert[commonLength ..^ 1]
            textDelete = textDelete[commonLength ..^ 1]

          # Factor out any common suffixes.
          commonLength = commonSuffix(textInsert, textDelete)
          if commonLength != 0:
            # diffs[thisPointer] = (Equal, textInsert[^commonLength..^1] & diffs[thisPointer][1])
            # textInsert = textInsert[0..^(commonLength+1)]
            # textDelete = textDelete[0..^(commonLength+1)]
            diffs[thisPointer] = (
              Equal,
              textInsert[^(textInsert.len - commonLength) ..^ 1] & diffs[thisPointer][1],
            )
            textInsert = textInsert[0 ..^ (textInsert.len - commonLength)]
            textDelete = textDelete[0 ..^ (textInsert.len - commonLength)]

        # Delete the offending records and add the merged ones.
        if textDelete.len != 0:
          newOps.add((Delete, textDelete))
        if textInsert.len != 0:
          newOps.add((Insert, textInsert))
        diffs.delete(thisPointer - countDelete - countInsert .. thisPointer)
        thisPointer = thisPointer - countDelete - countInsert
        for op in newOps:
          diffs.insert(op, thisPointer)
          inc thisPointer
        inc thisPointer
      elif thisPointer != 0 and diffs[thisPointer - 1].op == Equal:
        # Merge this equality with the previous one.
        diffs[thisPointer - 1] =
          (Equal, diffs[thisPointer - 1].text & diffs[thisPointer][1])
        diffs.delete(thisPointer)
      else:
        inc thisPointer

      countInsert = 0
      countDelete = 0
      textDelete = ""
      textInsert = ""

  if diffs[^1][1].len == 0:
    diffs.setLen(diffs.len - 1) # Remove the dummy entry at the end.

  # Second pass: look for single edits surrounded on both sides by equalities
  # which can be shifted sideways to eliminate an equality.
  # e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
  var changes = false
  thisPointer = 1
  # Intentionally ignore the first and last element (don't need checking).
  while thisPointer < diffs.len - 1:
    if diffs[thisPointer - 1].op == Equal and diffs[thisPointer + 1].op == Equal:
      # This is a single edit surrounded by equalities.
      if diffs[thisPointer][1].endsWith(diffs[thisPointer - 1][1]):
        # Shift the edit over the previous equality.
        diffs[thisPointer] = (
          diffs[thisPointer][0],
          diffs[thisPointer - 1].text &
            diffs[thisPointer][1][0 ..^ (diffs[thisPointer - 1][1].len + 1)],
        )
        diffs[thisPointer + 1] = (
          diffs[thisPointer + 1][0],
          diffs[thisPointer - 1].text & diffs[thisPointer + 1][1],
        )
        diffs.delete(thisPointer - 1)
        changes = true
      elif diffs[thisPointer][1].startsWith(diffs[thisPointer + 1][1]):
        # Shift the edit over the next equality.
        diffs[thisPointer - 1] = (
          diffs[thisPointer - 1][0],
          diffs[thisPointer - 1].text & diffs[thisPointer + 1][1],
        )
        diffs[thisPointer] = (
          diffs[thisPointer][0],
          diffs[thisPointer][1][diffs[thisPointer + 1][1].len ..^ 1] &
            diffs[thisPointer + 1][1],
        )
        diffs.delete(thisPointer + 1)
        changes = true
    inc thisPointer

  # If shifts were made, the diff needs reordering and another shift sweep.
  if changes:
    cleanupMerge(diffs)

# CHECKED OK
proc cleanupEfficiency(diffs: var seq[StringDiff], params: DMPConfig) =
  var
    changes = false
    equalities: seq[int] = @[] # Stack of indices where equalities are found.
    lastEquality = "" # Always equal to diffs[equalities[^1]][1]
    thisPointer = 0 # Index of current position.
    pre_ins = false # Is there an insertion operation before the last equality.
    pre_del = false # Is there a deletion operation before the last equality.
    post_ins = false # Is there an insertion operation after the last equality.
    post_del = false # Is there a deletion operation after the last equality.

  while thisPointer < diffs.len:
    case diffs[thisPointer].op
    of Equal: # Equality found.
      if diffs[thisPointer][1].len < params.diffEditCost and (post_ins or post_del):
        # Candidate found.
        equalities.add(thisPointer)
        pre_ins = post_ins
        pre_del = post_del
        lastEquality = diffs[thisPointer].text
      else:
        # Not a candidate, and can never become one.
        equalities.setLen(0)
        lastEquality = ""

      post_ins = false
      post_del = false
    else: # An insertion or deletion.
      if diffs[thisPointer].op == Delete:
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
            thisPointer = equalities[^1]
          else:
            thisPointer = -1
          post_ins = false
          post_del = false
        changes = true

    inc thisPointer

  if changes:
    cleanupMerge(diffs)

# CHECKED OK
proc xIndex(diffs: seq[StringDiff], loc: int): int =
  var
    chars1 = 0
    chars2 = 0
    last_chars1 = 0
    last_chars2 = 0
    lastDiff: Option[StringDiff]

  for (op, text) in diffs:
    if op != Insert:
      chars1 += text.len
    if op != Delete:
      chars2 += text.len

    # FIX Rewritten manually:
    if chars1 > loc:
      # return last_chars2
      lastDiff = (op, text).some
      break

    last_chars1 = chars1
    last_chars2 = chars2
  if lastDiff.isNone and lastDiff.get()[0] == Delete:
    return last_chars2
  return last_chars2 + (loc - last_chars1)
  # return diffs.len

# CHECKED SUS (maybe subst `div` with `/`)
proc cleanupSemantic(diffs: var seq[StringDiff]) =
  var
    changes = false
    equalities: seq[int] = @[]
    lastEquality = ""
    thisPointer = 0
    lengthInsertions1 = 0
    lengthDeletions1 = 0
    lengthInsertions2 = 0
    lengthDeletions2 = 0

  while thisPointer < diffs.len:
    if diffs[thisPointer].op == Equal:
      equalities.add(thisPointer)
      lengthInsertions1 = lengthInsertions2
      lengthDeletions1 = lengthDeletions2
      lengthInsertions2 = 0
      lengthDeletions2 = 0
      lastEquality = diffs[thisPointer].text
    else:
      if diffs[thisPointer].op == Insert:
        lengthInsertions2 += diffs[thisPointer][1].len
      else:
        lengthDeletions2 += diffs[thisPointer][1].len

      if lastEquality.len > 0 and (
        lastEquality.len <= max(lengthInsertions1, lengthDeletions1) and
        lastEquality.len <= max(lengthInsertions2, lengthDeletions2)
      ):
        diffs.insert((Delete, lastEquality), equalities[^1])
        diffs[equalities[^1] + 1] = (Insert, diffs[equalities[^1] + 1][1])
        equalities.setLen(equalities.len - 1)
        if equalities.len > 0:
          equalities.setLen(equalities.len - 1)
        thisPointer =
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

    inc thisPointer

  if changes:
    cleanupMerge(diffs)

  cleanupSemanticLossless(diffs)

  thisPointer = 1
  # while thisPointer < diffs.len - 1:
  while thisPointer < diffs.len:
    if diffs[thisPointer - 1].op == Delete and diffs[thisPointer + 1].op == Insert:
      let
        deletion = diffs[thisPointer - 1].text
        insertion = diffs[thisPointer + 1].text
        overlap_length1 = commonOverlap(deletion, insertion)
        overlap_length2 = commonOverlap(insertion, deletion)

      if overlap_length1 >= overlap_length2:
        if overlap_length1 >= deletion.len div 2 or
            overlap_length1 >= insertion.len div 2:
          diffs.insert((Equal, insertion[0 ..< overlap_length1]), thisPointer)
          diffs[thisPointer - 1].text = deletion[0 ..< deletion.len - overlap_length1]
          diffs[thisPointer + 1].text = insertion[overlap_length1 ..^ 1]
          inc thisPointer
      else:
        if overlap_length2 >= deletion.len div 2 or
            overlap_length2 >= insertion.len div 2:
          diffs.insert((Equal, deletion[0 ..< overlap_length2]), thisPointer)
          diffs[thisPointer - 1] =
            (Insert, insertion[0 ..< insertion.len - overlap_length2])
          diffs[thisPointer + 1] = (Delete, deletion[overlap_length2 ..^ 1])
          inc thisPointer
      inc thisPointer
    inc thisPointer

# CHECKED OK
proc lineMode(
    text1, text2: string, deadline: float, params: DMPConfig = defaultParams
): seq[StringDiff] =
  # Scan the text on a line-by-line basis first.
  let (text1, text2, linearray) = linesToChars(text1, text2)

  var diffs = makeDiffs(text1, text2, false, deadline, params)

  # Convert the diff back to original text.
  charsToLines(diffs, linearray)
  # Eliminate freak matches (e.g. blank lines)
  cleanupSemantic(diffs)

  # Rediff any replacement blocks, this time character-by-character.
  # Add a dummy entry at the end.
  diffs.add((Equal, ""))
  var thisPointer = 0
  var countDelete = 0
  var countInsert = 0
  var textDelete = ""
  var textInsert = ""
  while thisPointer < diffs.len:
    case diffs[thisPointer].op
    of Insert:
      inc countInsert
      textInsert &= diffs[thisPointer].text
    of Delete:
      inc countDelete
      textDelete &= diffs[thisPointer].text
    of Equal:
      # Upon reaching an equality, check for prior redundancies.
      if countDelete >= 1 and countInsert >= 1:
        # Delete the offending records and add the merged ones.
        var subDiffs = makeDiffs(textDelete, textInsert, false, deadline, params)
        # diffs[thisPointer - countDelete - countInsert ..< thisPointer] = subDiffs # maybe .. and not ..<
        diffs.delete(
          thisPointer - count_delete - count_insert ..< count_delete + count_insert
        )
        thisPointer = thisPointer - countDelete - countInsert
        # for (op, data) in subDiffs:
        #   diffs.insert((op, data), thisPointer)
        #   thisPointer += 1
        var insIdx = thisPointer
        for diff in subDiffs:
          diffs.insert(diff, insIdx)
          inc insIdx
        thisPointer += subDiffs.len
      countInsert = 0
      countDelete = 0
      textDelete = ""
      textInsert = ""
    inc thisPointer

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
        let k2_offset = v_offset + delta - k1 # max_d?
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
        (op, longtext[0 ..< i]),
        (Equal, shorttext),
        (op, longtext[i + shorttext.len ..^ 1]),
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

  if text1 == nil or text2 == nil: # TODO: refurbish
    raise newException(ValueError, "Null inputs. (makeDiffs)")

  if text1 == text2:
    if text1.len > 0:
      return @[(Equal, text1)]
    return @[]

  # Trim off common prefix
  var commonLength = commonPrefix(text1, text2)
  let commonPrefix = text1[0 ..< commonLength]
  text1 = text1[commonLength .. ^1]
  text2 = text2[commonLength .. ^1]

  # Trim off common suffix
  commonLength = commonSuffix(text1, text2)
  let commonsuffix = text1[text1.len - commonLength .. ^1]
  text1 = text1[0 ..< text1.len - commonLength]
  text2 = text2[0 ..< text2.len - commonLength]

  # Compute the diff on the middle block
  result = computeDiff(text1, text2, checklines, deadline, params)

  # Restore the prefix and suffix
  if commonPrefix.len > 0:
    result.insert((Equal, commonPrefix), 0)
  if commonsuffix.len > 0:
    result.add((Equal, commonsuffix))
  cleanupMerge(result)

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
  # result = ""
  for (op, data) in diffs:
    if op != Insert:
      result &= data

#CHECKED
proc diffText2(diffs: seq[StringDiff]): string =
  # result = ""
  for (op, data) in diffs:
    if op != Delete:
      result &= data

# CHECKED
func levenshtein(diffs: seq[StringDiff]): int =
  var
    levenshtein = 0
    insertions = 0
    deletions = 0

  for (op, data) in diffs:
    case op
    of Insert:
      insertions += data.len
    of Delete:
      deletions += data.len
    of Equal:
      levenshtein += max(insertions, deletions)
      insertions = 0
      deletions = 0

  levenshtein += max(insertions, deletions)
  # levenshtein

# CHECKED MAYBE OK
proc matchAlphabet(
    pattern: string, params: DMPConfig = defaultParams
): Table[char, int] =
  result = initTable[char, int]()
  for c in pattern:
    result[c] = 0
  for i in 0 ..< pattern.len:
    result[pattern[i]] = result[pattern[i]] or (1 shl (pattern.len - i - 1))

proc bitapScore(
    e, x, loc: int, pattern: string, params: DMPConfig = defaultParams
): float =
  let accuracy = e / pattern.len
  let proximity = abs(loc - x)
  if params.matchDistance == 0:
    # Dodge divide by zero error.
    return if proximity == 0: accuracy else: 1.0
  return accuracy + (float(proximity) / float(params.matchDistance))

# CHECKED OK
proc bitap(text, pattern: string, loc: int, params: DMPConfig = defaultParams): int =
  assert(
    params.matchMaxBits == 0 or pattern.len <= params.matchMaxBits,
    "Pattern too long for this application.",
  )

  # Initialise the alphabet.
  let s = matchAlphabet(pattern, params)

  # Highest score beyond which we give up.
  var scoreThreshold = params.matchTreshold
  # Is there a nearby exact match? (speedup)
  var bestLoc = text.find(pattern, loc)
  if bestLoc != NotFound:
    scoreThreshold = min(bitapScore(0, bestLoc, loc, pattern, params), scoreThreshold)
    # What about in the other direction? (speedup)
    bestLoc = text.rfind(pattern, loc + pattern.len)
    if bestLoc != NotFound:
      scoreThreshold = min(bitapScore(0, bestLoc, loc, pattern, params), scoreThreshold)

  # Initialise the bit arrays.
  let matchmask = 1 shl (pattern.len - 1)
  bestLoc = NotFound

  var
    binMin, binMid: int
    binMax = pattern.len + text.len
    last_rd: seq[int] = @[]
  for d in 0 ..< pattern.len: # SUS
    binMin = 0
    binMid = binMax
    while binMin < binMid:
      if bitapScore(d, loc + binMid, loc, pattern, params) <= scoreThreshold:
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
      if text.len <= j - 1 or s.contains(text[j - 1]):
        # Out of range.
        charMatch = 0
      else:
        charMatch = s.getOrDefault(text[j - 1], 0) # this probably can be just get()
      if d == 0:
        # First pass: exact match.
        rd[j] = ((rd[j + 1] shl 1) or 1) and charMatch
      else:
        # Subsequent passes: fuzzy match.
        rd[j] =
          (((rd[j + 1] shl 1) or 1) and charMatch) or
          (((last_rd[j + 1] or last_rd[j]) shl 1) or 1) or last_rd[j + 1]
      if (rd[j] and matchmask) != 0:
        let score = bitapScore(d, j - 1, loc, pattern, params)
        if score <= scoreThreshold:
          # This match will almost certainly be better than any existing match.
          # But check anyway.
          scoreThreshold = score
          bestLoc = j - 1
          if bestLoc > loc:
            # When passing loc, don't exceed our current distance from loc.
            start = max(1, 2 * loc - bestLoc)
          else:
            # Already passed loc, downhill from here on in.
            break
    if bitapScore(d + 1, loc, loc, pattern, params) > scoreThreshold:
      # No hope for a (better) match at greater error levels.
      break
    last_rd = rd

  return bestLoc

# CHECKED OK
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
proc addContext(patch: var Patch, text: string, params: DMPConfig) =
  if text == "":
    return
  var
    pattern = text[patch.start2 ..< (patch.start2 + patch.length1)]
    padding = 0

  # Look for the first and last matches of pattern in text.  If two different
  # matches are found, increase the pattern length.
  while (
    text.find(pattern) != text.rfind(pattern) and
    pattern.len < params.matchMaxBits - params.patchMargin - params.patchMargin
  )
  :
    padding += params.patchMargin
    pattern = text[
      max(0, patch.start2 - padding) ..<
        min(text.len, patch.start2 + patch.length1 + padding)
    ]

  # Add one chunk for good luck.
  padding += params.patchMargin

  # Add the prefix.
  let prefix = text[max(0, patch.start2 - padding) ..< patch.start2]
  if prefix != "":
    patch.diffs.insert((Equal, prefix), 0)

  # Add the suffix.
  let suffix = text[
    patch.start2 + patch.length1 ..<
      min(text.len, patch.start2 + patch.length1 + padding)
  ]
  if suffix != "":
    patch.diffs.add((Equal, suffix))

  # Roll back the start points.
  patch.start1 -= prefix.len
  patch.start2 -= prefix.len
  # Extend the lengths.
  patch.length1 += prefix.len + suffix.len
  patch.length2 += prefix.len + suffix.len

# CHECKED OK
proc makePatch2(
    text1: string, diffs: seq[StringDiff], params: DMPConfig = defaultParams
): seq[Patch] =
  result = @[]
  if diffs.len == 0:
    return result # Get rid of the null case.
  var
    patch = Patch(diffs: @[], start1: 0, start2: 0, length1: 0, length2: 0)
    char_count1 = 0 # Number of characters into the text1 string.
    char_count2 = 0 # Number of characters into the text2 string.
    # Start with text1 (prepatchText) and apply the diffs until we arrive at
    # text2 (postpatchText). We recreate the patches one by one to determine
    # context info.
    prepatchText = text1
    postpatchText = text1
  for (op, data) in diffs:
    if patch.diffs.len == 0 and op != Equal:
      # A new patch starts here.
      patch.start1 = char_count1
      patch.start2 = char_count2

    case op
    of Insert:
      patch.diffs.add((op, data))
      patch.length2 += data.len
      postpatchText =
        postpatchText[0 ..< char_count2] & data & postpatchText[char_count2 ..^ 1]
    of Delete:
      patch.length1 += data.len
      patch.diffs.add((op, data))
      postpatchText =
        postpatchText[0 ..< char_count2] & postpatchText[(char_count2 + data.len) ..^ 1]
    of Equal:
      if data.len <= 2 * params.patchMargin and patch.diffs.len != 0 and
          op != diffs[^1][0]:
        # Small equality inside a patch.
        patch.diffs.add((op, data))
        patch.length1 += data.len
        patch.length2 += data.len
      if data.len >= 2 * params.patchMargin and patch.diffs.len != 0:
        # Time for a new patch.
        # if patch.diffs.len != 0:
        addContext(patch, prepatchText, params)
        result.add(patch)
        patch = Patch(
          diffs: @[], start1: char_count1, start2: char_count2, length1: 0, length2: 0
        )
        # FIX Added manually:
        prepatchText = postpatchText
        char_count1 = char_count2

    # Update the current character count.
    if op != Insert:
      char_count1 += data.len
    if op != Delete:
      char_count2 += data.len

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

  return makePatch2(text1, diffs, params)

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
  let paddingLength = params.patchMargin
  var nullPadding = ""
  for i in 1 .. paddingLength:
    nullPadding &= chr(i)

  # Bump all the patches forward.
  for patch in patches.mitems:
    patch.start1 += paddingLength
    patch.start2 += paddingLength

  # Add some padding on start of first diff.
  var patch = patches[0]
  var diffs = patch.diffs
  if diffs.len == 0 or diffs[0].op != Equal:
    # Add nullPadding equality.
    diffs.insert((Equal, nullPadding), 0)
    patch.start1 -= paddingLength # Should be 0.
    patch.start2 -= paddingLength # Should be 0.
    patch.length1 += paddingLength
    patch.length2 += paddingLength
  elif paddingLength > diffs[0][1].len:
    # Grow first equality.
    let extraLength = paddingLength - diffs[0][1].len
    diffs[0] = (diffs[0][0], nullPadding[diffs[0][1].len ..^ 1] & diffs[0][1])
    patch.start1 -= extraLength
    patch.start2 -= extraLength
    patch.length1 += extraLength
    patch.length2 += extraLength

  # Add some padding on end of last diff.
  patch = patches[^1]
  diffs = patch.diffs
  if diffs.len == 0 or diffs[^1].op != Equal:
    # Add nullPadding equality.
    diffs.add((Equal, nullPadding))
    patch.length1 += paddingLength
    patch.length2 += paddingLength
  elif paddingLength > diffs[^1][1].len:
    # Grow last equality.
    let extraLength = paddingLength - diffs[^1][1].len
    diffs[^1] = (diffs[^1][0], diffs[^1].text & nullPadding[0 ..< extraLength])
    patch.length1 += extraLength
    patch.length2 += extraLength

  return nullPadding

# CHECKED MAYBE OK
proc splitMax(patches: var seq[Patch], params: DMPConfig) =
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
    var start1 = bigpatch.start1
    var start2 = bigpatch.start2
    var precontext = ""
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
        let (op, text) = bigpatch.diffs[0]
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
          let text =
            text[0 ..< min(text.len, patchSize - patch.length1 - params.patchMargin)]
          patch.length1 += text.len
          start1 += text.len
          if op == Equal:
            patch.length2 += text.len
            start2 += text.len
          else:
            empty = false

          patch.diffs.add((op, text))
          if text == bigpatch.diffs[0][1]:
            bigpatch.diffs.delete(0)
          else:
            bigpatch.diffs[0] =
              (bigpatch.diffs[0][0], bigpatch.diffs[0][1][text.len ..^ 1])

      # Compute the head context for the next patch.
      precontext = diffText2(patch.diffs)
      precontext = precontext[max(0, precontext.len - params.patchMargin) ..^ 1]

      # Append the end context for this patch.
      var postcontext = diffText1(bigpatch.diffs)
      postcontext = postcontext[0 ..< min(postcontext.len, params.patchMargin)]
        # MAYBE SUS?
      if postcontext.len != 0:
        patch.length1 += postcontext.len
        patch.length2 += postcontext.len
        if patch.diffs.len != 0 and patch.diffs[^1].op == Equal:
          patch.diffs[^1] = (Equal, patch.diffs[^1].text & postcontext)
        else:
          patch.diffs.add((Equal, postcontext))

      if not empty:
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
    patches = deepCopy(srcPatches)
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
      end_loc = -1
    if text1.len > params.matchMaxBits:
      # splitMax will only provide an oversized pattern in the case of
      # a monster delete.
      start_loc =
        findMatch(text, text1[0 ..< params.matchMaxBits], expected_loc, params)
      if start_loc != -1:
        end_loc = findMatch(
          text,
          text1[^params.matchMaxBits ..^ 1],
          expected_loc + text1.len - params.matchMaxBits,
          params,
        )
        if end_loc == -1 or start_loc >= end_loc:
          # Can't find valid trailing context.  Drop this patch.
          start_loc = -1
    else:
      start_loc = findMatch(text, text1, expected_loc, params)

    if start_loc == -1:
      # No match found.  :(
      results[x] = false
      # Subtract the delta for this failed patch from subsequent patches.
      delta -= patch.length2 - patch.length1
    else:
      # Found a match.  :)
      results[x] = true
      delta = start_loc - expected_loc
      var text2: string
      if end_loc == -1:
        text2 = text[start_loc ..< min(start_loc + text1.len, text.len)]
      else:
        text2 = text[start_loc ..< min(end_loc + params.matchMaxBits, text.len)]
      if text1 == text2:
        # Perfect match, just shove the replacement text in.
        text =
          text[0 ..< start_loc] & diffText2(patch.diffs) &
          text[start_loc + text1.len ..^ 1]
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
                  text[0 ..< start_loc + index2] & data & text[start_loc + index2 ..^ 1]
              of Delete:
                text =
                  text[0 ..< (start_loc + index2)] &
                  text[start_loc + xIndex(diffs, index1 + data.len) ..^ 1]
              else:
                discard
            if op != Delete:
              index1 += data.len
    inc x

  # Strip the padding off.
  text = text[nullPadding.len ..^ nullPadding.len]
  return (text, results)

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
    text &= encodeUrl(data, usePlus = true) & "\n"
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
proc patchFromText(textline: string): seq[Patch] =
  var patches: seq[Patch] = @[]
  if textline == "":
    return patches
  var
    text = textline.split('\n')
    textPointer = 0
    patchHeader = re"^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@$"
  while textPointer < text.len:
    var m = text[textPointer].match(patchHeader)
    if m.isNone:
      raise newException(ValueError, "Invalid patch string: " & text[textPointer])
    var patch = Patch()
    patches.add(patch)
    let
      capt = m.get.captures
      start1 = capt[1]
      length1 = capt[2]
      start2 = capt[3]
      length2 = capt[4]
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

    inc textPointer

    while textPointer < text.len:
      if text[textPointer] == "":
        inc textPointer
        continue
      var sign = text[textPointer][0]
      var line = decodeUrl(text[textPointer][1 ..^ 1], decodePlus = true)

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
      inc textPointer

  return patches

# CHECKED OK
proc toDelta(diffs: seq[StringDiff]): string =
  var text: seq[string] = @[]
  for (op, data) in diffs:
    case op
    of Insert:
      # High ascii will raise UnicodeDecodeError. Use Unicode instead.
      # let encodedData = encodeUrl(data, "!~*'();/?:@&=+$,# ")
      let encodedData = encodeUrl(data, usePlus = true)
      text.add("+" & encodedData)
    of Delete:
      text.add("-" & $data.len)
    of Equal:
      text.add("=" & $data.len)
  return text.join("\t")

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
    let param = token[1 ..^ 1]
    case token[0]
    of '+':
      let decodedParam = decodeUrl(param, decodePlus = true)
      diffs.add((Insert, decodedParam))
    of '-', '=':
      let n = parseInt(param)
      if n < 0:
        raise newException(ValueError, "Negative number in fromDelta: " & param)
      let text = text1[pointer ..< pointer + n]
      pointer += n
      if token[0] == '=':
        diffs.add((Equal, text))
      else:
        diffs.add((Delete, text))
    else:
      raise
        newException(ValueError, "Invalid diff operation in fromDelta: " & $token[0])
  if pointer != text1.len:
    raise newException(
      ValueError,
      "Delta length (" & $pointer & ") does not equal source text length (" & $text1.len &
        ").",
    )
  return diffs
