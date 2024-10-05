# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest2

include diffmatchpatch
# {.experimental: "codeReordering".}

suite "Diff basic functions":
  test "Common Prefix":
    check:
      commonPrefix("abc", "xyz") == 0
      commonPrefix("1234abcdef", "1234xyz") == 4
      commonPrefix("1234", "1234xyz") == 4

  test "Common Suffix":
    check:
      commonSuffix("abc", "xyz") == 0
      commonSuffix("abcdef1234", "xyz1234") == 4
      commonSuffix("1234", "xyz1234") == 4

  test "Common Overlap":
    check:
      commonOverlap("", "abcd") == 0
      commonOverlap("abc", "abcd") == 3
      commonOverlap("123456", "abcd") == 0
      commonOverlap("123456xxx", "xxxabcd") == 3
      commonOverlap("fi", "\ufb01i") == 0

suite "Half Match":
  test "Single Match":
    check:
      halfMatch("1234567890", "a345678z").get == ("12", "90", "a", "z", "345678")
      halfMatch("a345678z", "1234567890").get == ("a", "z", "12", "90", "345678")
      halfMatch("abc56789z", "1234567890").get == ("abc", "z", "1234", "0", "56789")
      halfMatch("a23456xyz", "1234567890").get == ("a", "xyz", "1", "7890", "23456")

  test "Multiple Matches":
    check:
      halfMatch("121231234123451234123121", "a1234123451234z").get ==
        ("12123", "123121", "a", "z", "1234123451234")
      halfMatch("x-=-=-=-=-=-=-=-=-=-=-=-=", "xx-=-=-=-=-=-=-=").get ==
        ("", "-=-=-=-=-=", "x", "", "x-=-=-=-=-=-=-=")
      # ("x-=-=-=", "-=-=", "xx", "", "-=-=-=-=-=-=-=") <- FAILED RESULT
      halfMatch("-=-=-=-=-=-=-=-=-=-=-=-=y", "-=-=-=-=-=-=-=yy").get ==
        ("-=-=-=-=-=", "", "", "y", "-=-=-=-=-=-=-=y")
      # ("-=-=-=", "-=-=y", "", "yy", "-=-=-=-=-=-=-=") <- FAILED RESULT

  test "Non-optimal halfmatch":
    # Optimal diff would be -q+x=H-i+e=lloHe+Hu=llo-Hew+y not -qHillo+x=HelloHe-w+Hulloy
    check halfMatch("qHilloHelloHew", "xHelloHeHulloy").get ==
      ("qHillo", "w", "x", "Hulloy", "HelloHe")

  test "Optimal no halfmatch":
    var dmp = newDiffMatchPatch()
    dmp.diffTimeout = 0
    check halfMatch("qHilloHelloHew", "xHelloHeHulloy", dmp).isNone

suite "Lines to Chars":
  var
    lineList8: seq[string]
    lineList16: seq[string]
    charList: seq[Rune]
    chars16: string
    chars8: string
    lines: string
  const n = 300
  when isMainModule:
    charList = @[]
    lineList8 = @[]
    lineList16 = @[]
    for i in 1 .. n:
      lineList8.add($i & "\n")
      charList.add(Rune(i))
    for i in 1 .. 1115000:
      lineList16.add($i & "\n")
    lines = lineList8.join("")
    chars16 = lineList16.join("")
    chars8 = charList.join("")

  test "Basic Operations":
    check:
      linesToChars("alpha\nbeta\nalpha\n", "beta\nalpha\nbeta\n") ==
        ("\x01\x02\x01", "\x02\x01\x02", @["", "alpha\n", "beta\n"])
      linesToChars("", "alpha\r\nbeta\r\n\r\n\r\n") ==
        ("", "\x01\x02\x03\x03", @["", "alpha\r\n", "beta\r\n", "\r\n"])
      linesToChars("a", "b") == ("\x01", "\x02", @["", "a", "b"])

  # More than 256 to reveal any 8-bit limitations
  test "8-bit limitation":
    check lineList8.len == n
    check chars8.runeLen == n
    lineList8.insert("", 0)
    var diffs: seq[StringDiff] = @[(DiffOp.Delete, chars8)]
    charsToLines(diffs, lineList8)
    check diffs == @[(DiffOp.Delete, lines)]

  # More than 1,114,112 to verify any 17 * 16-bit limitation.
  # TODO: Check the function. Takes too much time (3-4 sec)
  test "16-bit limitation":
    let results = linesToChars(chars16, "")
    var diffs = @[(Insert, results[0])]
    charsToLines(diffs, results[2])
    check chars16 == diffs[0][1]

suite "Cleanup Merge":
  test "No change case":
    var diffs = @[(Equal, "a"), (Delete, "b"), (Insert, "c")]
    let correct = @[(Equal, "a"), (Delete, "b"), (Insert, "c")]
    cleanupMerge diffs
    check diffs == correct

  test "Merge equalities":
    var diffs = @[(Equal, "a"), (Equal, "b"), (Equal, "c")]
    let correct = @[(Equal, "abc")]
    cleanupMerge diffs
    check diffs == correct

  test "Merge deletions":
    var diffs = @[(Delete, "a"), (Delete, "b"), (Delete, "c")]
    let correct = @[(Delete, "abc")]
    cleanupMerge diffs
    check diffs == correct

  test "Merge insertions":
    var diffs = @[(Insert, "a"), (Insert, "b"), (Insert, "c")]
    let correct = @[(Insert, "abc")]
    cleanupMerge diffs
    check diffs == correct

  test "Merge interweave":
    var diffs =
      @[
        (Delete, "a"),
        (Insert, "b"),
        (Delete, "c"),
        (Insert, "d"),
        (Equal, "e"),
        (Equal, "f"),
      ]
    let correct = @[(Delete, "ac"), (Insert, "bd"), (Equal, "ef")]
    cleanupMerge diffs
    check diffs == correct

  test "Prefix and suffix detection":
    var diffs = @[(Delete, "a"), (Insert, "abc"), (Delete, "dc")]
    let correct = @[(Equal, "a"), (Delete, "d"), (Insert, "b"), (Equal, "c")]
    cleanupMerge diffs
    check diffs == correct

  test "Prefix and suffix detection with equalities":
    var diffs =
      @[(Equal, "x"), (Delete, "a"), (Insert, "abc"), (Delete, "dc"), (Equal, "y")]
    let correct = @[(Equal, "xa"), (Delete, "d"), (Insert, "b"), (Equal, "cy")]
    cleanupMerge diffs
    check diffs == correct

  test "Slide edit left":
    var diffs = @[(Equal, "a"), (Insert, "ba"), (Equal, "c")]
    let correct = @[(Insert, "ab"), (Equal, "ac")]
    cleanupMerge diffs
    check diffs == correct

  test "Slide edit right":
    var diffs = @[(Equal, "c"), (Insert, "ab"), (Equal, "a")]
    let correct = @[(Equal, "ca"), (Insert, "ba")]
    cleanupMerge diffs
    check diffs == correct

  test "Slide edit left recursive":
    var diffs =
      @[(Equal, "a"), (Delete, "b"), (Equal, "c"), (Delete, "ac"), (Equal, "x")]
    let correct = @[(Delete, "abc"), (Equal, "acx")]
    cleanupMerge diffs
    check diffs == correct

  test "Slide edit right recursive":
    var diffs =
      @[(Equal, "x"), (Delete, "ca"), (Equal, "c"), (Delete, "b"), (Equal, "a")]
    let correct = @[(Equal, "xca"), (Delete, "cba")]
    cleanupMerge diffs
    check diffs == correct

  test "Empty merge":
    var diffs = @[(Delete, "b"), (Insert, "ab"), (Equal, "c")]
    let correct = @[(Insert, "a"), (Equal, "bc")]
    cleanupMerge diffs
    check diffs == correct

  test "Empty equality":
    var diffs = @[(Equal, ""), (Insert, "a"), (Equal, "b")]
    let correct = @[(Insert, "a"), (Equal, "b")]
    cleanupMerge diffs
    check diffs == correct

suite "Cleanup Semantic Lossless":
  test "Null case":
    var diffs: seq[StringDiff] = @[]
    cleanupSemanticLossless(diffs)
    check diffs.len == 0

  test "Blank lines":
    var diffs =
      @[(Equal, "AAA\r\n\r\nBBB"), (Insert, "\r\nDDD\r\n\r\nBBB"), (Equal, "\r\nEEE")]
    let correct =
      @[(Equal, "AAA\r\n\r\n"), (Insert, "BBB\r\nDDD\r\n\r\n"), (Equal, "BBB\r\nEEE")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Line boundaries":
    var diffs = @[(Equal, "AAA\r\nBBB"), (Insert, " DDD\r\nBBB"), (Equal, " EEE")]
    let correct = @[(Equal, "AAA\r\n"), (Insert, "BBB DDD\r\n"), (Equal, "BBB EEE")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Word boundaries":
    var diffs = @[(Equal, "The c"), (Insert, "ow and the c"), (Equal, "at.")]
    let correct = @[(Equal, "The "), (Insert, "cow and the "), (Equal, "cat.")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Alphanumeric boundaries":
    var diffs = @[(Equal, "The-c"), (Insert, "ow-and-the-c"), (Equal, "at.")]
    let correct = @[(Equal, "The-"), (Insert, "cow-and-the-"), (Equal, "cat.")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Hitting the start":
    var diffs = @[(Equal, "a"), (Delete, "a"), (Equal, "ax")]
    let correct = @[(Delete, "a"), (Equal, "aax")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Hitting the end":
    var diffs = @[(Equal, "xa"), (Delete, "a"), (Equal, "a")]
    let correct = @[(Equal, "xaa"), (Delete, "a")]
    cleanupSemanticLossless diffs
    check diffs == correct

  test "Sentence boundaries":
    var diffs = @[(Equal, "The xxx. The "), (Insert, "zzz. The "), (Equal, "yyy.")]
    let correct = @[(Equal, "The xxx."), (Insert, " The zzz."), (Equal, " The yyy.")]
    cleanupSemanticLossless diffs
    check diffs == correct

suite "Cleanup Semantic":
  test "Null case":
    var diffs: seq[StringDiff] = @[]
    cleanupSemantic(diffs)
    check diffs.len == 0

  test "No elimination #1":
    var diffs = @[(Delete, "ab"), (Insert, "cd"), (Equal, "12"), (Delete, "e")]
    let correct = @[(Delete, "ab"), (Insert, "cd"), (Equal, "12"), (Delete, "e")]
    cleanupSemantic diffs
    check diffs == correct

  test "No elimination #2":
    var diffs = @[(Delete, "abc"), (Insert, "ABC"), (Equal, "1234"), (Delete, "wxyz")]
    let correct = @[(Delete, "abc"), (Insert, "ABC"), (Equal, "1234"), (Delete, "wxyz")]
    cleanupSemantic diffs
    check diffs == correct

  test "Simple elimination":
    var diffs = @[(Delete, "a"), (Equal, "b"), (Delete, "c")]
    let correct = @[(Delete, "abc"), (Insert, "b")]
    cleanupSemantic diffs
    check diffs == correct

  test "Backpass elimination":
    var diffs = @[
      (Delete, "ab"),
      (Equal, "cd"),
      (Delete, "e"),
      (Equal, "f"),
      (Insert, "g"),
    ]
    let correct = @[
      (Delete, "abcdef"),
      (Insert, "cdfg")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "Multiple eliminations":
    var diffs = @[
      (Insert, "1"),
      (Equal, "A"),
      (Delete, "B"),
      (Insert, "2"),
      (Equal, "_"),
      (Insert, "1"),
      (Equal, "A"),
      (Delete, "B"),
      (Insert, "2"),
    ]
    let correct = @[
      (Delete, "AB_AB"),
      (Insert, "1A2_1A2")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "Word boundaries":
    var diffs = @[
      (Equal, "The c"),
      (Delete, "ow and the c"),
      (Equal, "at."),
    ]
    let correct = @[
      (Equal, "The "),
      (Delete, "cow and the "),
      (Equal, "cat.")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "No overlap elimination":
    var diffs = @[
      (Delete, "abcxx"),
      (Insert, "xxdef"),
    ]
    let correct = @[
      (Delete, "abcxx"),
      (Insert, "xxdef")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "Overlap elimination":
    var diffs = @[
      (Delete, "abcxxx"),
      (Insert, "xxxdef"),
    ]
    let correct = @[
      (Delete, "abc"),
      (Equal, "xxx"),
      (Insert, "def")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "Reverse overlap elimination":
    var diffs = @[
      (Delete, "xxxabc"),
      (Insert, "defxxx"),
    ]
    let correct = @[
      (Insert, "def"),
      (Equal, "xxx"),
      (Delete, "abc")
    ]
    cleanupSemantic diffs
    check diffs == correct

  test "Two overlap eliminations":
    var diffs = @[
      (Delete, "abcd1212"),
      (Insert, "1212efghi"),
      (Equal, "----"),
      (Delete, "A3"),
      (Insert, "3BC"),
    ]
    let correct = @[
      (Delete, "abcd"),
      (Equal, "1212"),
      (Insert, "efghi"),
      (Equal, "----"),
      (Delete, "A"),
      (Equal, "3"),
      (Insert, "BC")
    ]
    cleanupSemantic diffs
    check diffs == correct

suite "Cleanup Efficiency":

  var dmp = newDiffMatchPatch()
  dmp.diffEditCost = 4

  test "Null case":
    var diffs: seq[StringDiff] = @[]
    cleanupEfficiency(diffs, dmp)
    check diffs.len == 0

  test "Four-edit elimination":
    var diffs = @[
      (Delete, "ab"),
      (Insert, "12"),
      (Equal, "xyz"),
      (Delete, "cd"),
      (Insert, "34"),
    ]
    let correct = @[
      (Delete, "abxyzcd"),
      (Insert, "12xyz34")
    ]
    cleanupEfficiency diffs, dmp
    check diffs == correct

  test "Three-edit elimination":
    var diffs = @[
      (Insert, "12"),
      (Equal, "x"),
      (Delete, "cd"),
      (Insert, "34"),
    ]
    let correct = @[
      (Delete, "xcd"),
      (Insert, "12x34")
    ]
    cleanupEfficiency diffs, dmp
    check diffs == correct

  test "Backpass elimination":
    var diffs = @[
      (Delete, "ab"),
      (Insert, "12"),
      (Equal, "xy"),
      (Insert, "34"),
      (Equal, "z"),
      (Delete, "cd"),
      (Insert, "56"),
    ]
    let correct = @[
      (Delete, "abxyzcd"),
      (Insert, "12xy34z56")
    ]
    cleanupEfficiency diffs, dmp
    check diffs == correct

  test "High cost elimination":
    dmp.diffEditCost = 5
    var diffs = @[
      (Delete, "ab"),
      (Insert, "12"),
      (Equal, "wxyz"),
      (Delete, "cd"),
      (Insert, "34"),
    ]
    let correct = @[
      (Delete, "abwxyzcd"),
      (Insert, "12wxyz34")
    ]
    cleanupEfficiency diffs, dmp
    check diffs == correct

suite "Pretty HTML":
  test "Pretty print":
    var diffs = @[
      (Equal, "a\n"),
      (Delete, "<B>b</B>"),
      (Insert, "c&d"),
    ]
    let correct = "<span>a&para;<br></span><del style=\"background:#ffe6e6;\">&lt;B&gt;b&lt;/B&gt;</del><ins style=\"background:#e6ffe6;\">c&amp;d</ins>"
    check correct == prettyHtml(diffs)

suite "Diff Text":
  test "Compute the source and destination texts":
    var diffs = @[
      (Equal, "jump"),
      (Delete, "s"),
      (Insert, "ed"),
      (Equal, " over "),
      (Delete, "the"),
      (Insert, "a"),
      (Equal, " lazy"),
    ]
    check "jumps over the lazy" == diffText1(diffs)
    check "jumped over a lazy"  == diffText2(diffs)

suite "Diff Delta":
  var
    diffs = @[
      (Equal, "jump"),
      (Delete, "s"),
      (Insert, "ed"),
      (Equal, " over "),
      (Delete, "the"),
      (Insert, "a"),
      (Equal, " lazy"),
      (Insert, "old dog"),
    ]
    correct: string
    text1 = diffText1(diffs)
    text2: string
    delta = diffs.toDelta

  test "Convert a diff into delta string":
    check "jumps over the lazy" == text1
    let correct = "=4\t-1\t+ed\t=6\t-3\t+a\t=5\t+old dog"
    check correct == delta
    # Convert delta string into a diff
    check diffs == text1.fromDelta(delta)


  test "Generate errors":
    proc gen19isNot20 =
      discard fromDelta(text1 & "x", delta)
    proc gen19isNot18 =
      discard fromDelta(text1[1 .. ^1], delta)
    proc genInvalidUnicode =
      discard fromDelta("", "+%c3xy")
    expect ValueError:
      gen19isNot20()
      gen19isNot18()
      genInvalidUnicode()

  test "Deltas with special characters":
    diffs = @[
      (Equal, "\u0680 \x00 \t %"),
      (Delete, "\u0681 \x01 \n ^"),
      (Insert, "\u0682 \x02 \\ |")
    ]

    text1 = diffText1(diffs)
    delta = diffs.toDelta
    correct = "\u0680 \x00 \t %\u0681 \x01 \n ^"
    check correct == text1

    delta = toDelta diffs
    correct = "=7\t-7\t+%DA%82 %02 %5C %7C"
    check correct == delta
    # Convert delta string into a diff
    check diffs == text1.fromDelta(delta)

  test "Verify pool of unchanged characters":
    diffs = @[
      (Insert, "A-Z a-z 0-9 - _ . ! ~ * ' ( ) ; / ? : @ & = + $ , # ")
    ]
    text2 = diffText2(diffs)
    correct = "A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # "
    check correct == text2

    delta = diffs.toDelta
    correct = "+A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # "
    check correct == delta
    # Convert delta string into a diff
    check diffs == fromDelta("", delta)

  test "160 kb string":
    var a: string
    when isMainModule:
      a = "abcdefghij"
      for i in 0..13:
        a &= a
    diffs = @[(Insert, a)]
    delta = toDelta diffs
    correct = "+" & a
    check correct == delta
    # Convert delta string into a diff
    check diffs == fromDelta("", delta)

suite "Diff XIndex":
  test "Translate a location in text1 to text2":
    var diffs = @[
      (Delete, "a"),
      (Insert, "1234"),
      (Equal, " xyz ")
    ]
    let correct = 5
    check correct == xIndex(diffs, 2)

  test "Translation on deletion":
    var diffs = @[
      (Equal, "a"),
      (Delete, "1234"),
      (Equal, " xyz ")
    ]
    let correct = 1
    check correct == xIndex(diffs, 3)


suite "Diff Levenshtein":
  test "Levenshtein with trailing equality":
    var diffs = @[
      (Delete, "abc"),
      (Insert, "1234"),
      (Equal, "xyz")
    ]
    let correct = 4
    check correct == levenshtein(diffs)

  test "Levenshtein with leading equality":
    var diffs = @[
      (Equal, "xyz"),
      (Delete, "abc"),
      (Insert, "1234")
    ]
    let correct = 4
    check correct == levenshtein(diffs)

  test "Levenshtein with middle equality":
    var diffs = @[
      (Delete, "abc"),
      (Equal, "xyz"),
      (Insert, "1234")
    ]
    let correct = 7
    check correct == levenshtein(diffs)

suite "Diff Bisect":
  let
    a = "cat"
    b = "map"

  test "Normal":
    let correct = @[
      (Delete, "c"),
      (Insert, "m"),
      (Equal, "a"),
      (Delete, "t"),
      (Insert, "p")
    ]
    check correct == bisect(a, b, high(float))

  test "Timeout":
    let correct = @[
      (Delete, "cat"),
      (Insert, "map")
    ]
    check correct == bisect(a, b, 0.0)

suite "Diff Main - Trivial":
  test "Null case":
    let correct: seq[StringDiff] = @[]
    check correct == makeDiffs("", "", false)

  test "Equality":
    let correct = @[
      (Equal, "abc"),
    ]
    check correct == makeDiffs("abc", "abc", false)


  test "Simple insertion":
    let correct = @[
      (Equal, "ab"),
      (Insert, "123"),
      (Equal, "c")
    ]
    check correct == makeDiffs("abc", "ab123c", false)


  test "Simple insertion":
    let correct = @[
      (Equal, "a"),
      (Delete, "123"),
      (Equal, "bc")
    ]
    check correct == makeDiffs("a123bc", "abc", false)

  test "Two insertions":
    let correct = @[
      (Equal, "a"),
      (Insert, "123"),
      (Equal, "b"),
      (Insert, "456"),
      (Equal, "c")
    ]
    check correct == makeDiffs("abc", "a123b456c", false)

  test "Two deletions":
    let correct = @[
      (Equal, "a"),
      (Delete, "123"),
      (Equal, "b"),
      (Delete, "456"),
      (Equal, "c")
    ]
    check correct == makeDiffs("a123b456c", "abc", false)


suite "Diff Main - Real":
  # Switch off the timeout.
  var dmp = newDiffMatchPatch()
  dmp.diffTimeout = 0
  var correct: seq[StringDiff] = @[]

  test "Simple cases":
    correct = @[
      (Delete, "a"),
      (Insert, "b")
    ]
    check correct == makeDiffs("a", "b", false)

    correct = @[
      (Delete, "Apple"),
      (Insert, "Banana"),
      (Equal, "s are a"),
      (Insert, "lso"),
      (Equal, " fruit.")
    ]
    check correct == makeDiffs("Apples are a fruit.", "Bananas are also fruit.", false)

    correct = @[
      (Delete, "a"),
      (Insert, "\u0680"),
      (Equal, "x"),
      (Delete, "\t"),
      (Insert, "\x00")
    ]
    check correct == makeDiffs("ax\t", "\u0680x\x00", false)

  test "Overlaps":
    correct = @[
      (Delete, "1"),
      (Equal, "a"),
      (Delete, "y"),
      (Equal, "b"),
      (Delete, "2"),
      (Insert, "xab"),
    ]
    check correct == makeDiffs("1ayb2", "abxab", false)

    correct = @[
      (Insert, "xaxcx"),
      (Equal, "abc"),
      (Delete, "y")
    ]
    check correct == makeDiffs("abcy", "xaxcxabc", false)

  test "Overlaps":
    correct = @[
      (Delete, "ABCD"),
      (Equal, "a"),
      (Delete, "="),
      (Insert, "-"),
      (Equal, "bcd"),
      (Delete, "="),
      (Insert, "-"),
      (Equal, "efghijklmnopqrs"),
      (Delete, "EFGHIJKLMNOefg"),
    ]
    check correct == makeDiffs("ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg", "a-bcd-efghijklmnopqrs", false)

  test "Large equality":
    correct = @[
      (Insert, " "),
      (Equal, "a"),
      (Insert, "nd"),
      (Equal, " [[Pennsylvania]]"),
      (Delete, " and [[New"),
    ]
    check correct == makeDiffs("a [[Pennsylvania]] and [[New", " and [[Pennsylvania]]", false)

  test "Timeout":
    dmp.diffTimeout = 0.1
    var
      a = "`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n"
      b = "I am the very model of a modern major general,\nI've information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n"
    for _ in 0 .. 10:
      a &= a
      b &= b
    let startTime = epochTime()
    discard makeDiffs(a, b, params = dmp)
    let endTime = epochTime()
    check dmp.diffTimeout <= endTime - startTime
    check dmp.diffTimeout * 2 > endTime - startTime

suite "Diff Main - Linemode":
  test "Simple line-mode":
    let
      a = "1234567890\n".repeat(13)
      b = "abcdefghij\n".repeat(13)
    check makeDiffs(a, b, false) == makeDiffs(a, b, true)

  test "Single line-mode":
    let
      a = "1234567890".repeat(13)
      b = "abcdefghij".repeat(13)
    check makeDiffs(a, b, false) == makeDiffs(a, b, true)

  test "Overlap line-mode":
    func rebuildTexts(diffs: seq[StringDiff]): (string, string) =
      result = ("", "")
      for diff in diffs:
        if diff.op != Insert:
          result[0] &= diff.text
        if diff.op != Delete:
          result[1] &= diff.text

    const
      a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n"
      b = "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n"
    let
      diffs1 = makeDiffs(a, b, true)
      diffs2 = makeDiffs(a, b, false)
      textsLinemode = rebuildTexts(diffs1)
      textsTextmode = rebuildTexts(diffs2)
    #echo diffs1
    #echo textsLinemode
    #echo diffs2
    #echo textsTextmode
    check textsLinemode == textsTextmode

  # Nim would not allow this
  # test "Null inputs":
  #   expect ValueError:
  #     discard makeDiffs(nil, nil)

suite "Match Alphabet":

  test "Initialize the bitmasks for Bitap":
    let correct = {'a': 4, 'b': 2, 'c': 1}.toTable
    check correct == matchAlphabet("abc")
    let correct2 = {'a': 37, 'b': 18, 'c': 8}.toTable
    check correct2 == matchAlphabet("abcaba")

suite "Match Bitap":
  var dmp = newDiffMatchPatch()
  dmp.matchDistance = 100
  dmp.matchThreshold = 0.5

  test "Exact Matches":
    check bitap("abcdefghijk", "fgh", 5, dmp) == 5
    check bitap("abcdefghijk", "fgh", 0, dmp) == 5

  test "Fuzzy matches":
    check bitap("abcdefghijk", "efxhi", 0, dmp) == 4
    check bitap("abcdefghijk", "cdefxyhijk", 5, dmp) == 2
    check bitap("abcdefghijk", "bxy", 1, dmp) == NotFound

  test "Overflow":
    check bitap("123456789xx0", "3456789x0", 2, dmp) == 2
    check bitap("abcdef", "xxabc", 4, dmp) == 0
    check bitap("abcdef", "defyy", 4, dmp) == 3
    check bitap("abcdef", "xabcdefy", 0, dmp) == 0

  test "Threshold":
    dmp.matchThreshold = 0.4
    check bitap("abcdefghijk", "efxyhi", 1, dmp) == 4
    dmp.matchThreshold = 0.3
    check bitap("abcdefghijk", "efxyhi", 1, dmp) == NotFound
    dmp.matchThreshold = 0.0
    check bitap("abcdefghijk", "bcdef", 1, dmp) == 1
    dmp.matchThreshold = 0.5

  test "Multiple select":
    check bitap("abcdexyzabcde", "abccde", 3, dmp) == 0
    check bitap("abcdexyzabcde", "abccde", 5, dmp) == 8

  test "Distance":
    dmp.matchDistance = 10 # Strict location
    check bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, dmp) == NotFound
    check bitap("abcdefghijklmnopqrstuvwxyz", "abcdxxefg", 1, dmp) == 0

    dmp.matchDistance = 1000 # Loose location
    check bitap("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, dmp) == 0

suite "Match Main":
  var dmp = newDiffMatchPatch()
  dmp.matchDistance = 100
  dmp.matchThreshold = 0.5

  test "Shortcut matches":
    check:
      findMatch("abcdef", "abcdef", 1000) == 0
      findMatch("", "abcdef", 1) == NotFound
      findMatch("abcdef", "", 3) == 3
      findMatch("abcdef", "de", 3) == 3
      findMatch("abcdef", "defy", 4) == 3
      findMatch("abcdef", "abcdefy", 0) == 0

  test "Complex match":
    dmp.matchThreshold = 0.7
    check findMatch("I am the very model of a modern major general.", " that berry ", 5) == 4
    dmp.matchThreshold = 0.5

  # Nim does not allow this
  # test "Null inputs":
  #  expect ValueError:
  #    findMatch(nil, nil, 0)

suite "Patch Object":
  var dmp = newDiffMatchPatch()
  var p = Patch()
  p.start1 = 20
  p.start2 = 21
  p.length1 = 18
  p.length2 = 17
  p.diffs = @[(Equal, "jump"), (Delete, "s"), (Insert, "ed"), (Equal, " over "), (Delete, "the"), (Insert, "a"), (Equal, "\nlaz")]

  test "Object":
    check "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n" == $p

  test "From text":
    let strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n"
    check:
      patchFromText("").len == 0
      $(patchFromText(strp)[0]) == strp
      $(patchFromText("@@ -1 +1 @@\n-a\n+b\n")[0]) == "@@ -1 +1 @@\n-a\n+b\n"
      $(patchFromText("@@ -1,3 +0,0 @@\n-abc\n")[0]) == "@@ -1,3 +0,0 @@\n-abc\n"
      $(patchFromText("@@ -0,0 +1,3 @@\n+abc\n")[0]) == "@@ -0,0 +1,3 @@\n+abc\n"
    expect ValueError:
      discard patchFromText("Bad\nPatch\n")

  test "To text":
    var
      strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"
      p = patchFromText(strp)
    check strp == p.toText

    strp = "@@ -1,9 +1,9 @@\n-f\n+F\n oo+fooba\n@@ -7,9 +7,9 @@\n obar\n-,\n+.\n tes\n"
    p = patchFromText(strp)
    check strp == p.toText

  test "Add context":
    dmp.patchMargin = 4
    p = patchFromText("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")[0]
    addContext(p, "The quick brown fox jumps over the lazy dog.", dmp)
    check "@@ -17,12 +17,18 @@\n fox \n-jump\n+somersault\n s ov\n" == $p

  test "Add context, not enough trailing":
    p = patchFromText("@@ -21,4 +21,10 @@\n-jump\n+somersault\n")[0]
    addContext(p, "The quick brown fox jumps.")
    check "@@ -17,10 +17,16 @@\n fox \n-jump\n+somersault\n s.\n" == $p

  test "Add context, not enough leading":
    p = patchFromText("@@ -3 +3,2 @@\n-e\n+at\n")[0]
    addContext(p, "The quick brown fox jumps.", dmp)
    check "@@ -1,7 +1,8 @@\n Th\n-e\n+at\n  qui\n" == $p

  test "Add context, with ambiguity":
    p = patchFromText("@@ -3 +3,2 @@\n-e\n+at\n")[0]
    addContext(p, "The quick brown fox jumps.  The quick brown fox crashes.", dmp)
    check "@@ -1,27 +1,28 @@\n Th\n-e\n+at\n  quick brown fox jumps. \n" == $p

suite "Patch Make":
  let
    text1 = "The quick brown fox jumps over the lazy dog."
    text2 = "That quick brown fox jumped over a lazy dog."
    diffs = makeDiffs(text1, text2, false)
  var expectedPatch: string

  test "Null case":
    let patches = makePatches("", "")
    check ($patches).len == 0

  test "Text2+Text1 inputs":
    expectedPatch = "@@ -1,8 +1,7 @@\n Th\n-at\n+e\n  qui\n@@ -21,17 +21,18 @@\n jump\n-ed\n+s\n  over \n-a\n+the\n  laz\n"
    check $makePatches(text2, text1) == expectedPatch

  test "Text1+Text2 inputs":
    expectedPatch = "@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"
    check $makePatches(text1, text2) == expectedPatch

  test "Diff input":
    let patches = makePatches(diffs)
    check $patches == expectedPatch

  test "Text1+Diff inputs":
    let patches = makePatches(text1, diffs)
    check $patches == expectedPatch

  test "Text1+Text2+Diff inputs": # deprecated
    let patches = makePatches(text1, text2, diffs)
    check $patches == expectedPatch

  test "Character encoding":
    let patches = makePatches("`1234567890-=[]\\;',./", "~!@#$%^&*()_+{}|:\"<>?")
    check $patches == "@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n"

  test "Character decoding":
    let d = @[
      (Delete, "`1234567890-=[]\\;',./"),
      (Insert, "~!@#$%^&*()_+{}|:\"<>?")
    ]
    check d == patchFromText("@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n")[0].diffs

  test "Long string with repeats":
    let
      t1 = "abcdef".repeat(100)
      t2 = t1 & "123"
      expected = "@@ -573,28 +573,31 @@\n cdefabcdefabcdefabcdefabcdef\n+123\n"
      patches = makePatches(t1, t2)
    check expected == $patches

  # Nim doesn't allow this
  # test "Nil inputs":
  #  expect ValueError:
  #    discard makePatches(nil, nil)

suite "Patch SplitMax":
  var patches: seq[Patch]
  test "SplitMax":
    patches = makePatches("abcdefghijklmnopqrstuvwxyz01234567890", "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0")
    splitMax patches
    check "@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n" == $patches

    patches = makePatches("abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz", "abcdefuvwxyz")
    let oldToText = $patches
    splitMax patches
    check oldToText == $patches

    patches = makePatches("1234567890123456789012345678901234567890123456789012345678901234567890", "abc")
    splitMax patches
    check "@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n" == $patches

    patches = makePatches("abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1", "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1")
    splitMax patches
    check "@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n" == $patches

suite "Patch Add Padding":
  var patches: seq[Patch]
  test "Both edges full":
    patches = makePatches("", "test")
    check "@@ -0,0 +1,4 @@\n+test\n" == $patches
    discard addPatchPadding(patches)
    check "@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n" == $patches

  test "Both edges partial":
    patches = makePatches("XY", "XtestY")
    check "@@ -1,2 +1,6 @@\n X\n+test\n Y\n" == $patches
    discard addPatchPadding(patches)
    check "@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n" == $patches

  test "Both edges none":
    patches = makePatches("XXXXYYYY", "XXXXtestYYYY")
    check "@@ -1,8 +1,12 @@\n XXXX\n+test\n YYYY\n" == $patches
    discard addPatchPadding(patches)
    check "@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n" == $patches

suite "Patch Apply":
  var dmp = newDiffMatchPatch()
  dmp.matchDistance = 1000
  dmp.matchThreshold = 0.5
  dmp.patchDeleteThreshold = 0.5
  var
    patches: seq[Patch]
    results: tuple[text: string, results: seq[bool]]
    patchstr: string

  test "Null case":
    patches = makePatches("", "", dmp)
    results = applyPatches("Hello World.", patches, dmp)
    check "Hello World." == results[0]
    check 0 == results[1].len

  test "Exact match":
    patches = makePatches("The quick brown fox jumps over the lazy dog.", "That quick brown fox jumped over a lazy dog.", dmp)
    results = applyPatches("The quick brown fox jumps over the lazy dog.", patches, dmp)
    check ("That quick brown fox jumped over a lazy dog.", @[true, true]) == results

  test "Partial match":
    results = applyPatches("The quick red rabbit jumps over the tired tiger.", patches, dmp)
    check ("That quick red rabbit jumped over a tired tiger.", @[true, true]) == results

  test "Failed match":
    results = applyPatches("I am the very model of a modern major general.", patches, dmp)
    check ("I am the very model of a modern major general.", @[false, false]) == results

  test "Big delete, small change":
    patches = makePatches("x1234567890123456789012345678901234567890123456789012345678901234567890y", "xabcy", dmp)
    results = applyPatches("x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y", patches, dmp)
    check ("xabcy", @[true, true]) == results

  test "Big delete, big change 1":
    patches = makePatches("x1234567890123456789012345678901234567890123456789012345678901234567890y", "xabcy", dmp)
    results = applyPatches("x12345678901234567890---------------++++++++++---------------12345678901234567890y", patches, dmp)
    check ("xabc12345678901234567890---------------++++++++++---------------12345678901234567890y", @[false, true]) == results

  test "Big delete, big change 2":
    dmp.patchDeleteThreshold = 0.6
    patches = makePatches("x1234567890123456789012345678901234567890123456789012345678901234567890y", "xabcy", dmp)
    results = applyPatches("x12345678901234567890---------------++++++++++---------------12345678901234567890y", patches, dmp)
    check ("xabcy", @[true, true]) == results
    dmp.patchDeleteThreshold = 0.5

  test "Compensate for failed patch":
    dmp.matchThreshold = 0.0
    dmp.matchDistance = 0
    patches = makePatches("abcdefghijklmnopqrstuvwxyz--------------------1234567890", "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890", dmp)
    results = applyPatches("ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890", patches, dmp)
    check ("ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890", @[false, true]) == results
    dmp.matchThreshold = 0.5
    dmp.matchDistance = 1000

  test "No side effects":
    patches = makePatches("", "test", dmp)
    patchstr = $(patches)
    results = applyPatches("", patches, dmp)
    check patchstr == $(patches)

  test "No side effects with major delete":
    patches = makePatches("The quick brown fox jumps over the lazy dog.", "Woof", dmp)
    patchstr = $(patches)
    discard applyPatches("The quick brown fox jumps over the lazy dog.", patches, dmp)
    check patchstr == $(patches)

  test "Edge exact match":
    patches = makePatches("", "test", dmp)
    discard applyPatches("", patches, dmp)
    check ("test", @[true]) == results

  test "Near edge exact match":
    patches = makePatches("XY", "XtestY", dmp)
    results = applyPatches("XY", patches, dmp)
    check ("XtestY", @[true]) == results

  test "Edge partial match":
    patches = makePatches("y", "y123", dmp)
    results = applyPatches("x", patches, dmp)
    check ("x123", @[true]) == results
