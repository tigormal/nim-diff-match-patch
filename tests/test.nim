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
    check halfMatch("qHilloHelloHew", "xHelloHeHulloy").isNone

suite "Lines to Chars":
  test "Basic Operations":
    check:
      linesToChars("alpha\nbeta\nalpha\n", "beta\nalpha\nbeta\n") ==
        ("\x01\x02\x01", "\x02\x01\x02", @["", "alpha\n", "beta\n"])
      linesToChars("", "alpha\r\nbeta\r\n\r\n\r\n") ==
        ("", "\x01\x02\x03\x03", @["", "alpha\r\n", "beta\r\n", "\r\n"])
      linesToChars("a", "b") == ("\x01", "\x02", @["", "a", "b"])

  # More than 256 to reveal any 8-bit limitations
  test "8-bit limitation":
    let n = 300
    var lineList: seq[string] = @[]
    var charList: seq[Rune] = @[]
    for i in 1 .. n:
      lineList.add($i & "\n")
      charList.add(Rune(i))
    check lineList.len == n
    let lines = lineList.join("")
    let chars = charList.join("")
    check chars.runeLen == n # TODO: check if runeLen should be used in functions
    lineList.insert("", 0)
    var diffs: seq[StringDiff] = @[(DiffOp.Delete, chars)]
    charsToLines(diffs, lineList)
    check diffs == @[(DiffOp.Delete, lines)]

  # More than 1,114,112 to verify any 17 * 16-bit limitation.
  test "16-bit limitation":
    var lineList: seq[string] = @[]
    for i in 1 .. 1115000:
      lineList.add($i & "\n")

    let chars = lineList.join("")
    let results = linesToChars(chars, "")
    var diffs = @[(DiffOp.Insert, results[0])]
    charsToLines(diffs, results[2])
    check chars == diffs[0][1]

suite "Cleanup Merge":
  test "No change case":
    var diffs = @[(Equal, "a"), (Delete, "b"), (Insert, "c")]
    cleanupMerge diffs
    check diffs == @[(Equal, "a"), (Delete, "b"), (Insert, "c")]

  test "Merge equalities":
    var diffs = @[(Equal, "a"), (Equal, "b"), (Equal, "c")]
    cleanupMerge diffs
    check diffs == @[(Equal, "abc")]

  test "Merge deletions":
    var diffs = @[(Delete, "a"), (Delete, "b"), (Delete, "c")]
    cleanupMerge diffs
    check diffs == @[(Delete, "abc")]

  test "Merge insertions":
    var diffs = @[(Insert, "a"), (Insert, "b"), (Insert, "c")]
    cleanupMerge diffs
    check diffs == @[(Insert, "abc")]

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
    cleanupMerge diffs
    check diffs == @[(Delete, "ac"), (Insert, "bd"), (Equal, "ef")]

  test "Prefix and suffix detection":
    var diffs = @[(Delete, "a"), (Insert, "abc"), (Delete, "dc")]
    cleanupMerge diffs
    check diffs == @[(Equal, "a"), (Delete, "d"), (Insert, "b"), (Equal, "c")]

  test "Prefix and suffix detection with equalities":
    var diffs =
      @[(Equal, "x"), (Delete, "a"), (Insert, "abc"), (Delete, "dc"), (Equal, "y")]
    cleanupMerge diffs
    check diffs == @[(Equal, "xa"), (Delete, "d"), (Insert, "b"), (Equal, "cy")]

  test "Slide edit left":
    var diffs = @[(Equal, "a"), (Insert, "ba"), (Equal, "c")]
    cleanupMerge diffs
    check diffs == @[(Insert, "ab"), (Equal, "ac")]

  test "Slide edit right":
    var diffs = @[(Equal, "c"), (Insert, "ab"), (Equal, "a")]
    cleanupMerge diffs
    check diffs == @[(Equal, "ca"), (Insert, "ba")]

  test "Slide edit left recursive":
    var diffs =
      @[(Equal, "a"), (Delete, "b"), (Equal, "c"), (Delete, "ac"), (Equal, "x")]
    cleanupMerge diffs
    check diffs == @[(Delete, "abc"), (Equal, "acx")]

  test "Slide edit right recursive":
    var diffs =
      @[(Equal, "x"), (Delete, "ca"), (Equal, "c"), (Delete, "b"), (Equal, "a")]
    cleanupMerge diffs
    check diffs == @[(Equal, "xca"), (Delete, "cba")]

  test "Empty merge":
    var diffs = @[(Delete, "b"), (Insert, "ab"), (Equal, "c")]
    cleanupMerge diffs
    check diffs == @[(Insert, "a"), (Equal, "bc")]

  test "Empty equality":
    var diffs = @[(Equal, ""), (Insert, "a"), (Equal, "b")]
    cleanupMerge diffs
    check diffs == @[(Insert, "a"), (Equal, "b")]
