package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("combine of empty list") {
    assert(combine(Nil) === Nil)
  }

  test("combine of a singleton list") {
    new TestTrees {
      assert(combine(List(t1)) === List(t1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("sort a List of characters") {
    assert(sort[Char]("deafbca".toList, (x,y) => x < y) === "aabcdef".toList)
    assert(sortChars("deafbca".toList) === "aabcdef".toList)
  }

  test("sort a List of CodeTrees") {
    assert(sort[CodeTree](List(Leaf('b',2), Leaf('a',1)), (x,y) => weight(x) < weight(y)) === List(Leaf('a',1), Leaf('b',2)))
    assert(sortCodeTreesByWeight(List(Leaf('b',2), Leaf('a',1))) === List(Leaf('a',1), Leaf('b',2)))
  }

  test("count a char occurrence in a List[Char]") {
    assert(countChar('a', "abcda".toList) == 2)
  }

  test("zip two lists"){
    assert(zip[Char,Int](List('a','b'), List(1,2)) === List(('a',1), ('b',2)))
  }

  test("uniq elements in a list") {
    assert(uniq(List(1,2,2,3,3)) === List(1,2,3))
  }

  test("until") {
    assert(until(singleton,combine)(List(Leaf('a',1),Leaf('b',2))) === List(Fork(Leaf('a',1),Leaf('b',2),List('a', 'b'),3)))
  }

  test("createCodeTree") {
    assert(createCodeTree("bab".toList) === Fork(Leaf('a',1),Leaf('b',2),List('a', 'b'),3))
  }

  test("decode") {
    assert(decode(createCodeTree("abcaba".toList), List(0,1,0,1,1,0,1,0,0)) === "abcaba".toList)
  }

  test("decode secret") {
    assert(decode(frenchCode, secret) === List('h','u','f','f','m','a','n','e','s','t','c','o','o','l'))
  }

  test("encode decode secret") {
    val decoded = List('h','u','f','f','m','a','n','e','s','t','c','o','o','l')
    assert(decode(frenchCode, encode(frenchCode)(decoded)) === decoded)
  }

  test("reverse") {
    assert(reverse[Char]("abc".toList) === "cba".toList)
  }

  test("codeBits") {
    val t1 = List(('a',List(0)), ('b',List(1,0)), ('c',List(1,1)))
    assert(codeBits(t1)('c') === List(1,1))
  }

  test("convert") {
    assert(convert(createCodeTree("aaabbc".toList)) === List(('a',List(0)), ('b',List(1,0)), ('c',List(1,1))))
  }

  test("quickEncode") {
    val decoded = "aaabbc".toList
    val codeTree = createCodeTree(decoded)
    assert(quickEncode(codeTree)(decoded) === encode(codeTree)(decoded))
  }
}
