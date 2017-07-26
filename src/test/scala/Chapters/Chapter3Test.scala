package Chapters

import DTList._
import org.scalatest.{FreeSpec, Matchers}

class Chapter3Test extends FreeSpec with Matchers {
  // Exercise 3.1 Pattern Matching
  "matchingStuff" in {
    Chapter3.matchingStuff shouldBe 3
  }

  // Exercise 3.2 Tail
  "emptylist" in {
    Chapter3.tail(Nil) shouldBe Nil
  }

  "nonemptyList" in {
     Chapter3.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  // Exercise 3.3 setHead
  "setHeadtoEmptyList" in {
    Chapter3.setHead(1, Nil) shouldBe List(1)
  }

  "setHeadtoNonemptylist" in {
    Chapter3.setHead(1, List(2, 3, 4)) shouldBe List(1, 2, 3 ,4)
  }

  // Exercise 3.4 Drop
  "dropNill" in {
    Chapter3.drop(Nil, 3) shouldBe Nil
  }

  "dropZeroList" in {
    Chapter3.drop(List(1, 2 ,3), 0) shouldBe List(1, 2, 3)
  }

  "nominalDrop" in {
    Chapter3.drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  "negativeDrop" in {
    Chapter3.drop(List(1, 2, 3), -3) shouldBe List(1, 2, 3)
  }

  "tooBigDrop" in {
    Chapter3.drop(List(1, 2, 3), 10) shouldBe Nil
  }

  // Exercise 3.5 DropWhile
  "dropWhileEmpty" in {
    Chapter3.dropWhile(Nil, (a: Int) => a > 2) shouldBe Nil
  }

  "nominalCaseDropWhile" in {
    Chapter3.dropWhile(List(1, 2, 3, 4, 5, 6), (a: Int) => a < 3) shouldBe List(3, 4, 5, 6)
  }

  "dropNothing" in {
    Chapter3.dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a > 100) shouldBe List(1, 2, 3, 4, 5)
  }

  // Exercise 3.6 Init
  "init Nil" in {
    Chapter3.init(Nil) shouldBe Nil
  }

  "init single" in {
    Chapter3.init(List(1)) shouldBe Nil
  }

  "init nominal" in {
    Chapter3.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
  }

  "init short" in {
    Chapter3.init(List(1, 2)) shouldBe List(1)
  }

  // Exercise 3.8: Constructor?
  "does it build a list?" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  // Exercise 3.9: Length
  "calculate the length" in {
    List.foldRight(List(1, 2, 3, 4), 0)((_, b) => b + 1) shouldBe 4
  }

  // Exercise 3.11: Sum, Product and Length
  "foldLeft length" in {
    List.foldLeft(List(1, 2, 3, 4), 0)((b, _) => b + 1) shouldBe 4
  }

  "foldLeft sum" in {
    List.foldLeft(List(1, 2, 3, 4), 0)(_+_) shouldBe 10
  }

  "foldLeft product" in {
    List.foldLeft(List(1, 2, 3, 4), 1)(_*_) shouldBe 24
  }

  // Exervise 3.12: Revers
  "reverse some numbers" in {
    Chapter3.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
  }

  "reverse nil" in {
    Chapter3.reverse(Nil) shouldBe Nil
  }

  // Exercise 3.13: Flip-Flop
  "flipflop sum" in {
    Chapter3.foldLeftRight(List(1, 2, 3, 4), 0)(_+_) shouldBe 10
  }

  // Exercise 3.14: Append
  "append nil to nul" in {
    Chapter3.append(Nil, Nil) shouldBe Nil
  }

  "append list to nil" in {
      Chapter3.append(Nil, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  "append Nil to list" in {
    Chapter3.append(List(1,2), Nil) shouldBe List(1,2)
  }

  "append list to list" in {
    Chapter3.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  // Exercise 3.15: Flatten
  "flatten nil" in {
    Chapter3.flatten(Nil) shouldBe Nil
  }

  "flatten list of nils" in {
    Chapter3.flatten(List(Nil, Nil, Nil)) shouldBe Nil
  }

  "flatten a list of lists with Nils" in {
    Chapter3.flatten(List(List(1,2), Nil, List(4, 5), List(6))) shouldBe List(1, 2, 4, 5, 6)
  }

  // Exercise 3.16: Add1
  "add one to Nil" in {
    Chapter3.add1(Nil) shouldBe Nil
  }

  "add one to normal list" in {
    Chapter3.add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  // Exercise 3.17: mapToString
  "toString nil" in {
    Chapter3.mapToString(Nil) shouldBe Nil
  }

  "toString nominal" in {
    Chapter3.mapToString(List(1.414, 2.71, 3.14)) shouldBe List("1.414", "2.71", "3.14")
  }

  // Exercise 3.18: Map
  "map +1" in {
    Chapter3.map(List(1, 2, 3, 4))(a => a + 1) shouldBe List(2, 3, 4, 5)
  }

  // Exercise 3.19: Filter
  "filter everythingh" in {
    Chapter3.filter(List(1, 2, 3, 4))(a => a > 100) shouldBe Nil
  }

  "filter odd numbers" in {
    Chapter3.filter(List(1, 2, 3, 4, 5, 6, 7, 8))(a => a % 2 == 0) shouldBe List(2, 4, 6, 8)
  }

  // Exercise 3.20
  "given example" in {
    Chapter3.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  // Exercise 3.21
  "newfilter everythingh" in {
    Chapter3.newFilter(List(1, 2, 3, 4))(a => a > 100) shouldBe Nil
  }

  "newfilter odd numbers" in {
    Chapter3.newFilter(List(1, 2, 3, 4, 5, 6, 7, 8))(a => a % 2 == 0) shouldBe List(2, 4, 6, 8)
  }

  // Exercise 3.22: Add elements
  "add elements" in {
    Chapter3.addCoresponding(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  // Exercise 3.23: zipWith
  "zipwith add elements" in {
    Chapter3.zipWith(List(1, 2, 3), List(4, 5, 6))(_+_) shouldBe List(5, 7, 9)
  }

  "zipWith weird" in {
    Chapter3.zipWith(List(1, 2, 3), List(true, false, false))((i, b) => if (b) i.toString else "Nope") shouldBe List("1", "Nope", "Nope")
  }

  // Exercise 3.24: hasSequence
  "hasSequence nominal" in {
    Chapter3.hasSequence(List(1, 2, 3, 4, 5), List(3, 4)) shouldBe true
  }

  "hasseq false" in {
    Chapter3.hasSequence(List(1, 2, 3, 4, 5), List(4, 3)) shouldBe false
  }

  "noway falsefsddfagadfhsfatrw" in {
    Chapter3.hasSequence(List(1, 2, 3, 4, 5),List(10)) shouldBe false
  }

  "match repeating" in {
    Chapter3.hasSequence(List(1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2),List(1, 2, 2)) shouldBe true
  }

  "onemoreforsafety" in {
    Chapter3.hasSequence(List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 1,12, 2, 1, 2, 1, 2,1), List())
  }


  // Exercise 3.25: tree size
  "size 1" in {
    Chapters.Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
  }

  // Exercise 3.26: biggest element
  "nominal" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(8)), Leaf(3))) shouldBe 8
  }

  // Exercise 3.27: depth
  "bush" in {
    Tree.depth(Leaf(5)) shouldBe 1
  }

  "test1" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(8)), Leaf(3))) shouldBe 3
  }

  // Exercise 3.28: Map on trees
  "nomainalas amapa atest" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(8)), Leaf(3)))(v => v + 1) shouldBe Branch(Branch(Leaf(2), Leaf(9)), Leaf(4))
  }

  // Exercise 3.29: Fold the tree
  "fold depth" in {
    Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))((r: Int, l: Int) => (r max l) + 1)(_ => 1) shouldBe 3
  }

  "fold max" in {
    Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))((r: Int, l: Int) => r max l)(identity) shouldBe 3
  }

  "fold map" in {
    Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))((r: Tree[Int], l: Tree[Int]) => Branch(r, l): Tree[Int])(x => Leaf(x + 1)) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
  }
}
