package annotator

import org.scalatest._

import scala.collection.immutable.IntMap
import scala.collection.immutable.Set

class AnnotatorSpec extends FunSuite {

  test("mkIndexPairSeq") {
    assertResult(
      IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
    ) {
      val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
      Annotator.mkIndexPairSeq(textMap)
    }
  }

  test("mkIndexPairMap with indexPairSeq") {

    assertResult(
      IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3)) 
    ) {
      val indexPairSeq = IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
      val bIndexPairSet = Set((3,6), (5,1))
      Annotator.mkIndexPairMap(indexPairSeq, bIndexPairSet)
    }
  }

  test("mkIndexPairMap with textMap") {
    assertResult(
      IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3)) 
    ) {
      val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
      val bIndexPairSet = Set((3,6), (5,1))
      Annotator.mkIndexPairMap(textMap, bIndexPairSet)
    }
  }

  test("mkTextWithBreaks") {
    val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
    val break = ' '
    assertResult("abcde fghi") {
      val bIndexPairSet = Set(5 -> 0) 
      Annotator.mkTextWithBreaks(textMap, bIndexPairSet, break)
    }
    assertResult("abcde fghi") {
      val bIndexPairSet = Set(3 -> 7, 5 -> 0, 5 -> 3) 
      Annotator.mkTextWithBreaks(textMap, bIndexPairSet, break)
    }
  }

}
