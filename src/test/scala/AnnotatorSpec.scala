package annotator

import org.scalatest._

import scala.collection.immutable.IntMap
import scala.collection.immutable.Set
import scala.collection.immutable.Map
import scala.collection.immutable.ListMap

import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

class AnnotatorSpec extends FlatSpec {

  val dom = new Document()

  val e = {
    val _e = new Element("svg")
      .setAttribute("version", "1.1") 
      .setAttribute("width", "612px")
      .setAttribute("height", "3168px")
      .setAttribute("viewBox", "0 0 612 3168")
     dom.setRootElement(_e)
     _e
  }

  val e_1 = {
    val _e = new Element("g")
      .setAttribute("transform", "matrix(1 0 0 -1 0 792)")
    e.addContent(_e)
    _e
  } 

  val e_1_1 = {
    val _e = new Element("text")
      .setAttribute("transform", "matrix(0 1 -1 0 32 256) scale(1, -1)")
    e_1.addContent(_e)
    _e
  }

  val e_1_1_1 = {
    val _e = new Element("tspan")
      .setAttribute("x", "0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1")
      .setAttribute("endX", "100.2")
      .setAttribute("y", "0")
      .setAttribute("font-size", "20px")
      .setText("abcdefghijkl")
    e_1_1.addContent(_e)
    _e
  }

  val e_1_2 = {
    val _e = new Element("text")
      .setAttribute("transform", "translate(136.8 669.12) scale(1, -1)")
    e_1.addContent(_e)
    _e
  }

  val e_1_2_1 = {
    val _e = new Element("tspan")
      .setAttribute("x", "0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83")
      .setAttribute("endX", "112.43")
      .setAttribute("y", "0")
      .setAttribute("font-size", "17.2154px")
      .setText("abcdefghijklmno")
    e_1_2.addContent(_e)
    _e
  }

  val e_1_2_2 = {
    val _e = new Element("tspan")
      .setAttribute("x", "137.16 146.75 154.43 164.03 171.71 186.11")
      .setAttribute("endX", "191.22")
      .setAttribute("y", "19.91")
      .setAttribute("font-size", "17.2154px")
      .setText("abcdef")
    e_1_2.addContent(_e)
    _e
  }

  "fontSize" should "raise exception if element has no attribute font-size" in {
    intercept[NullPointerException] {
      Annotator.fontSize(e)
    }
  }

  it should "return a double value if element has attribute font-size with value \"[some numer]px\"" in {
    assertResult(20) {
      Annotator.fontSize(e_1_1_1)
    }

    assertResult(17.2154) {
      Annotator.fontSize(e_1_2_1)
    }
  }

  "y" should "raise exception if element has no attribtue y" in {
    intercept[NullPointerException] {
      Annotator.y(e_1_1)
    }
  }

  it should "return a double value if element has attribute y with numeric value" in {
    assertResult(0) {
      Annotator.y(e_1_1_1)
    }

    assertResult(19.91) {
      Annotator.y(e_1_2_2)
    }

  }

  "xs" should  "raise exception if element has no attribute x" in {
    intercept[NullPointerException] {
      Annotator.y(e_1_1)
    }
  }

  it should "return a list of doubles if element has attribute x with value as space separated list of numbers" in {
    assertResult(List(0, 8.88, 15.54, 29.98, 35.54, 45.54, 51.1, 61.1, 71.1, 81.1, 91.1, 96.1)) {
      Annotator.xs(e_1_1_1)
    }
  }

  "commonAncestor" should "raise exception if either argument is null" in {
    intercept[IllegalArgumentException] {
      Annotator.commonAncestor(null, null)
    }
  }

  it should "raise exception if arguments are in different trees" in {
    intercept[IllegalArgumentException] {
      Annotator.commonAncestor(new Element("a"), new Element("b"))
    }
  }

  it should "raise exception if arguments are of different depths in the same tree" in {
    intercept[IllegalArgumentException] {
      Annotator.commonAncestor(e_1_1, e_1_2_1)
    }
  }

  it should "return the most recent common ancestor, otherwise" in {
    assertResult(e_1_2) {
      Annotator.commonAncestor(e_1_2_1, e_1_2_2)
    }

    assertResult(e_1) {
      Annotator.commonAncestor(e_1_1_1, e_1_2_1)
    }

  }

  "getTransformedCoords" should "raise exception if the first argument is missing y, endX, or x attributes" in {
    intercept[NullPointerException] {
      Annotator.getTransformedCoords(e_1_1, e)
    }
  }

  it should "return a PositionGroup, otherwise" in {
    assertResult(
      Annotator.PositionGroup(
        List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
        32.0,
        List(536.0, 527.12, 520.46, 506.02, 500.46, 490.46, 484.9, 474.9, 464.9, 454.9, 444.9, 439.9)
      )
    ) {
      Annotator.getTransformedCoords(e_1_1_1, e)
    }

    assertResult(
      Annotator.PositionGroup(
        List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
        32.0,
        List(256.0, 264.88, 271.54, 285.98, 291.54, 301.54, 307.1, 317.1, 327.1, 337.1, 347.1, 352.1)
      )
    ) {
      Annotator.getTransformedCoords(e_1_1_1, e_1_1)
    }
    
    assertResult(
      Annotator.PositionGroup(
        List(137.16, 146.75, 154.43, 164.03, 171.71, 186.11),
        191.22,
        List(19.91, 19.91, 19.91, 19.91, 19.91, 19.91)
      )
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e_1_2_2)
    }

    assertResult(
      Annotator.PositionGroup(
        List(273.96000000000004, 283.55, 291.23, 300.83000000000004, 308.51, 322.91),
        328.02,
        List(142.79, 142.79, 142.79, 142.79, 142.79, 142.79)
      )
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e)
    }
  }

  it should "not make transformations for ancestral elements lacking transform attributes" in {
    assertResult(
      Annotator.getTransformedCoords(e_1_2_2, e)
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e_1)
    }
  }

  "mkIndexPairSeq" should "return a sequence of int pairs" in {
    assertResult(
      IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
    ) {
      val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
      Annotator.mkIndexPairSeq(textMap)
    }
  }

  "mkIndexPairMap" should "return a map of ints to int pairs, when first argument is an indexPairSeq" in {

    assertResult(
      IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3)) 
    ) {
      val indexPairSeq = IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
      val bIndexPairSet = Set((3,6), (5,1))
      Annotator.mkIndexPairMap(indexPairSeq, bIndexPairSet)
    }
  }

  it should "return a map of ints to int pairs, when first argument is a textmap" in {
    assertResult(
      IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3)) 
    ) {
      val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
      val bIndexPairSet = Set((3,6), (5,1))
      Annotator.mkIndexPairMap(textMap, bIndexPairSet)
    }
  }

  val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
  val break = ' '
  "mkTextWithBreaks" should "return a string of text with extra characters inserted" in {
    assertResult("abcde fghi") {
      val bIndexPairSet = Set(5 -> 0) 
      Annotator.mkTextWithBreaks(textMap, bIndexPairSet, break)
    }
  }

  it should "only add characters at provided position if the position marks the beginning of a text group" in {
    assertResult("abcde fghi") {
      val bIndexPairSet = Set(3 -> 7, 5 -> 0, 5 -> 3) 
      Annotator.mkTextWithBreaks(textMap, bIndexPairSet, break)
    }
  }


  //Annotator Instances

  import Annotator._
  val annotator = new Annotator(dom)

  val quailTable = (annotator.getBIndexPairSet(Single(CharCon)).zipWithIndex.toMap.map {
    case (indexPair, i) =>
      val charIndex = indexPair._2
      indexPair -> (if (i % 4 == 0) {
        B('q')
      } else if ((i - 3) % 4 == 0) {
        L
      } else {
        I
      })
  }) + ((2,5) -> U('q'))

  val annotator2 = annotator.annotate(List("quail" -> 'q'), Single(CharCon), quailTable)

  val falconTable: Map[(Int, Int), Label] = annotator2.getBIndexPairSet(Single(SegmentCon("quail")))
    .filter(indexPair => indexPair._2 % 8 == 0)
    .zipWithIndex.toMap.map {
      case (indexPair, i) =>
        val charIndex = indexPair._2
        indexPair -> (if (i % 2 == 0) {
          B('f')
        } else if ((i - 1) % 2 == 0) {
          L
        } else {
          I
        })
    }


  val annotator3 = annotator2.annotate(List("falcon" -> 'f'), Single(SegmentCon("quail")), falconTable)


  val penguinTable = annotator3.getBIndexPairSet(Single(SegmentCon("quail")))
    .filter(indexPair => indexPair._2 % 6 == 0)
    .map(ip => ip -> U('p')).toMap

  val annotator4 = annotator3.annotate(List("penguin" -> 'p'), Single(SegmentCon("quail")), penguinTable)


  "new Annotator" should "create an Annotator instance with annotationBlockSeq populated without any annotations" in {

    val emptyMap = ListMap[AnnotationType, AnnotationSpan]()
    val expectedResults = IndexedSeq(
        AnnotationBlock(0,12, emptyMap),
        AnnotationBlock(12,27, emptyMap), 
        AnnotationBlock(27,33, emptyMap)
   )

    (0 to 2).map(i => {
      val expected = expectedResults(i)
      val actual =  annotator.annotationBlockSeq(i)
      assertResult(expected)(actual)
    })

  }

  it should "create annotator such that the next index minus start index of each annotation block " + 
  "equals the text length of the corresponding element produced by getElements() " in {

    val es = annotator.getElements()

    es.toIndexedSeq.zipWithIndex.map {
      case (e, i) =>
        val annoBlock = annotator.annotationBlockSeq(i)
        val expected = e.getText().size 
        val actual = annoBlock.nextIndex - annoBlock.startIndex
        assertResult(expected)(actual)
    }

  }

  "annotate" should "raise exception if called with an annotation type that already exists" in {
    val table = annotator.getBIndexPairSet(Single(CharCon)).map(_ -> U('q')).toMap
    intercept[AssertionError] {
      annotator4.annotate(List("quail" -> 'q'), Single(CharCon), table)
    }
  }

  it should "raise exception if called with a constraint range containing an annotation type does not exist in annotator" in {
    intercept[NoSuchElementException] {
      annotator.annotate(List("quail" -> 'q'), Single(SegmentCon("xyz")), quailTable)
    }
  }


  it should "raise exception if called with a constraint range where" +
  " the range's annotation type does not descend from the constraint" in {
    val table = annotator.getBIndexPairSet(Single(CharCon)).map(_ -> U('x')).toMap
    intercept[IllegalArgumentException] {
      annotator4.annotate(List("xyz" -> 'x'), Range("falcon", SegmentCon("penguin")), table)
    }
  }


  it should "create an Annotator instance with annotationBlockSeq populated with some annotations" in {

    val expected = {
      AnnotationBlock(
          0,12,
          ListMap(
              AnnotationType("quail", 'q', Single(CharCon)) -> 
              AnnotationSpan(
                  IntMap(
                      0 -> B('q'), 1 -> I, 2 -> I, 3 -> L, 
                      4 -> B('q'), 5 -> I, 6 -> I, 7 -> L, 
                      8 -> B('q'), 9 -> I, 10 -> I, 11 -> L
                  ),
                  Seq(AnnotationType("quail", 'q', Single(CharCon)))
              )
          )
      )
    }

    val actual = annotator2.annotationBlockSeq(0)
    assertResult(expected)(actual)
  }


  "getBIndexPairSet" should "raise an exception if the constraint range specifies a non existent annotation type" in {
    intercept[NoSuchElementException] {
      annotator.getBIndexPairSet(Range("xyz", CharCon))
    }
  }

  it should "raise exception if range's annotation type does not descend from the constraint" in {

    intercept[IllegalArgumentException] {
      annotator4.getBIndexPairSet(Range("falcon", SegmentCon("penguin")))
    }
  }

  it should "produce all index pairs if the constraint is CharCon" in {
    assertResult(Set(
        (0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), 
        (0,7), (0,8), (0,9), (0,10), (0,11), (1,0), 
        (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7), 
        (1,8), (1,9), (1,10), (1,11), (1,12), (1,13), 
        (1,14), (2,0), (2,1), (2,2), (2,3), (2,4), (2,5)
    )) {
      annotator.getBIndexPairSet(Single(CharCon))
    }
  }

  it should "produce index pairs on Single of segment con of existing annotation type" in {
    assertResult(Set((0,0), (0,4), (0,8), (1,0), (1,4), (1,8), (1,12), (2,1), (2,5))) {
      annotator2.getBIndexPairSet(Single(SegmentCon("quail")))
    }
  }

  it should "produce index pairs on a Range where the annotation type descends from the constraint" in {

    assertResult(Set((0,0), (1,0))) {
      annotator3.getBIndexPairSet(Single(SegmentCon("falcon")))
    }

    assertResult(Set((0,0), (0,8), (1,0), (1,8))) {
      annotator3.getBIndexPairSet(Range("falcon", SegmentCon("quail")))
    }

    assertResult(Set(
        (0,0), (0,1), (0,2), (0,3), (0,8), (0,9), (0,10), (0,11),
        (1,0), (1,1), (1,2), (1,3), (1,8), (1,9), (1,10), (1,11)
    )) {
      annotator3.getBIndexPairSet(Range("falcon", CharCon))
    }

    assertResult(Set((0,0), (1,0), (1,12))) {
      annotator4.getBIndexPairSet(Single(SegmentCon("penguin")))
    }
  }





}
