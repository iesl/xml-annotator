package annotator

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.Writer

import org.jdom2.Content
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import scala.collection.JavaConversions.iterableAsScalaIterable 
import scala.collection.immutable.IntMap
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet

import org.jdom2.output.Format
import org.jdom2.output.XMLOutputter
import org.jdom2.output.LineSeparator
import org.jdom2.output.support.AbstractXMLOutputProcessor

/** Functions and Constructors for transforming Annotator instance values **/
object Annotator {

  type Segment = IntMap[IntMap[Label]]
  type Element = org.jdom2.Element
  type ElementFilter = org.jdom2.filter.ElementFilter
  type AnnotationLink = Map[String, (Int, Int)]

  /** Constructors for Label
    *
    * BIOLU represent a text character's position in an annotation
    */
  sealed trait Label
  case class B(c: Char) extends Label
  case object I extends Label
  case object O extends Label
  case object L extends Label
  case class U(c: Char) extends Label

  /** Constructor for AnnotationType **/
  case class AnnotationType(name: String, c: Char, constraintRange: ConstraintRange)


  /** Constructors for Constraint
    *
    * CharCon is a constraint on the primitive unit of characters 
    * SegmentCon is a constraint on a segment of a specified annotation type
    */
  sealed trait Constraint
  case object CharCon extends Constraint
  case class SegmentCon(annotationTypeName: String) extends Constraint

  /** Constructors for ConstraintRange 
    *
    * Range takes an annotation type string and a constraint such that
    * the annotation type is associated with the constraint 
    * or is associated with a SegmentCon constraint whose type 
    * has these same qualities just mentioned of the initial annotation type 
    */
  sealed trait ConstraintRange
  case class Range(annoTypeName: String, con: Constraint) extends ConstraintRange
  case class Single(constraint: Constraint) extends ConstraintRange

  /** Constructor for AnnotationSpan **/
  case class AnnotationSpan(
    labelMap: IntMap[Label], 
    annotationTypeSeq: Seq[AnnotationType]
  )

  /** Constructor for AnnotationInfo **/
  case class AnnotationInfo(annotationType: AnnotationType, bIndexPairSortedSet: SortedSet[(Int, Int)])

  /** Constructor for AnnotationBlock **/
  case class AnnotationBlock(startIndex: Int, nextIndex: Int, annotationMap: ListMap[AnnotationType, AnnotationSpan])

  /** Function to get all non-empty tspan elements **/
  private def getElements(dom: Document): Iterable[Element] = {
    dom.getRootElement().getDescendants(new ElementFilter("tspan")).toIterable.filter(e => {
      e.getText().size > 0
    })
  }

  /** Function to get element's font size **/
  def fontSize(e: Element): Double = {
    e.getAttribute("font-size").getValue().dropRight(2).toDouble
  }

  /** Function to get element's y position **/
  def y(e: Element): Double = {
    e.getAttribute("y").getValue().toDouble 
  }

  /** Function to get element's x positions **/
  def xs(e: Element): Array[Double] = {
    e.getAttribute("x").getValue().split(" ").map(_.toDouble) 
  }

  /** Function to get element's last character's right edge position **/
  def endX(e: Element): Double = {
    e.getAttribute("endX").getValue().toDouble 
  }

  /** Function to get the most recent common ancestor of two elements **/
  def commonAncestor(e1: Element, e2: Element): Element = {
    require(e1 != null && e2 != null, "one of the elements has invalid null value")
    if (e1 == e2) {
      e1
    } else {
      commonAncestor(e1.getParentElement(), e2.getParentElement())
    }
  }

  //svg matrix is defined at http://www.w3.org/TR/SVG/coords.html#EstablishingANewUserSpace
  //and at https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
  private type SvgMatrix = Array[Double]

  /** Function to multiply two svg matrices **/
  private def svgMatrixMultiply(m1: SvgMatrix, m2: SvgMatrix): SvgMatrix = {
    require(m1.size == 6 && m2.size == 6, "one or more SvgMatrix has invalid size instead of size of 6")

    val _0 = m1(0) * m2(0) + m1(2) * m2(1) 
    val _1 = m1(1) * m2(0) + m1(3) * m2(1)
    val _2 = m1(0) * m2(2) + m1(2) * m2(3) 
    val _3 = m1(1) * m2(2) + m1(3) * m2(3)
    val _4 = m1(0) * m2(4) + m1(2) * m2(5) + m1(4) 
    val _5 = m1(1) * m2(4) + m1(3) * m2(5) + m1(5) 
    Array(_0, _1, _2, _3, _4, _5)
  }

  /** Function to extract svg matrix from element's transform attribute **/
  private def svgMatrix(e: Element): SvgMatrix = {

    val identity = Array(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    def translate2Matrix(array: Array[Double]) = Array(1.0, 0.0, 0.0, 1.0, array(0), array(1))
    def scale2Matrix(array: Array[Double]) = Array(array(0), 0.0, 0.0, array(1), 0.0, 0.0)

    Option(e.getAttribute("transform")) match {
      case Some(attr) if !attr.getValue().isEmpty  =>
        attr.getValue().split("((?<=\\))[\\s,]+)").map(str => {
          val firstN = str.indexOf("(") + 1
          val lastN = str.size - str.indexOf(")")
          val doubleArray = str.drop(firstN).dropRight(lastN).split("[\\s,]+").map(_.toDouble)
          if (str.startsWith("matrix(")) {
            assert(doubleArray.size == 6, "svg matrix has invalid size")
            doubleArray
          } else if (str.startsWith("translate(")) {
            assert(doubleArray.size == 2, "svg translate has invalid size")
            translate2Matrix(doubleArray)
          } else if (str.startsWith("scale(")) {
            assert(doubleArray.size == 2, "svg scale has invalid size")
            scale2Matrix(doubleArray)
          } else {
            identity
          }
        }).foldLeft(identity) {
          case (mAcc, m) => svgMatrixMultiply(mAcc, m)
        }
      case _ => 
        identity
    }
  }

  case class PositionGroup(xs: List[Double], endX: Double, ys: List[Double]) 

  /** Function to get source element's x and y positions in its ancestor's coordinate system **/
  def getTransformedCoords(sourceE: Element, ancestorE: Element): PositionGroup = {

    def matrixTotal(e: Element): SvgMatrix = {
      require(e != null)
      val m = svgMatrix(e)
      if (e == ancestorE) {
        m
      } else {
        svgMatrixMultiply(matrixTotal(e.getParentElement()), m)
      }
    }

    val m = matrixTotal(sourceE)
    val sourceXs = xs(sourceE)
    val sourceY = y(sourceE)

    val _xs = sourceXs.map(x => {
      m(0) * x + m(2) * sourceY + m(4)
    })

    val _ys = sourceXs.map(x => {
      m(1) * x + m(3) * sourceY + m(5)
    })

    val _endX = m(0) * endX(sourceE) + m(2) * sourceY + m(4)

    PositionGroup(_xs.toList, _endX, _ys.toList)

  }

  /** Function to make a sequence of int pairs from a map of int to int and string  
    *
    * example:
    * input: { 3 -> (4, "abcde"), 5 -> (0, "fghi") }
    * output: [ (3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3) ]  
    *
    */
  final def mkIndexPairSeq(textMap: IntMap[(Int, String)]): IndexedSeq[(Int, Int)] = {
    textMap.toIndexedSeq.flatMap {
      case (_blockIndex, (_charIndex, text)) =>
        (0 until text.size).map(i => _blockIndex -> (_charIndex + i))
    }
  }


  /** Function to make a map of ints to int pairs  
    *
    * param indexPairSeq specifies the original position of every int pair
    * param bIndexPairSet specifies the int pair whose position is to be incremented 
    * and whose following int pairs' positions are to be incremented
    *
    * example:
    * input: indexPairSeq = [ (3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3) ]  
    *        bIndexPairSet = { (3,6), (5,1) } 
    * output: { 0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3) }  
    *
    */
  final def mkIndexPairMap(indexPairSeq: IndexedSeq[(Int,Int)], bIndexPairSet: Set[(Int, Int)]): IntMap[(Int, Int)] = {
    indexPairSeq.foldLeft(IntMap[(Int,Int)]()) {
      case (mapAcc, indexPair) =>
        val key = if (mapAcc.isEmpty) 0 else mapAcc.lastKey + 1
        mapAcc + (if (bIndexPairSet.contains(indexPair)) {
          (key + 1) -> indexPair
        } else {
          key -> indexPair
        })
    }
  }

  /** Function to make a map of ints to int pairs  
    *
    * example:
    * input: textMap = { 3 -> (4, "abcde"), 5 -> (0, "fghi") }
    *        bIndexPairSet = { (3,6), (5,1) } 
    * output: { 0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3) }  
    *
    */
  final def mkIndexPairMap(textMap: IntMap[(Int, String)], bIndexPairSet: Set[(Int, Int)]): IntMap[(Int, Int)] = {
    val indexPairSeq = mkIndexPairSeq(textMap)
    mkIndexPairMap(indexPairSeq, bIndexPairSet)
  }

  /** Function to make a string of text with specified char inserted at specified locations 
    *
    * example:
    * input: textMap = { 3 -> (4, "abcde"), 5 -> (0, "fghi") }
    *        bIndexPairSet = { (5,0) } 
    *        break = ' '
    * output: "abcde fghi"
    *
    */
  final def mkTextWithBreaks(textMap: IntMap[(Int, String)], bIndexPairSet: Set[(Int, Int)], break: Char = '\n'): String = {
    textMap.foldLeft("") {
      case (strAcc, (blockIndex, (charIndex, text))) =>
        if (bIndexPairSet.contains(blockIndex -> charIndex)) {
          strAcc + break + text
        } else {
          strAcc + text
        }

    }
  }


}

import Annotator._
/**
  * Private Constructor for Annotator, which stores annotations associated with xml data
  *
  * instance has methods and attributes for retrieving annotation and xml data and 
  * adding new annotations
  *
  */
class Annotator private (
    /** Original mutable object containing xml data**/
    private val dom: Document, 
    /** Sequence of AnnotationBlocks **/
    val annotationBlockSeq: IndexedSeq[AnnotationBlock], 
    /** Map of AnnotationType string to AnnotationInfo **/
    val annotationInfoMap: Map[String, AnnotationInfo],
    /** Set of AnnotationLink **/
    val annotationLinkSet: Set[AnnotationLink] 
) {

  /** Public Constructor **/
  def this(dom: Document) = this(
    dom,
    Annotator.getElements(dom).foldLeft(IndexedSeq[AnnotationBlock]())( (seqAcc, e) => {
      val startIndex = if (seqAcc.isEmpty) 0 else seqAcc.last.nextIndex
      val nextIndex = startIndex + e.getText().size
      seqAcc :+ AnnotationBlock(startIndex, nextIndex, ListMap())
    } ),
    HashMap(),
    HashSet()
  )

  /** Clone of dom that is never passed outside of instance, therefore always unmutated **/
  private val frozenDom = dom.clone()

  /** Clone of dom for passing outside **/
  private var _dom: Document = frozenDom.clone()

  /** Function to replace _dom with a certainly clean unmutated dom **/
  final def resetDom(): Unit = {
    _dom = frozenDom.clone()
  }

  /** Function to get dom externally **/
  final def getDom(): Document = _dom

  /** Function to get all non empty tspan elements externally **/
  final def getElements(): Iterable[Element] = Annotator.getElements(_dom)

  /** Function to produce a string representation of an AnnotationSpan **/
  private def renderAnnotation(a: AnnotationSpan, length: Int): String = {

    val posi = (0 until length).foldLeft("")((stringAcc, i) => {
      stringAcc + (a.labelMap.get(i) match {
        case Some(B(char)) => char.toLower
        case Some(U(char)) => char.toUpper
        case Some(I) => '~'
        case Some(O) => '-'
        case Some(L) => '$'
        case None => ' '
      })
    })

    val constr =  ", constraint: " + {
      val constraintRange = a.annotationTypeSeq(0).constraintRange
      a.annotationTypeSeq.foreach(annoType => {
        assert(annoType.constraintRange == constraintRange, "annotationTypeSeq has inconsistent constraints")
      })
      def loop(cr: ConstraintRange): String = {
        cr match {
          case Single(CharCon) => 
            "char"
          case Single(SegmentCon(annotationTypeName)) => 
            annotationTypeName
          case Range(annotationTypeName, end) => 
            val annotationType = annotationInfoMap(annotationTypeName).annotationType
            val con = annotationType.constraintRange match {
              case Single(c) => c
              case Range(_, c) => c
            }

            if (con == end) {
              annotationTypeName + "." + loop(Single(end))
            } else {
              con match {
                case CharCon => 
                  assert(false)
                  annotationTypeName + "." + loop(Single(con))
                case SegmentCon(_annoTypeName) =>
                  annotationTypeName + "." + loop(Range(_annoTypeName, end))
              }
            }
        }
      }
      loop(constraintRange)
    }

    val annot = {
      "type: " + "{" + a.annotationTypeSeq.map(at => {
        at.name + ": " + at.c
      }).mkString(", ") + "}"
    }

    "| |" + posi + "| " + "{" + annot + constr + "}"

  }

  /** Function to produce a string representation of an AnnotationBlock **/
  private def renderAnnotationBlock(bb: AnnotationBlock): String = {
    val next = bb.nextIndex

    val height = (next - 1).toString.size

    val topRulerList = (height to 2 by -1).map(level => {
      "| |"+(bb.startIndex until next).map(i => {
        if (i == bb.startIndex || (i % 10) == 0){
          val divisor = Math.pow(10,level).toInt
          val digit = (i % divisor)/(divisor/10)
          if (digit == 0 && level == height) " " else digit
        } else " "
      }).mkString("")+"|"
    })

    val bottomRuler = "| |" + (bb.startIndex until next).map(_ % 10).mkString("") + "|"

    val ruler = (topRulerList :+ bottomRuler).mkString("\n")

    "\n" + bb.annotationMap.values.toList.reverse.distinct.map(renderAnnotation(_, (next - bb.startIndex))).mkString("\n") + "\n" + ruler + "\n "
  }

  /** Function to create a new AnnotationBlock with an additional annotationSpan **/
  private def addAnnotation(annotationSpan: AnnotationSpan, annotationBlock: AnnotationBlock): AnnotationBlock = { 
    require(annotationSpan.labelMap.lastKey < annotationBlock.nextIndex, "annotationSpan is too long for annotationBlock")
    annotationSpan.annotationTypeSeq.foldLeft(annotationBlock)((b, annotationType) => {
      b.copy(annotationMap = b.annotationMap + (annotationType -> annotationSpan))
    })

  }
  
  /** Sorted set of the index pair of every character in the non-empty tspans **/
  private val charBIndexPairSet: SortedSet[(Int, Int)] = SortedSet(getElements().toIndexedSeq.zipWithIndex.flatMap { 
    case (e, blockIndex) => 
      (0 until e.getText().size).map(charIndex => {
        blockIndex -> charIndex
      })
  }: _*)


  /** Function to produce a map of int to map of int to label, known as segment  
    *
    * It takes an annotation type string, a blockIndex (tspan element offset)
    * and a charIndex (character offset), and returns the labels and corresponding index pairs
    * of the first complete segment, one starting with a B and ending with an L,
    * or just a U, for the provided annotation type string
    */
  final def getSegment(annotationTypeName: String)(blockIndex: Int, charIndex: Int): Segment = {

    val annotationType = annotationInfoMap(annotationTypeName).annotationType

    def loop(foundFirst: Boolean, blockIndex: Int, charIndex: Int): Segment = {

      if (annotationBlockSeq.size > blockIndex) {
        val block = annotationBlockSeq(blockIndex)
        block.annotationMap.get(annotationType) match {
          case None => loop(foundFirst, blockIndex + 1, 0)
          case Some(annotation) =>
            val labelMap = annotation.labelMap
            labelMap.keys.find(_ >= charIndex) match {
              case None =>
                loop(foundFirst, blockIndex + 1, 0)
              case Some(_charIndex) =>
                val label = labelMap(_charIndex)
                (foundFirst, label) match {
                  case (false, B(char)) if annotationType.c == char => loop(true, blockIndex, _charIndex) 
                  case (false, U(char)) if annotationType.c == char => loop(true, blockIndex, _charIndex) 
                  case (false, _) => loop(false, blockIndex, _charIndex + 1)

                  case (true, L) =>
                    IntMap(blockIndex -> IntMap(_charIndex -> L))
                  case (true, U(char)) if annotationType.c == char => 
                    IntMap(blockIndex -> IntMap(_charIndex -> U(char)))
                  case (true, U(_)) => 
                    loop(foundFirst, blockIndex, _charIndex + 1)
                  case (true, B(char)) if annotationType.c != char => 
                    loop(foundFirst, blockIndex, _charIndex + 1)
                  case (true, label) => 
                    val labelTable = loop(foundFirst, blockIndex, _charIndex + 1)
                    labelTable.get(blockIndex) match {
                      case None => 
                        labelTable + (blockIndex -> IntMap(_charIndex -> label))
                      case Some(rowIntMap) => 
                        labelTable + (blockIndex -> (rowIntMap + (_charIndex -> label)))
                    }
                }
            }
        }
      } else {
        IntMap[IntMap[Label]]()
      }

    }

    loop(false, blockIndex, charIndex)

  }

  /** Function to return an option of 4 ints representing the range of
    * text that makes up the the first complete string found on or after
    * the provided indexes that is of the provided annotation type string
    *
    * returns None if there is no text matching the provided annotation type or indexes
    *
    * the result's four ints represent 
    * the first block index, first char index, last block index, and last char index, respectively
    */
  final def getRange(annotationTypeName: String)(blockIndex: Int, charIndex: Int): Option[(Int, Int, Int, Int)] = {
    val segment = getSegment(annotationTypeName)(blockIndex, charIndex)

    if (segment.isEmpty) {
      None 
    } else {
      def findLastIndexPair(blockIndex: Int, charIndex: Int, constraint: Constraint): (Int, Int) = {
        constraint match {
          case CharCon => 
            (blockIndex -> charIndex)
          case SegmentCon(annoTypeName) =>
            val annoType = annotationInfoMap(annoTypeName).annotationType
            val segment =  getSegment(annoTypeName)(blockIndex, charIndex)
            val blockLIndex = segment.lastKey
            val charLIndex = segment(segment.lastKey).lastKey
            val con = annoType.constraintRange match {
              case Single(c) => c
              case Range(_, c) => c
            }
            findLastIndexPair(blockLIndex, charLIndex, con)
        }
      }

      val blockBIndex = segment.firstKey
      val charBIndex = segment(blockBIndex).firstKey
      val con = annotationInfoMap(annotationTypeName).annotationType.constraintRange match {
        case Single(c) => c
        case Range(_, c) => c
      }
      val (blockLIndex, charLIndex) = findLastIndexPair(segment.lastKey, segment(segment.lastKey).lastKey, con)

      Some(blockBIndex, charBIndex, blockLIndex, charLIndex)

    }
  }

  /** Function to return a map of Int to Element based on a range of block indexes (tspan offsets)
    * 
    * the result's integer key is the Element's offset out of all the non-empty tspans
    */
  final def getElementsInRange(blockIndex1: Int, blockIndex2: Int): IntMap[Element] = {
    IntMap((blockIndex1 to blockIndex2).map(blockIndex =>{
      blockIndex -> getElements().toIndexedSeq(blockIndex)
    }): _*)
  }

  /** Function to return a map of Int to Element 
    * 
    * the returned elements correspond to annotations that are of the provided annotation type 
    * and start on or after the provided indexes 
    */
  final def getElements(annotationTypeName: String)(blockIndex: Int, charIndex: Int): IntMap[Element] = {
    getRange(annotationTypeName)(blockIndex, charIndex) match {
      case None =>
        IntMap[Element]()
      case Some((blockBIndex, _, blockLIndex, _)) =>
        getElementsInRange(blockBIndex, blockLIndex)
    }
  }

  /** Function to return a map of Int to Int String pair based on a index range
    * 
    * the returned map's keys are block indexes, and each corresponding value is a char index and 
    * the text that exists in that block starting from that char index 
    */
  final def getTextMapInRange(blockIndex1: Int, charIndex1: Int, blockIndex2: Int, charIndex2: Int): IntMap[(Int, String)] = {
    getElementsInRange(blockIndex1, blockIndex2).map { case (blockIndex, e) => 

      if (blockIndex == blockIndex1 && blockIndex == blockIndex2) {
        blockIndex -> (charIndex1 -> e.getText().take(charIndex2 + 1).drop(charIndex1))

      } else if (blockIndex == blockIndex1) {
        blockIndex -> (charIndex1 -> e.getText().drop(charIndex1))

      } else if (blockIndex == blockIndex2) {
        blockIndex -> (0 -> e.getText().take(charIndex2 + 1))

      } else {
        blockIndex -> (0 -> e.getText())

      }

    }
  }


  /** Function to return a map of Int to Int String pair based on annotation type and minimum starting index pair 
    * 
    * the returned map's keys are block indexes, and each corresponding value is a char index and 
    * the text that exists in that block starting from that char index 
    */
  final def getTextMap(annotationTypeName: String)(blockIndex: Int, charIndex: Int): IntMap[(Int, String)] = {
    getRange(annotationTypeName)(blockIndex, charIndex) match {
      case None =>
        IntMap[(Int,String)]()
      case Some((blockBIndex, charBIndex, blockLIndex, charLIndex)) =>
        getTextMapInRange(
            blockBIndex, 
            charBIndex,
            blockLIndex,
            charLIndex
        )
    }
  }

  /** Function to return a sorted set of index pairs
    * 
    * the result index pairs represent the beginning indexes 
    * of the last constraint in the constraintRange, filtered down
    * to indexes that are also annotated as the annotatio type in
    * the first part of the range
    */
  final def getBIndexPairSet(constraintRange: ConstraintRange): SortedSet[(Int, Int)] = {
    constraintRange match {
      case Single(CharCon) =>
        charBIndexPairSet
      case Single(SegmentCon(annotationTypeName)) =>
        annotationInfoMap(annotationTypeName).bIndexPairSortedSet
      case Range(annotationTypeName, endCon) =>
        def loop(bIndexPairSortedSetAcc: SortedSet[(Int, Int)], constraint: Constraint): SortedSet[(Int, Int)] = {
          (constraint, endCon) match {
            case (CharCon, SegmentCon(_)) => 
              require(false, "constraintRange's end does not follow from its start")
              SortedSet[(Int, Int)]()
            case (x, y) if (x == y) => 
              bIndexPairSortedSetAcc
            case (SegmentCon(annotationTypeName), _) =>

              val _bIndexPairSortedSetAcc = bIndexPairSortedSetAcc.flatMap(pair => { 
                val (blockIndex, charIndex) = pair
                val segment = getSegment(annotationTypeName)(blockIndex, charIndex)
                segment.keys.flatMap(bI => {
                  segment(bI).keys.map(cI => {
                    bI -> cI
                  })
                })
              })

              val annotationType = annotationInfoMap(annotationTypeName).annotationType
              val _constraint = annotationType.constraintRange match {
                case Single(c) => c
                case Range(_, c) => c
              }

              loop(_bIndexPairSortedSetAcc, _constraint)
          }
        }
        loop(annotationInfoMap(annotationTypeName).bIndexPairSortedSet, SegmentCon(annotationTypeName))
      case _ =>
        require(false, "constraintRange is illformed")
        SortedSet[(Int, Int)]()
    }
  }


  /** Function to return text that exists of the provided annotation type 
    * on or after each provided index pair 
    */
  private def getSegmentedText(annoType: String, bIndexPairSet: Set[(Int, Int)]): List[String] = {
    bIndexPairSet.toList.map {
      case (blockBIndex, charBIndex) =>
        val textMap = getTextMap(annoType)(blockBIndex, charBIndex)
        textMap.values.map(_._2).mkString("")
    }
  }

  /** Function to return the text of each complete annotation segment of the provided annotation type **/
  final def getTextByAnnotationType(annoType: String): List[String] = {
    val bIndexPairSet = getBIndexPairSet(Single(SegmentCon(annoType)))
    getSegmentedText(annoType, bIndexPairSet)
  }

  /** Function to return the text of each complete annotation segment of the provided annotation type 
    * where the text is also part of a segment of the first argument, filterAnnoType
    */
  final def getFilteredTextByAnnotationType(filterAnnoType: String, annoType: String): List[String] = {
    val bIndexPairSet = getBIndexPairSet(Range(filterAnnoType, SegmentCon(annoType)))
    getSegmentedText(annoType, bIndexPairSet)
  }

  /** Function to produce a new Annotator with additional annotations
    *
    * nameCharPairSeq is a sequence new annotation type strings and corresponding chars 
    * constraintRange specifies the the parts of text the annotations are restricted to
    * fullLabelMap is a map of index pair to label, which specifies where to add annotation labels 
    *
    * labels with index pairs that are outside of the annotatble region defined by the dom and constraintRange
    * will not be added
    */
  final def annotate(
      nameCharPairSeq: Seq[(String, Char)], 
      constraintRange: ConstraintRange, 
      fullLabelMap: Map[(Int, Int), Label]
  ): Annotator = {

    val annotatableIndexPairSet = getBIndexPairSet(constraintRange)

    val annotationTypeSeq = nameCharPairSeq.map {
      case (name, char) =>
        assert(!annotationInfoMap.contains(name), "annotation type named " + name + " already exists")
        AnnotationType(name, char, constraintRange)
    }

    val labelTable = fullLabelMap.filter(p => {
      val indexPair = p._1
      annotatableIndexPairSet.contains(indexPair)
    }).foldLeft(IntMap[IntMap[Label]]()) {
      case (tableAcc, ((blockIndex, charIndex), label)) =>
        if (tableAcc.contains(blockIndex)) {
          tableAcc + (blockIndex -> (tableAcc(blockIndex) + (charIndex -> label)))
        } else {
          tableAcc + (blockIndex -> IntMap(charIndex -> label))
        }
    }

    val _annotationBlockSeq = annotationBlockSeq.zipWithIndex.map { case (block, blockIndex) => {
      labelTable.get(blockIndex) match {
        case None => block
        case Some(labelMap) =>
          val annotation = AnnotationSpan(labelMap, annotationTypeSeq)
          addAnnotation(annotation, block)
      }
    }}

    val _annotationInfoMap =  {
      val annotationInfoList = annotationTypeSeq.map {
        case _annotationType => 
          val char = _annotationType.c
          val bIndexPairSet = annotatableIndexPairSet.filter {
            case (blockIndex, charIndex) => 
              labelTable.contains(blockIndex) && ({
                val labelMap = labelTable(blockIndex)
                labelMap.contains(charIndex) && ({
                  val label = labelMap(charIndex)
                  label == B(char) || label == U(char)
                })
              })
          }

          _annotationType.name -> AnnotationInfo(_annotationType, bIndexPairSet)
          
      }

      annotationInfoMap ++ annotationInfoList
    }

    new Annotator(
      frozenDom,
      _annotationBlockSeq,
      _annotationInfoMap,
      annotationLinkSet
    )
    
  }

  /** Function to return a new Annotator that that has the provided links added 
    *
    * each link in the input set should be a Map of an annotation type string
    * to the index pair that has the beginning of one of its segments 
    */
  final def annotateLink(_annotationLinkSet: Set[AnnotationLink]): Annotator = {

    val bIndexSetMap = _annotationLinkSet.flatMap(_.keys).map(annoTypeStr => {
      annoTypeStr -> getBIndexPairSet(Single(SegmentCon(annoTypeStr)))
    }).toMap

    new Annotator(
      frozenDom, annotationBlockSeq, annotationInfoMap,
      annotationLinkSet ++ _annotationLinkSet.filter(annoLink => {
        annoLink.foldLeft(true) {
          case (boolAcc, (annoTypeStr, indexPair)) =>
            boolAcc && bIndexSetMap.contains(annoTypeStr) && bIndexSetMap(annoTypeStr).contains(indexPair)
        }
      })
    )


  }

  private val xmlOutputProcessor = new AbstractXMLOutputProcessor {
    override def write(writer: Writer, str: String) = {
      super.write(
          writer, 
          if (str == null) {
            str
          } else {
            str.replaceAll("&#xA;", "\n").replaceAll("<svg:tspan", "\n<svg:tspan")
          }
      )
    }
  }

  /** Function to write a string representation of the Annotator instance to the provided file path**/
  final def write(filePath: String): Annotator = {

    val writableDom = frozenDom.clone()
    Annotator.getElements(writableDom).zipWithIndex.foreach { case (e, i) => {
      val block = annotationBlockSeq(i)
      e.setAttribute("bio", renderAnnotationBlock(block))
    }}

    val root = writableDom.getRootElement()
    val annotationLinksE = new Element("annotation-links")
    annotationLinkSet.map(link => {
      val e = new Element("link")
      link.foreach(pair => {
        val annoType = pair._1
        val indexPair = pair._2
        val block = annotationBlockSeq(indexPair._1)
        val totalIndex = block.startIndex + indexPair._2
        e.setAttribute(annoType, totalIndex.toString)
      })
      annotationLinksE.addContent(e)
    })
    root.addContent(annotationLinksE)

    //format
    val outputter = new XMLOutputter(Format.getPrettyFormat(), xmlOutputProcessor)

    //write
    val out = new FileOutputStream(filePath)
    outputter.output(writableDom, out)
    this

  }

}
