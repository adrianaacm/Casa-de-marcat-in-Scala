package decoder

import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable

object Decoder {
    // TODO 1.1.1
    given char2Bit: Conversion[Char, Bit] with 
      def apply(x: Char): Bit = x match {
        case '0' => Zero
        case '1' => One
      }

    given int2Bit: Conversion[Int, Bit] with
      def apply(s: Int): Bit = s match {
        case 0 => Zero
        case 1 => One
      }

    // TODO 1.1.2
    extension(c:Bit)
      def complement: Bit = c match {
        case Zero => One
        case One => Zero
      }
    // TODO 1.1.3
    def reverseList[A] (l: List[A]): List[A] = {
      def reverseLoop (l: List[A], result: List[A]) : List[A] = {
        l match {
            case Nil => result
            case x::xs => reverseLoop(xs, x::result)
        }
      }
      reverseLoop(l, Nil)
    }

    val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
    val leftOddList: List[List[Bit]] = LStrings.map(x => x.map(y => char2Bit(y)).toList) // codificări L
    val rightList: List[List[Bit]] = leftOddList.map(x => x.map(complement)) // codificări R
    val leftEvenList: List[List[Bit]] = rightList.map(x => reverseList(x)) // codificări  G
  
    // TODO 1.1.4
    extension[A](l: List[A])
      def groupedByEquality: List[List[A]] = {
        def grouped_aux(fullList: List[List[A]], curr: List[A], l: List[A]): List[List[A]] = {
            l match {
                case Nil => curr::fullList
                case x::xs => {
                    curr match {
                        case Nil => grouped_aux(fullList, x::Nil, xs)
                        case y::ys => {
                            if (x == y) {
                                grouped_aux(fullList, y::curr, xs)
                            } else {
                                grouped_aux(curr::fullList, x :: Nil, xs)
                            }
                        } 
                    }
                }
            }
        }
        grouped_aux(Nil, Nil, l).reverse
      }
  
    // TODO 1.1.5
    def runLength[A](l: List[A]): List[(Int, A)] = {
      def run_loop(list: List[List[A]], finalList: List[(Int,A)]): List[(Int, A)] = {
        list match {
            case Nil => finalList
            case x::xs => run_loop(xs, (x.length, x.head)::finalList)
        }
      }
      run_loop(l.groupedByEquality, Nil).reverse
    }
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }
    //TODO 1.2.1
    def -(other: RatioInt) = {
      val newA = this.a * other.b - other.a * this.b
      val newB = this.b * other.b
      RatioInt(newA, newB)
    }

    def +(other: RatioInt) = {
      val newA = this.a * other.b + other.a * this.b
      val newB = this.b * other.b
      RatioInt(newA, newB)
    }

    def *(other: RatioInt) = {
      val newA = this.a * other.a
      val newB = this.b * other.b
      RatioInt(newA, newB)
    }

    def /(other: RatioInt) = {
      val newA = this.a * other.b
      val newB = this.b * other.a
      RatioInt(newA, newB)
    }

    //TODO 1.2.2
    def compare(other: RatioInt): Int = {
      val thisValue = this.a * other.b
      val thatValue = other.a * this.b
      thisValue.compare(thatValue)
    }
  }

    // TODO 1.3.1
    def totalLength[A](l: List[(Int, A)]): Int = {
      def count_loop(l: List[(Int,A)], acc: Int): Int = {
        l match {
            case Nil => acc
            case x::xs => count_loop(xs, acc + x._1)
        }
      }
      count_loop(l, 0)
    }

    def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
      val totalLen = totalLength(l);
      def scale_loop(l: List[(Int, A)], finalList: List[(RatioInt, A)]): List[(RatioInt, A)] = {
        l match {
          case Nil => finalList
          case x::xs => scale_loop(xs, (RatioInt(x._1, totalLen), x._2)::finalList)
        }
      }
      scale_loop(l, Nil).reverse
    }

    // TODO 1.3.2
    def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
      val scaledL = scaleToOne(l)
      val firstBit = scaledL.head._2 
      def form_loop(l: List[(RatioInt, Bit)], finalList: List[RatioInt]): List[RatioInt] = {
        l match {
          case Nil => finalList
          case x::xs => form_loop(xs, x._1::finalList)
        }
      }
      (firstBit, form_loop(scaledL, Nil).reverse)
    }
  
    // TODO 1.3.3
    def toParities(s: Str): List[Parity] = {
      def parity_loop(s: Str, finalList: List[Parity]): List[Parity] = {
        s match {
          case Nil => finalList
          case x::xs => {
            x match {
              case 'L' => parity_loop(xs, Odd::finalList)
              case 'G' => parity_loop(xs, Even::finalList)
              case _ => parity_loop(xs, finalList)
            }
          }
        }
      }
      parity_loop(s, Nil).reverse
    }
  
    // TODO 1.3.4
    val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
      "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
    val leftParityList: List[List[Parity]] = {
      def parityList_loop(l: List[String], finalList: List[List[Parity]]): List[List[Parity]] = {
        l match {
          case Nil => finalList
          case x::xs => parityList_loop(xs, toParities(x.toList)::finalList)
        }
      }
      parityList_loop(PStrings, Nil).reverse
    }

    // TODO 1.3.5
    type SRL = (Bit, List[RatioInt])

    val leftOddSRL:  List[SRL] = leftOddList.map(x => scaledRunLength(runLength(x)))
    val leftEvenSRL:  List[SRL] = leftEvenList.map(x => scaledRunLength(runLength(x)))
    val rightSRL:  List[SRL] = rightList.map(x => scaledRunLength(runLength(x)))

    // TODO 1.4.1
    def distance(l1: SRL, l2: SRL): RatioInt = {
      if (l1._1 != l2._1) RatioInt(100,1)
      else {
        val l = l1._2.zip(l2._2)
        val lFinal: List[RatioInt] = l.map(x => {
          if (x._1 > x._2) x._1 - x._2
          else x._2 - x._1
        })
        lFinal.foldRight(RatioInt(0,1))(_ + _)
      }
    }
  
    // TODO 1.4.2
    def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
      def bestLoop(codes: List[SRL], min: RatioInt, minIdx: Int, currIdx: Int): (RatioInt, Digit) = {
        codes match {
          case Nil => (min, minIdx)
          case x::xs => {
            if (distance(x, digitCode) < min) bestLoop(xs, distance(x, digitCode), currIdx, currIdx + 1)
            else bestLoop(xs, min, minIdx, currIdx + 1)
          }
        }
      }

      bestLoop(SRL_Codes, RatioInt(100,1), 0, 0)
    }
  
    // TODO 1.4.3
    def bestLeft(digitCode: SRL): (Parity, Digit) = {
      val oddMatch = bestMatch(leftOddSRL, digitCode)
      val evenMatch = bestMatch(leftEvenSRL, digitCode)

      if (oddMatch._1 < evenMatch._1) (Odd, oddMatch._2)
      else (Even, evenMatch._2)
    }
  
    // TODO 1.4.4
    def bestRight(digitCode: SRL): (Parity, Digit) = {
      val rightMatch = bestMatch(rightSRL, digitCode)
      (NoParity, rightMatch._2)
    }
    
    def chunksOf[A](n: Int)(l: List[A]): List[List[A]] = {  
      def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
      l match {
        case Nil => Nil
        case _ =>
          val (h, t) = f(l)
          h :: chunkWith(f)(t)
        }
      }
      chunkWith((l: List[A]) => l.splitAt(n))(l)
      }

    // TODO 1.4.5
    def nextBit(bit: Bit): Bit = {
      bit match {
        case One => Zero
        case Zero => One
      }
    }

    def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
      val left = rle.drop(3).take(24)

      val right = rle.drop(32).take(24)

      val leftList: List[(Parity, Digit)] = chunksOf(4)(left).map(chunk => bestLeft(scaledRunLength(chunk)))

      val rightList: List[(Parity, Digit)] = chunksOf(4)(right).map(chunk => bestRight(scaledRunLength(chunk)))

      leftList ::: rightList
    }

    // TODO 1.4.6
    def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
      val parityList = l.map(x => x._1).take(6)

      def loopFirstDigit(leftParity: List[List[Parity]], parityList: List[Parity], idx: Int): Option[Digit] = {
        leftParity match {
          case x::xs => {
            if (x == parityList) Some(idx)
            else loopFirstDigit(xs, parityList, idx + 1)
          }
          case Nil => None
        }
      }
      loopFirstDigit(leftParityList, parityList, 0)
    }

    // TODO 1.4.7
    def checkDigit(l: List[Digit]): Digit = {
      val WiCi = l.zipWithIndex;
      val WiCiFold = WiCi.map((x,y) => {
        if ((y+1) % 2 == 0) x * 3
        else x
      }).foldRight(0)(_+_) 

      (10 - WiCiFold % 10) % 10
    }
  
    // TODO 1.4.8
    def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
      if (code.map(_._2).length != 13) None
      else {
        if (firstDigit(code.drop(1).take(6)) == None) None
        else if (checkDigit(code.map(_._2).take(12)) != code.last._2) None
        else Some(code.map(_._2).mkString)
      }
    }
  
    // TODO 1.4.9
    def solve(rle:  List[(Int, Bit)]): Option[String] = {
      val digits = findLast12Digits(rle)
      if (firstDigit(digits) == None) None
      else if (verifyCode((NoParity, firstDigit(digits).get) :: digits) == None) None
      else Some(firstDigit(digits).get.toString + digits.map(x => x._2).mkString)
    }
    
    def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
      val rle = runLength(row);

      def condition(sl: List[(Int, Pixel)]): Boolean = {
        if (sl.isEmpty) false
        else if (sl.size < 59) false
        else sl.head._2 == 1 &&
          sl.head._1 == sl.drop(2).head._1 &&
          sl.drop(56).head._1 == sl.drop(58).head._1
      }

      rle.sliding(59, 1)
        .filter(condition)
        .toList
        .map(_.map(pair => (pair._1, pair._2)))
    }
}
