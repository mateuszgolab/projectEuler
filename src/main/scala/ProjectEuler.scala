import scala.collection.mutable.Set

/**
 * Created by mgo12 on 18/03/2015.
 */
object ProjectEuler extends App {

  lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs zip fibs.tail map (e => e._1 + e._2)

  lazy val fibs2: Stream[BigInt] = {
    def fib(a: BigInt, b: BigInt): Stream[BigInt] = {
      a #:: fib(b, a + b)
    }
    fib(1, 1)
  }

  def primes(n: BigInt): Set[BigInt] = {
    val limit = BigInt(math.sqrt(n.toDouble).toLong)
    val eratostenesSieve: Set[BigInt] = Set.empty ++ (BigInt(2) to limit)

    def filterNonPrimes(primeCandidate: BigInt): Unit = {
      if (eratostenesSieve contains primeCandidate) {
        eratostenesSieve --= primeCandidate * primeCandidate to limit by primeCandidate
      }
    }

    (BigInt(2) to limit) foreach filterNonPrimes

    eratostenesSieve

  }

  def problem1(n: Int): Int = {
    (1 until n) filter (e => e % 3 == 0 || e % 5 == 0) sum
  }

  def problem2(n: BigInt): BigInt = {
    fibs2 filter (_ % 2 == 0) takeWhile (_ <= n) sum
  }

  def problem3(n: BigInt): BigInt = {
    primes(n) filter (e => (n % e == 0)) max
  }

  def problem4(range : Seq[Int]) : Int = {
    def isPalindrome(n : Int) : Boolean = {
      val nToString = n.toString
      nToString == nToString.reverse
    }

    val palindroms = for {
      x <- range
      y <- range
      if(isPalindrome(x*y))
    } yield x*y

    palindroms max

  }

  override def main(args: Array[String]) {
    //   println(problem2(4000000))
    //    fibs2 take 4 print
    //println(primes(10))
    //println(problem3(10))
    //println(problem3(13195))
    //println(problem3(600851475143L))
    println(problem4(100 to 999))

  }


}
