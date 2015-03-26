

object ProjectEuler extends App {

  lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs zip fibs.tail map (e => e._1 + e._2)

  lazy val fibs2: Stream[BigInt] = {
    def fib(a: BigInt, b: BigInt): Stream[BigInt] = {
      a #:: fib(b, a + b)
    }
    fib(1, 1)
  }

  def primes(n: BigInt): scala.collection.mutable.Set[BigInt] = {
    val limit = BigInt(math.sqrt(n.toDouble).toLong)
    val eratostenesSieve: scala.collection.mutable.Set[BigInt] = scala.collection.mutable.Set.empty ++ (BigInt(2) to limit)

    def filterNonPrimes(primeCandidate: BigInt): Unit = {
      if (eratostenesSieve contains primeCandidate) {
        eratostenesSieve --= primeCandidate * primeCandidate to limit by primeCandidate
      }
    }

    (BigInt(2) to limit) foreach filterNonPrimes

    eratostenesSieve

  }

  def eratostenesSieve(n: BigInt): List[BigInt] = {
    val set = List.empty ++ (BigInt(2) to n)
    val dividers = BigInt(2) to BigInt(math.sqrt(n.toDouble).toLong)
    dividers.foldLeft(set)((setRefined, d) => setRefined filterNot (e => e > d && e % d == 0))
  }

  def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) a else gcd(b, a % b)
  }

  def problem1(n: Int): Int = {
    (1 until n) filter (e => e % 3 == 0 || e % 5 == 0) sum
  }

  def problem2(n: BigInt): BigInt = {
    fibs2 filter (_ % 2 == 0) takeWhile (_ <= n) sum
  }

  def problem3(n: BigInt): BigInt = {
    primes(n) filter (e => n % e == 0) max
  }

  def problem4(range: Seq[Int]): Int = {
    def isPalindrome(n: Int): Boolean = {
      val nToString = n.toString
      nToString == nToString.reverse
    }

    val palindroms = for {
      x <- range
      y <- range
      if isPalindrome(x * y)
    } yield x * y

    palindroms max

  }

  def problem5(s: Seq[Int]): BigInt = {
    s.foldRight(BigInt(1))((a, b) => a * b / gcd(a, b))
  }

  def problem6(s: Seq[Int]): BigInt = {
    val sum = s.sum
    sum * sum - s.foldLeft(0)((a, b) => a + b * b)
  }

  def problem7(n: Int): BigInt = {
    def guess(estimatedSize: Int): List[BigInt] = {
      val s = eratostenesSieve(estimatedSize)
      if (s.size >= n) {
        s
      } else {
        guess(estimatedSize * 2)
      }
    }
    guess(n).sorted.drop(n - 1).head
  }


  override def main(args: Array[String]) {
    //   println(problem2(4000000))
    //    fibs2 take 4 print
    //println(primes(10))
    //println(problem3(10))
    //println(problem3(13195))
    //println(problem3(600851475143L))
    //println(problem4(100 to 999))
//    println(problem6(1 to 100))
    //println(problem7(10001))
    //    print(eratostenesSieve(10001) max)
  }


}
