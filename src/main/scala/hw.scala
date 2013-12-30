object Hi {

  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

  object Fraction {
    def apply(n: Int): Fraction = Fraction(n, 1)
  }

  case class Fraction(val n: Int, d: Int) {
    def normalized(): Fraction = {
      val hcf = gcd(n, d)
      Fraction(n / hcf, d / hcf)
    }

    override def equals(that: Any): Boolean = that match {
      case Fraction(tn, td) =>
        tn == n && td == d
      case _ => false
    }

    override def toString(): String = 
      if (d > 1) n.toString + " / " + d.toString
      else n.toString
  }

  trait Op {
    def act(a: Fraction, b: Fraction): Fraction
  }

  case class plus extends Op {
    def act(a: Fraction, b: Fraction): Fraction = {
      Fraction(a.n * b.d + b.n * a.d, a.d * b.d).normalized
    }

    override def toString(): String = "+"
  }

  case class minus extends Op {
    def act(a: Fraction, b: Fraction): Fraction = {
      Fraction(a.n * b.d - b.n * a.d, a.d * b.d).normalized
    }
    override def toString(): String = "-"
  }

  case class mult extends Op {
    def act(a: Fraction, b: Fraction): Fraction = {
      Fraction(a.n * b.n, a.d * b.d).normalized
    }
    override def toString(): String = "*"
  }

  case class div extends Op {
    def act(a: Fraction, b: Fraction): Fraction = {
      Fraction(a.n * b.d, a.d * b.n).normalized
    }
    override def toString(): String = "/"
  }

  /*
  def form(fs: List[Fraction]) = {
    val ops = List(plus, minus, mult, div)
    for {
      perm <- fs.permutations
      op1 <- ops
      op2 <- ops
      op3 <- ops
      } {
    }
  }
  */


  /*
  def twentyFour(a: Int, b: Int, c: Int, d: Int): List[List[String]] = {
  }
  */

  case class Node(var left: Node, var right: Node)

  // assume n > 0
  def genTrees(n: Int): Seq[Node] = {
    if (n == 0)
      return List()
    if (n == 1)
      return List(Node(null, null))

    (0 until n) flatMap { i =>
      val leftTrees = genTrees(i)
      val rightTrees = genTrees(n - i - 1)
      if (leftTrees.isEmpty) {
        rightTrees map { rightTree =>
          Node(null, rightTree)
        }
      } else if (rightTrees.isEmpty) {
        leftTrees map { leftTree =>
          Node(leftTree, null)
        }
      } else {
        for {
          leftTree <- leftTrees
          rightTree <- rightTrees
        } yield {
          Node(leftTree, rightTree)
        }
      }
    }
  }

  case class InOrderApplyResult(result: Fraction, remainingOps: List[Op], remainingNumbers: List[Fraction], str: String)

  def inOrderApply(root: Node, ops: List[Op], ns: List[Fraction]): Option[InOrderApplyResult] = {
    if (root == null) Some(InOrderApplyResult(ns.head, ops, ns.tail, ns.head.toString))
    else {
      try {
        for {
          InOrderApplyResult(leftResult, leftRemOps, leftRemNums, leftStr) <- inOrderApply(root.left, ops.tail, ns)
          InOrderApplyResult(rightResult, remOps, remNums, rightStr) <- inOrderApply(root.right, leftRemOps, leftRemNums)
        } yield {
          val str = "(" + leftStr + ops.head.toString + rightStr + ")"
          InOrderApplyResult(ops.head.act(leftResult, rightResult), remOps, remNums, str)
        }
      } catch {
        case _ => None
      }
    }
  }

  def genCombosN[T](xs: List[T], chars: Int): List[List[T]] = {
    if (chars == 0)
      return List(List())
    for {
      x <- xs
      ys <- genCombosN(xs, chars - 1)
    } yield {
      x :: ys
    }
  }

  def genCombos[T](xs: List[T]): List[List[T]] = 
    genCombosN(xs, xs.size)

  case class FinalResult(tree: Node, ops: List[Op], nums: List[Fraction], str: String)

  def opNumCombinationsAndApply(ops: List[Op], nums: List[Fraction], size: Int): Seq[FinalResult] = {
    for {
      tree <- genTrees(size)
      thisOps <- genCombosN(ops, nums.size - 1)
      thisNums <- nums.permutations
      InOrderApplyResult(result, _, _, str) <- inOrderApply(tree, thisOps, thisNums)
      if(result == Fraction(24))
      //if (result.d == 1 && result.n > 20 && result.n < 30)
    } yield {
      FinalResult(tree, thisOps, thisNums, str)
    }
  }


  def main(args:Array[String]) = {
    /*
    val f1 = Fraction(2, 1)
    val f2 = Fraction(3, 6)
    println(f1)
    println(f2)
    println(div(f1, f2))
    println(mult(f1, f2))
    println(plus(f1, f2))
    println(minus(f1, f2))
    */
    //println(genTrees(2))
    /*
    if (true) {
      val fun = minus()
      val ops = List(fun, fun, fun)
      val one = Fraction(1, 1)
      val nums = List(one, one, one, one)
      for (tree <- genTrees(3)) {
        println(tree)
        val InOrderApplyResult(result, _, _, str) = inOrderApply(tree, ops, nums)
        println(result)
      }

      println(genCombos(List(1, 2, 3)))
    }
    */

    val ops = List(plus() , minus(), mult(), div())
    //val nums = List(Fraction(3), Fraction(4), Fraction(8), Fraction(6))
    //val nums = List(Fraction(3), Fraction(3), Fraction(8), Fraction(8))
    val nums = List(Fraction(1), Fraction(2), Fraction(8), Fraction(7))

    val results = opNumCombinationsAndApply(ops, nums, 3)
    println("Total results: " + results.size)
    for(result <- results) {
      println(result.str)
    }
    //println(result)

  }
}
