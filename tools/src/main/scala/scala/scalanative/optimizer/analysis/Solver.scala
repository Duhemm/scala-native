package scala.scalanative
package optimizer
package analysis

object Solver {
  def getConstraints[K, V](elements: Map[K, Set[V]]): (Set[Inequality[V]], Set[V]) = {
    val fromCombinations =
      elements.values
               .flatMap { x =>
                 x.toSeq
                   .combinations(2)
                   .toList
                   .map {
                    case Seq(a, b) => Inequality.apply(a, b)
                   }
              }.toSet

    val freeVariables =
      elements.values.flatten.filter {
        case v => !fromCombinations.exists(_ uses v)
      }.toSet

    (fromCombinations, freeVariables)

  }

  def solve[V](constraints: Set[Inequality[V]]): Map[V, Int] = {
    def allVariables(cs: Set[Inequality[V]]): Seq[V] =
      cs.flatMap(_.variables).toSeq
    def inner(cs: Set[Inequality[V]])(sol: Map[V, V]): (Set[Inequality[V]], Map[V, V]) = {
      val combinations = allVariables(cs).combinations(2).toSeq

      combinations.find {
        case Seq(a, b) => !cs.exists(c => c.uses(a) && c.uses(b))
      } match {
        case None =>
          (cs, sol)
        case Some(Seq(a, b)) =>
          println("No constraint uses both " + a + " and " + b + ". Replace all " + b + " with " + a)
          inner(cs.map(_.subst(b, a)))(sol + (b -> a))
      }
    }
    val (remainingConstraints, substitutions) = inner(constraints)(Map.empty)
    val simplifiedSol = allVariables(remainingConstraints).zipWithIndex.toMap
    println("Simplified solution: " + simplifiedSol)
    println("All substitutions:" + substitutions)

    combineSols(substitutions.toSeq, simplifiedSol)
    //substitutions.foldLeft(simplifiedSol) {
      //case (sol, (b, a)) => sol + (b -> sol(a))
    //}
  }

  private def combineSols[V](substitutions: Seq[(V, V)], simplifiedSol: Map[V, Int]): Map[V, Int] = {
    substitutions.toSeq match {
      case Seq() =>
        simplifiedSol
      case (b, a) +: rest if simplifiedSol.contains(a) =>
        combineSols(rest, simplifiedSol + (b -> simplifiedSol(a)))
      case (b, a) +: rest =>
        val rmap = rest.toMap
        combineSols((b, rmap(a)) +: rest, simplifiedSol)
    }
  }

  def solveFromProfiling[V](parsedMap: Map[V, Set[V]]): Map[V, Int] = {
    val (constraints, freeVariables) = getConstraints(parsedMap)
    val solution =
      (freeVariables.map(v => v -> 0).toMap ++ solve(constraints)).withDefaultValue(-1)
    println("#" * 181)
    println("All constraints: ")
    constraints foreach println
    println("-" * 181)
    println("Solution:")
    println(solution)
    println("#" * 181)
    val assignments = parsedMap.mapValues(xs => xs.map(solution))
    parsedMap.foreach { case (k, vs) => assert(vs.size == assignments(k).size) }
    solution
  }

  class Inequality[T](val a: T, val b: T) {
    assert(a != b)

    override def equals(o: Any): Boolean = o match {
      case o: Inequality[_] => (o.a == a && o.b == b) || (o.b == a && o.a == b)
      case _                => false
    }

    def uses(v: T): Boolean =
      v == a || v == b

    def variables: Set[T] =
      Set.apply(a, b)

    def subst(v1: T, v2: T): Inequality[T] =
      if (a == v1) Inequality.apply(v2, b)
      else if (b == v1) Inequality.apply(a, v2)
      else this

    override def hashCode(): Int =
      a.## + b.##

    override def toString: String =
      StringContext.apply("(", " != ", ")").s(a, b)
  }

  object Inequality {
    def apply[T](a: T, b: T): Inequality[T] =
      new Inequality(a, b)
  }
}
