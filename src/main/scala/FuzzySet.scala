object FuzzySet {
  class Universe[T](val values: Set[T])
}

class FuzzySet[T](m: T => Double) {
  import FuzzySet.Universe

  def isEmpty(implicit universe: Universe[T]): Boolean =
    universe.values.forall(m(_) == 0.0)

  def equalTo(that: FuzzySet[T])(implicit universe: Universe[T]): Boolean =
    universe.values.forall(m(_).equals(that.contains(_)))

  def contains(value: T): Double =
    m(value)

  def union(that: FuzzySet[T]): FuzzySet[T] =
    new FuzzySet[T](a => contains(a).max(that.contains(a)))

  def intersect(that: FuzzySet[T]): FuzzySet[T] =
    new FuzzySet[T](a => contains(a).min(that.contains(a)))

  def complement(implicit universe: Universe[T]): FuzzySet[T] =
    new FuzzySet[T](a => {
      if (universe.values.contains(a))
        1 - contains(a)
      else
        0.0
    })
}

object FuzzySetApp extends App {
  import FuzzySet.Universe

  implicit val fuzzySetUniverse: Universe[Int] = new Universe(Set.from(1 to 5))

  val emptyFuzzySet = new FuzzySet[Int](_ => 0.0)
  println(emptyFuzzySet.isEmpty)

  val someNonEmptyFuzzySet = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

  val someFuzzySet = new FuzzySet[Int]({
    case 1 => 0.4
    case 2 => 0.8
    case 3 => 1
    case _ => 0.0
  })

  println(someNonEmptyFuzzySet.isEmpty)

  println(someNonEmptyFuzzySet.union(someFuzzySet).contains(1))
  println(someFuzzySet.union(someNonEmptyFuzzySet).contains(1))

  println(someNonEmptyFuzzySet.union(someFuzzySet).contains(1) == someFuzzySet.union(someNonEmptyFuzzySet).contains(1))

  println(someNonEmptyFuzzySet.intersect(someFuzzySet).contains(1))
  println(someFuzzySet.intersect(someNonEmptyFuzzySet).contains(1))

  println(someNonEmptyFuzzySet.intersect(someFuzzySet).contains(1) == someFuzzySet.intersect(someNonEmptyFuzzySet).contains(1))

  println(someFuzzySet.equalTo(someFuzzySet)(fuzzySetUniverse))

  println(someFuzzySet.complement(fuzzySetUniverse).contains(1))

}