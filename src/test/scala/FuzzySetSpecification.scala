import FuzzySet.Universe
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

// https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md
object FuzzySetSpecification extends Properties("FuzzySet") {

  import ArbitraryFuzzySet._

  property("isEmpty") = forAll { a: ArbitraryFuzzySet[Int] =>
    !a.fuzzySet.isEmpty(a.universe)
  }

  property("equalTo") = forAll { a: ArbitraryFuzzySet[Int] =>
    a.fuzzySet.equalTo(a.fuzzySet)(a.universe)
  }

  property("contains") = forAll { a: ArbitraryFuzzySet[Int] =>
    a.universe.values.forall(x => a.fuzzySet.contains(x) == x)
  }

  property("union") = forAll { (a: ArbitraryFuzzySet[Int], b: ArbitraryFuzzySet[Int]) =>
    val s = a.fuzzySet.union(b.fuzzySet)
    (a.universe.values ++ b.universe.values).forall(x => a.fuzzySet.contains(x).max(b.fuzzySet.contains(x)) == s.contains(x))
  }

  property("intersect") = forAll { (a: ArbitraryFuzzySet[Int], b: ArbitraryFuzzySet[Int]) =>
    val s = a.fuzzySet.union(b.fuzzySet)
    (a.universe.values ++ b.universe.values).forall(x => a.fuzzySet.contains(x).min(b.fuzzySet.contains(x)) == s.contains(x))
  }

  property("complement") = forAll { a: ArbitraryFuzzySet[Int] =>
    a.fuzzySet.equalTo(a.fuzzySet.complement(a.universe))(a.universe)
  }

}

case class ArbitraryFuzzySet[T](fuzzySet: FuzzySet[T], universe: Universe[T])

object ArbitraryFuzzySet {

  implicit def arbitraryNonEmptyUniverse[T](implicit a: Arbitrary[T]): Arbitrary[Universe[T]] =
    Arbitrary {
      for {
        values <- Gen.nonEmptyContainerOf[Set, T](a.arbitrary)
      } yield new Universe(values)
    }

  implicit def arbitraryNonEmptyFuzzySet[T](implicit a: Arbitrary[Universe[T]]): Arbitrary[ArbitraryFuzzySet[T]] =
    Arbitrary {
      for {
        universe <- a.arbitrary
        values <- Gen.nonEmptyContainerOf[List, T](Gen.oneOf(universe.values))
        grades <- Gen.containerOfN[List, Double](values.size, Gen.choose(0.0, 1.0) suchThat (v => v != 0.0))
      } yield {
        val fuzzySet = new FuzzySet[T]({ v: T =>
          val index = values.indexOf(v)
          if (index < 0) 0.0 else grades(index)
        })

        ArbitraryFuzzySet(fuzzySet, universe)
      }
    }
}