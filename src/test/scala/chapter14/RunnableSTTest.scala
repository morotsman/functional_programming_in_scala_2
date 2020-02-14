package chapter14

import chapter8.{Gen, Prop}
import org.scalatest.FunSuite

class RunnableSTTest extends FunSuite {

  test("mutate") {
    val p = new RunnableST[(Int, Int)] {
      override def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    assert(ST.runST(p) == (3, 2))
  }

  test("won't compile") {
    //new RunnableST[STRef[Nothing, Int]] {
    //  override def apply[S] = STRef(1)
    //}
  }

  test("quicksort") {
    val prop = Prop.forAll(Gen.listOf(Gen.int))(l => {
      QuickSort.quicksort(l.toScalaList()) == l.toScalaList().sorted
    })
    Prop.run(prop)

  }

}
