import utest._

object UTestTest extends TestSuite {
  val tests = this {
    'test1 {
      println("!" * 1000)
      1
    }
  }
}