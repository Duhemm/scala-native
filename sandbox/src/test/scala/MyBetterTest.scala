import scala.scalanative.test

class MyBetterTest extends test.Test {

  override def test = {
    everythingWorks
  }

  def everythingWorks =
    println("Foobar!")
}
