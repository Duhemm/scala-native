import scala.scalanative.test

class MyTest extends test.Test {

  override def test = {
    everythingWorks
  }

  def everythingWorks =
    println("Foobar?")
}
