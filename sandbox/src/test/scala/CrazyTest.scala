import scala.scalanative.test

class CrazyTest extends test.Test {

  override def test = {
    everythingWorks
  }

  def everythingWorks =
    println("Foobar!!!!")
}
