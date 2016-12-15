package scala.scalanative

import nir.Global

import org.scalatest._

class FrameworkTest extends InlineCachingSpec with Matchers {

  // "The test framework" should "return the definitions for a single class" in {
  //   link("A$",
  //        """object A {
  //          |  def main(args: Array[String]): Unit =
  //          |    println("Hello, world!")
  //          |}""".stripMargin) {
  //     case (_, _, defns) =>
  //       val defNames = defns map (_.name)
  //       defNames should contain(Global.Top("A$"))
  //   }
  // }

  // it should "return the definitions for classes in multiple files" in {
  //   val sources = Map("A.scala" -> "class A",
  //                     "B.scala" -> """object B extends A {
  //                                    |  def main(args: Array[String]): Unit = ()
  //                                    |}""".stripMargin)

  //   link("B$", sources) {
  //     case (_, _, defns) =>
  //       val defNames = defns map (_.name)
  //       defNames should contain(Global.Top("A"))
  //       defNames should contain(Global.Top("B$"))
  //   }
  // }

  //"it" should "run a simple example" in {
    //run("A$",
        //"""object A {
          //|  def main(args: Array[String]): Unit = {
          //|    println("Hello, world!")
          //|  }
          //|}""".stripMargin) {
      //case (exit, out, err) =>
        //exit should be(0)
        //out should have length (1)
        //out(0) should be("Hello, world!")
    //}
  //}

  "it" should "work with inline caching like a boss" in {
    val source = """class C(val x: String)
                   |object A {
                   |  def gimme(y: String): Array[C] = {
                   |    val x: Array[C] = new Array[C](2)
                   |    x(0) = new C(y + "3")
                   |    x(1) = new C(y + "4")
                   |    x
                   |  }
                   |  def showme(x: Array[C]): Unit = {
                   |    var i = 0
                   |    while (i < x.length) {
                   |     println(s"x($i) = ${x(i)}")
                   |     i = i + 1
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = {
                   |    val x = gimme("lol")
                   |    showme(x)
                   |    println("Hello, world!")
                   |  }
                   |}""".stripMargin

    easyInlineCaching(entry = "A$",
                      source) {
      case (exit, errLines, outLines) =>
        errLines foreach (l => println("[!!!] " + l))
        outLines foreach (l => println("      " + l))
        exit should be(0)
    }
  }
}
