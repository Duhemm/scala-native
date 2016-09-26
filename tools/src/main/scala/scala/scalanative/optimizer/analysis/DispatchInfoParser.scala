package scala.scalanative
package optimizer
package analysis

import fastparse.WhitespaceApi
import fastparse.noApi._

import nir.parser.{Global, Local}

import util.sh
import Shows._

object DispatchInfoParser {

  private val IgnoreWhitespace = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(Seq(' ', '\t', '\n')).rep)
  }
  import IgnoreWhitespace._

  val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

  val dispatchHeader: P[String] =
    P("=" ~ Global.parser.! ~ "->" ~ Local.parser.! ~ "->" ~ Global.parser.! ~ ":") map {
      case (enclosing, inst, meth) => sh"$enclosing -> $inst -> $meth".toString
    }

  val dispatchMethod: P[(String, Seq[Int])] =
    dispatchHeader ~ (number ~ "(" ~ number ~ ")").rep(1) map {
      case (header, tpes) => (header, tpes.sortBy(_._2).map(_._1).reverse)
    }

  val dispatchInfo: P[Map[String, Seq[Int]]] =
    dispatchMethod.rep ~ End map (_.toMap)

  def apply(in: String): Map[String, Seq[Int]] =
    dispatchInfo.parse(in) match {
      case Parsed.Success(info, _) => info
      case Parsed.Failure(_, _, _) => Map.empty
    }
}
