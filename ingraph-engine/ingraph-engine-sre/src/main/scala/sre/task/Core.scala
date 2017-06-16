package sre.task

import ingraph.ire.IngraphElement

import scalaz.{-\/, \/, \/-}

object Core {
  import ingraph.ire.{IngraphEdge, IngraphVertex}
  type ElementT = IngraphElement
  type VertexT = IngraphVertex
  type EdgeT = IngraphEdge
  type LabelT = String

  type Res[+T] = Exception \/ T
  type Succ[+T] = \/-[T]
  type Fail = -\/[Exception]

  implicit class ResFlatten[+A](val res: Res[Res[A]]) extends AnyVal {
    def flatten: Res[A] = res.flatMap(a => a)
  }

  trait Context[+Element <: ElementT] {
    val head: Element
    val variables: Map[String, Any]
  }

  case class VertexContext(
                            head: VertexT,
                            variables: Map[String, Any]
                          ) extends Context[VertexT]
  case class EdgeContext(
                          head: EdgeT,
                          variables: Map[String, Any]
                        ) extends Context[EdgeT]
}



