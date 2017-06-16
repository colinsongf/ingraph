package sre.task

import ingraph.ire.{Indexer, IngraphVertex}
import sre.task.Core._
import sre.task.Expressions.Assert
import sre.task.Iterators.{EdgeIterator, VertexIterator}

import scala.collection.Iterator
import scalaz.{-\/, \/-}

object Direction {
  sealed trait Value
  final case object Out extends Value
  final case object In extends Value
  final case object Both extends Value

  val values: Set[Value] = Set(Out, In, Both)
}


class Engine(private val indexer: Indexer) {

  /*
    Returns an iterator for the vertex with the given `id`.
   */
  def vertex(id: Long): VertexIterator = {
    val vertexIter = Seq(indexer.vertexById(id)).iterator
    new VertexIterator(for {
      vertex <- vertexIter
    } yield VertexContext(vertex, Map()))
  }

  /*
    Returns an iterator for vertices that has all the labels given in the `labelSuperSetOf` parameter.
   */
  def vertices(labelsSupersetOf: Set[String]): VertexIterator = {
    val vertexIter = labelsSupersetOf
      .map((l) => (l, indexer.getNumberOfVerticesWithLabel(l)))
      .toSeq
      .sortBy({ case (_, size) => size })
      .map({ case (label, _) => label })
      .foldLeft(Set.empty[IngraphVertex])((result, label) => result & indexer.verticesByLabel(label).toSet)
      .iterator
    new VertexIterator(for {
      vertex <- vertexIter
    } yield VertexContext(vertex, Map()))
  }

  /*
    If `labelIn` is the empty set returns an iterator over all edges. Else returns an iterator for edges whose
    label is in `labelIn`.
   */
  def edges(labelIn: Set[String]): EdgeIterator = {

    val edgeIter = new EdgeIterator(
      labelIn
        .find((l) => indexer.getNumberOfEdgesWithLabel(l) > 0)
        .iterator
        .map((l) => indexer.edgesByLabel(l))
        .flatten
        .map((edge) => EdgeContext(edge, Map()))
    )
    new EdgeIterator(edgeIter)
  }

  /*
    Returns an iterator for the edge with the given `id`.
   */
  def edge(id: Long): EdgeIterator = {
    val edgeIter = Seq(indexer.edgeById(id)).iterator
    new EdgeIterator(for {
      edge <- edgeIter
    } yield EdgeContext(edge, Map()))
  }
}

object Iterators {

  class VertexIterator(
                        private val iterator: Iterator[Context[VertexT]]
                      ) extends Iterator[Context[VertexT]] {

    /*
      Filters the iterator based on an Assertion.
      Can throw an exception.
     */
    def check[Ctx >: Context[VertexT]](assert: Assert[Ctx]): VertexIterator = {
      new VertexIterator(iterator.filter((ctx) => assert(ctx) match {
          case \/-(kleene) => kleene.isTrue
          case -\/(exception) => throw exception
      }))
    }


    //    def capture(what: Head.PropertyRef[Context[VertexT], VertexT]): Iterator[Context[VertexT]] = {
    //      for {
    //        c @ Context(vertex, variables) <- iterator
    //      } yield Context(vertex, variables.)
    //    }

    def extend(direction: Direction.Value, label: Option[LabelT]): EdgeIterator = {
      type M = scala.collection.Map[String, EdgeT]
      type M2I = M => Iterator[EdgeT]

      val map2Iter = label match {
        case Some(label) => (x: M) => x.get(label).iterator // we can create an iterator out of an Option
        case _ => (x: M) => x.valuesIterator
      }

      val navigate = direction match {
        case Direction.Out => (v: VertexT, mapToIter: M2I)
        => mapToIter(v.edges)
        case Direction.In => (v: VertexT, mapToIter: M2I)
        => mapToIter(v.reverseEdges)
        case Direction.In => (v: VertexT, mapToIter: M2I)
        => mapToIter(v.edges) ++ mapToIter(v.reverseEdges)
      }

      new EdgeIterator(for {
        VertexContext(vertex, variables) <- iterator
        edge <- navigate(vertex, map2Iter)
      } yield EdgeContext(edge, variables))
    }

    override def hasNext: Boolean = iterator.hasNext

    override def next(): Context[VertexT] = iterator.next()
  }

  class EdgeIterator(
                      private val iterator: Iterator[Context[EdgeT]]
                    ) extends Iterator[Context[EdgeT]] {

    /*
      Filters the iterator based on an Assertion.
      Can throw an exception.
     */
    def filter(assert: Assert[Context[EdgeT]]): EdgeIterator = {
      new EdgeIterator(iterator.filter((ctx) => assert(ctx) match {
        case \/-(kleene) => kleene.isTrue
        case -\/(exception) => throw exception
      }))
    }

    // TODO: Need to fix this. EdgeIterator has to know which direction it is navigating.
    def targetVertex(): VertexIterator = {
      new VertexIterator(for {
        EdgeContext(edge, variables) <- iterator
      } yield VertexContext(edge.targetVertex, variables))
    }
    def sourceVertex(): VertexIterator = {
      new VertexIterator(for {
        EdgeContext(edge, variables) <- iterator
      } yield VertexContext(edge.sourceVertex, variables))
    }

    override def hasNext: Boolean = iterator.hasNext

    override def next(): Context[EdgeT] = iterator.next()
  }
}


