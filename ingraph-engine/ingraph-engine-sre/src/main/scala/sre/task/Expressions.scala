package sre.task

import sre.task.Kleene._
import sre.task.Core.{Context, ElementT, Res, Succ}
import sre.task.Expressions.Ref

import scalaz.{-\/, \/-}

object Expressions {

  sealed trait Expr[-InT, +OutT] {
    def apply(lhs: InT): OutT
  }

  type EExpr[-InT, +OutT] = Expr[InT, Res[OutT]]
  type Assert[-InT] = EExpr[InT, Kleene]
  type Ref[-InT, +T] = EExpr[InT, Option[T]]

  /*
    Ref[InT, Option[Boolean]] should be isomorphic to Assert[-InT], because
    Option[Boolean] and Kleene are isomorphic. To make them interchangable we
    have to use implicit conversions and context bounds.
   */

  /*
    Used as a evidence type when we need something convertible to an Assert
   */
  type GenAssert[-InT, -From] = From => Assert[InT]

  /*
    Used as a evidence type when we need something convertible to a Ref
   */
  type GenRef[-InT, +OutT, -From] = From => Ref[InT, OutT]



  implicit def assertToRefBoolean[InT](assert: Assert[InT]): Ref[InT, Boolean] =
    new Expr[InT, Res[Option[Boolean]]] {
      override def apply(lhs: InT): Res[Option[Boolean]] =
        assert.apply(lhs).map(_.toOption)
    }

  implicit def refBooleanToAssert[InT](refBoolean: Ref[InT, Boolean]): Assert[InT] =
    new Assert[InT] {
      override def apply(lhs: InT): Res[Kleene] =
        refBoolean(lhs).map(_.toKleene)
    }

  final case class And[-InT, +A1, +A2](left: A1, right: A2)
                                    (implicit e1: GenAssert[InT, A1], e2: GenAssert[InT, A2])
    extends Assert[InT] {
    override def apply(in: InT): Res[Kleene] =
      for { a <- left(in); b <- right(in) } yield a && b
  }

  final case class Or[-InT, +A1, +A2](left: A1, right: A2)
                                   (implicit e1: GenAssert[InT, A1], e2: GenAssert[InT, A2])
    extends Assert[InT] {
    override def apply(in: InT): Res[Kleene] =
      for { a <- left(in); b <- right(in)} yield a || b
  }
  final case class Not[-InT, +A](exp: A)
                               (implicit e1: GenAssert[InT, A])
    extends Assert[InT] {
    override def apply(in: InT): Res[Kleene] = for { a <- exp(in) } yield !a
  }

  final case class IsNull[-InT, +OutT, +R](exp: Ref[InT, OutT])
                                         (implicit e1: GenRef[InT, OutT, R]) extends Assert[InT] {
    override def apply(in: InT): Res[Kleene] = for { a <- exp(in) } yield a.isEmpty
  }

  final case class IsNotNull[-InT, +OutT, +R](exp: Ref[InT, OutT])
                                            (implicit e1: GenRef[InT, OutT, R])extends Assert[InT] {
    override def apply(in: InT): Res[Kleene] = for { a <- exp(in) } yield a.isDefined
  }

  final case class Eq[-InT, +OutT1, +OutT2, +R1, +R2](lhs: R1, rhs: R2)
                                                   (implicit e1: GenRef[InT, OutT1, R1], e2: GenRef[InT, OutT2, R2])
    extends Assert[InT] {
    def strictOp(a: Any, b: Any): Boolean = a == b

    override def apply(in: InT): Res[Kleene] = {
      for { optA <- lhs(in); optB <- rhs(in) } yield {
        for { a <- optA; b <- optB } yield
          strictOp(a, b)
      }
    }
  }


  /* FIXME: I am not sure if all types are covered here. ask @szarnyasg
     Currently supported types for the ordering assertions are: java.lang.String, java.lang.Float, java.lang.Integer
   */
  sealed trait OrderingAssert[-InT, +A, +B] extends Assert[InT] {
    val lhs: Ref[InT, A]
    val rhs: Ref[InT, B]
    def strictOp[T: Ordering](a: T, b: T): Boolean

    import NoOrdering._

    class NoOrdering(actual: Any)
      extends Exception(s"Type mismatch: expected ${float}, ${integer} or ${string} " +
        s"but was ${actual.getClass.getName}")
    object NoOrdering {
      val string = new java.lang.String("").getClass.getName
      val float = new java.lang.Float(0).getClass.getName
      val integer = new java.lang.Integer(0).getClass.getName
    }

    class CantCompare(expected: Any, actual: Any)
      extends Exception(s"Don't know how to compare that. Left: ${expected.toString} ${expected.getClass.getName}; " +
        s"Right: ${actual.toString} ${actual.getClass.getName}")

    override def apply(in: InT): Res[Kleene] = {
      val res = for { optA <- lhs(in); optB <- rhs(in) } yield {
        for { a <- optA; b <- optB } yield {
          (a, b) match {
            case (a: java.lang.String, b: java.lang.String) => \/-(strictOp(a, b))
            case (a: java.lang.String, b) => -\/(new CantCompare(a, b))
            case (a: java.lang.Float, b: java.lang.Float) => \/-(strictOp(a, b))
            case (a: java.lang.Float, b) => -\/(new CantCompare(a, b))
            case (a: java.lang.Integer, b: java.lang.Integer) =>  \/-(strictOp(a, b))
            case (a: java.lang.Integer, b) => -\/(new CantCompare(a, b))
            case (a, _) => -\/(new NoOrdering(a))
          }
        }
      }
      res.map((opt) => liftRes(opt)).flatMap(x => x.map(_.toKleene))
    }

    private def liftRes[A](option: Option[Res[A]]): Res[Option[A]] = {
      option match {
        case Some(left @ -\/(_)) => left
        case Some(\/-(a)) => \/-(Some(a))
        case None => \/-(None)
      }
    }
  }

  final case class Lt[-InT, +A, +B](override val lhs: Ref[InT, A],
                                    override val rhs: Ref[InT, B])
    extends OrderingAssert[InT, A, B] {
    override def strictOp[T: Ordering](a: T, b: T): Boolean = implicitly[Ordering[T]].lt(a, b)

  }
  final case class Lte[-InT, +A, +B](
                                  override val lhs: Ref[InT, A],
                                  override val rhs: Ref[InT, B])
    extends OrderingAssert[InT, A, B] {
    override def strictOp[T: Ordering](a: T, b: T): Boolean = implicitly[Ordering[T]].lteq(a, b)
  }
  final case class Gt[-InT, +A, +B](
                                     override val lhs: Ref[InT, A],
                                     override val rhs: Ref[InT, B])
    extends OrderingAssert[InT, A, B] {
    override def strictOp[T: Ordering](a: T, b: T): Boolean = implicitly[Ordering[T]].gt(a, b)
  }
  final case class Gte[-InT, +A, +B](
                                      override val lhs: Ref[InT, A],
                                      override val rhs: Ref[InT, B])
    extends OrderingAssert[InT, A, B] {
    override def strictOp[T: Ordering](a: T, b: T): Boolean = implicitly[Ordering[T]].gteq(a, b)
  }

  final case class Const[-InT, T](const: T) extends EExpr[InT, Some[T]] {
    override def apply(in: InT): Succ[Some[T]] = \/-(Some(const))
  }

  final case object True extends Assert[Any] {
    override def apply(in: Any): Succ[Kleene] = \/-(Kleene.True)
  }

  final case object Null extends Assert[Any] {
    override def apply(in: Any): Succ[Kleene] = \/-(Kleene.Null)
  }

  final case object False extends Assert[Any] {
    override def apply(in: Any): Succ[Kleene] = \/-(Kleene.False)
  }

  object This {

    case class Property[-Ctx <: Context[ElementT]](key: String) extends Ref[Ctx, Any] {
      override def apply(in: Ctx): Succ[Option[Any]] = {
        \/-(in.head.properties.get(key))
      }
    }

    def apply[Ctx <: Context[ElementT]] = new EExpr[Ctx, Some[ElementT]] {
      override def apply(in: Ctx): Succ[Some[ElementT]] = \/-(Some(in.head))
    }
  }

  case class Var[-Ctx <: Context[_]](key: String) extends Ref[Ctx, Any] {
    override def apply(in: Ctx): Succ[Option[Any]] = \/-(in.variables get key)
  }
}
