// Deriving with magnolia
//
trait ToBsonValue[-T]
// B <: A
// TBV[A] <: TBV[B] ??
class A
class B extends A
lazy val tbv_A: ToBsonValue[A] = ???
lazy val tvb_B: ToBsonValue[B] = tbv_A

trait FromBsonValue[+T]

trait BsonCodec[T] extends ToBsonValue[T] with FromBsonValue[T]


object DeriveCodec {

  def apply[T](implicit ev1: DeriveFrom[T], ev2: DeriveTo[T]): BsonCodec[T] = {
    val from = ev1.derive
    val to = ev2.derive
    new BsonCodec[T] {
      // use from and to
    }
  }
}

trait DeriveFrom[T] {
  def derive: FromBsonValue[T]
}

trait DeriveTo[T] {
  def derive: ToBsonValue[T]
}

object DeriveFrom {
  def apply[T](implicit ev: DeriveFrom[T]): FromBsonValue[T] = ev.derive

  type TypeClass[T] = FromBsonValue[T]

}

object DeriveTo {
  def apply[T](implicit ev: DeriveTo[T]): ToBsonValue[T] = ev.derive

  type TypeClass[T] = ToBsonValue[T]

}
