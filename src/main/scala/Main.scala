import java.time.Instant
import java.util.Objects
import scala.annotation.implicitNotFound
import scala.collection.Seq
import com.tersesystems.echopraxia.api.{Field, Value}
import magnolia1.Magnolia

import scala.language.experimental.macros

object Main {
  def main(args: Array[String]): Unit = {
    val paymentInfo = PaymentInfo("41111111", Instant.now())
    val shippingInfo = ShippingInfo("address 1", "address 2")
    val sku1 = Sku(232313, "some furniture")
    val lineItems = Seq(LineItem(sku1, 1))
    val user = User("user1", 2342331)
    val order = Order(paymentInfo = paymentInfo, shippingInfo = shippingInfo, lineItems = lineItems, owner = user)
    val orderToClass = SemiAutoFieldBuilder.gen[Order]
    val orderValue = orderToClass.toValue(order)
  }
}

case class User(name: String, id: Int)
case class Sku(id: Int, description: String)
case class LineItem(sku: Sku, quantity: Int)
case class PaymentInfo(creditCardNumber: String, expirationDate: Instant)
case class ShippingInfo(address1: String, address2: String)
final case class Order(paymentInfo: PaymentInfo, shippingInfo: ShippingInfo, lineItems: Seq[LineItem], owner: User)

trait ValueTypeClasses {

  @implicitNotFound("Could not find an implicit ToValue[${T}]")
  trait ToValue[-T] {
    def toValue(t: T): Value[_]
  }

  object ToValue {
    def apply[T: ToValue](t: T): Value[_] = implicitly[ToValue[T]].toValue(t)
    implicit val valueToValue: ToValue[Value[_]] = identity(_)
    implicit def objectValueToValue[T: ToObjectValue]: ToValue[T] = ToObjectValue[T](_)
    implicit def arrayValueToValue[T: ToArrayValue]: ToValue[T]   = ToArrayValue[T](_)
    implicit val stringToStringValue: ToValue[String] = (s: String) => Value.string(s)
    implicit val byteToValue: ToValue[Byte]             = (byte: Byte) => Value.number(byte: java.lang.Byte)
    implicit val shortToValue: ToValue[Short]           = (short: Short) => Value.number(short: java.lang.Short)
    implicit val intToValue: ToValue[Int]               = (int: Int) => Value.number(int: java.lang.Integer)
    implicit val longToValue: ToValue[Long]             = (long: Long) => Value.number(long: java.lang.Long)
    implicit val doubleToValue: ToValue[Double]         = (double: Double) => Value.number(double: java.lang.Double)
    implicit val floatToValue: ToValue[Float]           = (float: Float) => Value.number(float: java.lang.Float)
    implicit val bigIntToValue: ToValue[BigInt]         = (bigInt: BigInt) => Value.number(bigInt.bigInteger)
    implicit val bigDecimalToValue: ToValue[BigDecimal] = (bigDec: BigDecimal) => Value.number(bigDec.bigDecimal)
    implicit val booleanToBoolValue: ToValue[Boolean]            = bool => Value.bool(bool)
    implicit val javaBoolToBoolValue: ToValue[java.lang.Boolean] = bool => Value.bool(bool)
    implicit val throwableToValue: ToValue[Throwable] = e => Value.exception(e)
  }

  @implicitNotFound("Could not find an implicit ToArrayValue[${T}]")
  trait ToArrayValue[-T] extends ToValue[T] {
    def toValue(t: T): Value.ArrayValue
  }

  object ToArrayValue {

    def apply[T: ToArrayValue](array: T): Value.ArrayValue =
      implicitly[ToArrayValue[T]].toValue(array)

    implicit val identityArrayValue: ToArrayValue[Value.ArrayValue] = identity(_)

    implicit def iterableToArrayValue[V: ToValue]: ToArrayValue[collection.Iterable[V]] =
      iterable => Value.array(iterable.map(ToValue[V]).toArray: _*)

    implicit def immutableIterableToArrayValue[V: ToValue]: ToArrayValue[collection.immutable.Iterable[V]] =
      iterable => Value.array(iterable.map(ToValue[V]).toArray: _*)

    implicit def arrayToArrayValue[V: ToValue]: ToArrayValue[Array[V]] = array => {
      Value.array(array.map(ToValue[V]): _*)
    }
  }

  @implicitNotFound("Could not find an implicit ToObjectValue[${T}]")
  trait ToObjectValue[-T] extends ToValue[T] {
    def toValue(t: T): Value.ObjectValue
  }

  object ToObjectValue {
    def apply[T: ToObjectValue](obj: T): Value.ObjectValue =
      implicitly[ToObjectValue[T]].toValue(obj)

    def apply(fields: Field*): Value.ObjectValue = Value.`object`(fields: _*)

    implicit val identityObjectValue: ToObjectValue[Value.ObjectValue] = identity(_)

    implicit val fieldToObjectValue: ToObjectValue[Field] = f => Value.`object`(f)

    implicit val iterableToObjectValue: ToObjectValue[collection.Iterable[Field]] = t => Value.`object`(t.toArray: _*)

    implicit val immutableIterableToObjectValue: ToObjectValue[collection.immutable.Iterable[Field]] =
      t => Value.`object`(t.toArray: _*)
  }
}

sealed trait Derivation extends ValueTypeClasses {
  type Typeclass[T] = ToValue[T]

  type CaseClass[T]   = magnolia1.CaseClass[Typeclass, T]
  type SealedTrait[T] = magnolia1.SealedTrait[Typeclass, T]

  final def join[T](ctx: CaseClass[T]): Typeclass[T] = {
    if (ctx.isValueClass) {
      joinValueClass(ctx)
    } else if (ctx.isObject) {
      joinCaseObject(ctx)
    } else {
      joinCaseClass(ctx)
    }
  }

  // this is a regular case class
  protected def joinCaseClass[T](ctx: CaseClass[T]): Typeclass[T] = { obj =>
    val typeInfo = Field.keyValue("@type", ToValue(ctx.typeName.full))
    val fields: Seq[Field] = ctx.parameters.map { p =>
      val name: String = p.label
      val attribute = p.dereference(obj)
      val typeclassInstance = Objects.requireNonNull(p.typeclass, "type class is null!")
      val value: Value[_] = typeclassInstance.toValue(attribute)
      Field.keyValue(name, value)
    }
    ToObjectValue(typeInfo +: fields)
  }

  // this is a case object, we can't do anything with it.
  protected def joinCaseObject[T](ctx: CaseClass[T]): Typeclass[T] = {
    // ctx has no parameters, so we're better off just passing it straight through.
    value => Value.string(value.toString)
  }

  // this is a value class aka AnyVal, we should pass it through.
  protected def joinValueClass[T](ctx: CaseClass[T]): Typeclass[T] = {
    val param = ctx.parameters.head
    value => param.typeclass.toValue(param.dereference(value))
  }

  // this is a sealed trait
  def split[T](ctx: SealedTrait[T]): Typeclass[T] = (value: T) => {
    ctx.split(value) { sub =>
      sub.typeclass.toValue(sub.cast(value))
    }
  }
}

trait SemiAutoDerivation extends Derivation {
  final def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait SemiAutoFieldBuilder extends SemiAutoDerivation {
  implicit val userToValue: ToValue[User] = gen[User]
  implicit val lineItemToValue: ToValue[LineItem] = gen[LineItem]
  implicit val skuToValue: ToValue[Sku] = gen[Sku]
  implicit val paymentInfoToValue: ToValue[PaymentInfo] = gen[PaymentInfo]
  implicit val shippingInfoToValue: ToValue[ShippingInfo] = gen[ShippingInfo]
  implicit val orderToValue: ToValue[Order] = gen[Order]
  implicit val instantToValue: ToValue[Instant] = instant => ToValue(instant.toString)
}
object SemiAutoFieldBuilder extends SemiAutoFieldBuilder