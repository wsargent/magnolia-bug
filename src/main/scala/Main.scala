import java.time.Instant
import java.util.Objects
import scala.collection.Seq
import com.tersesystems.echopraxia.api.{Field, Value}
import magnolia1.Magnolia

import scala.language.experimental.macros

object Main {
  def main(args: Array[String]): Unit = {
    val paymentInfo = PaymentInfo("41111111", Instant.now())
    val order = Order(paymentInfo = paymentInfo)
    val orderToClass = SemiAutoFieldBuilder.gen[Order]
    val orderValue = orderToClass.toValue(order)
  }
}

case class PaymentInfo(creditCardNumber: String, expirationDate: Instant)
final case class Order(paymentInfo: PaymentInfo)

trait ValueTypeClasses {

  trait ToValue[-T] {
    def toValue(t: T): Value[_]
  }

  object ToValue {
    def apply[T: ToValue](t: T): Value[_] = implicitly[ToValue[T]].toValue(t)
    implicit val valueToValue: ToValue[Value[_]] = identity(_)
    implicit def objectValueToValue[T: ToObjectValue]: ToValue[T] = ToObjectValue[T](_)
    implicit val stringToStringValue: ToValue[String] = (s: String) => Value.string(s)
  }

  trait ToObjectValue[-T] extends ToValue[T] {
    def toValue(t: T): Value.ObjectValue
  }

  object ToObjectValue {
    def apply[T: ToObjectValue](obj: T): Value.ObjectValue =
      implicitly[ToObjectValue[T]].toValue(obj)

    implicit val iterableToObjectValue: ToObjectValue[collection.Iterable[Field]] = t => Value.`object`(t.toArray: _*)

    implicit val immutableIterableToObjectValue: ToObjectValue[collection.immutable.Iterable[Field]] =
      t => Value.`object`(t.toArray: _*)
  }
}

trait Derivation extends ValueTypeClasses {
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
  implicit val paymentInfoToValue: ToValue[PaymentInfo] = gen[PaymentInfo]
  implicit val instantToValue: ToValue[Instant] = instant => ToValue(instant.toString)
  implicit val orderToValue: ToValue[Order] = gen[Order]
}
object SemiAutoFieldBuilder extends SemiAutoFieldBuilder