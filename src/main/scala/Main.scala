import java.time.Instant
import java.util.Objects
import scala.collection.Seq
import com.tersesystems.echopraxia.api.{Field, Value}
import magnolia1.{CaseClass, Magnolia, SealedTrait}

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
    implicit val stringToStringValue: ToValue[String] = (s: String) => Value.string(s)
  }
}

trait SemiAutoFieldBuilder extends ValueTypeClasses {
  type Typeclass[T] = ToValue[T]

  final def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = {
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      value => param.typeclass.toValue(param.dereference(value))
    } else if (ctx.isObject) {
      value => Value.string(value.toString)
    } else {
       obj => {
        val fields: Seq[Field] = ctx.parameters.map { p =>
          val name: String = p.label
          val attribute = p.dereference(obj)
          val typeclassInstance = Objects.requireNonNull(p.typeclass, "type class is null!")
          val value: Value[_] = typeclassInstance.toValue(attribute)
          Field.keyValue(name, value)
        }
         Value.`object`(fields.toArray: _*)
      }
    }
  }

  // this is a sealed trait
  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = (value: T) => {
    ctx.split(value) { sub =>
      sub.typeclass.toValue(sub.cast(value))
    }
  }

  final def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  // XXX succeeds when defined before paymentInfoToValue
  //implicit val instantToValue: ToValue[Instant] = instant => Value.string(instant.toString)
  implicit val paymentInfoToValue: ToValue[PaymentInfo] = gen[PaymentInfo]
  // fails when defined _after_ paymentInfoToValue
  implicit val instantToValue: ToValue[Instant] = instant => Value.string(instant.toString)

  implicit val orderToValue: ToValue[Order] = gen[Order]
}
object SemiAutoFieldBuilder extends SemiAutoFieldBuilder