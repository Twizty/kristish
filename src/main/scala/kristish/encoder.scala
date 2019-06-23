package kristish

import kristish.ast.JsonValue
import kristish.ast.JsonValue._
import shapeless._
import shapeless.labelled.FieldType

object encoder {
  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

    def createEncoder[A](fn: A => JsonValue): JsonEncoder[A] = x => fn(x)

    implicit val stringEncoder: JsonEncoder[String] =
      createEncoder(JsonString.apply)
    implicit val booleanEncoder: JsonEncoder[Boolean] =
      createEncoder(JsonBoolean.apply)
    implicit val intEncoder: JsonEncoder[Int] = createEncoder(
      x => JsonNumber(x))
    implicit val longEncoder: JsonEncoder[Long] = createEncoder(
      x => JsonNumber(x))
    implicit val doubleEncoder: JsonEncoder[Double] =
      createEncoder(JsonNumber.apply)

    implicit def listEncoder[A](
        implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
      createEncoder(list => JsonArray(list.map(enc.encode)))

    implicit def optionEncoder[A](
        implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
      createEncoder(_.map(enc.encode).getOrElse(JsonNull))

    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
      fn.apply

    implicit val hnilEncoder: JsonObjectEncoder[HNil] =
      createObjectEncoder(_ => JsonObject(Map.empty))

    implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
        implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName: String = witness.value.name
      createObjectEncoder { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        val tail = tEncoder.encode(hlist.tail)
        JsonObject(Map(fieldName -> head) ++ tail.fields)
      }
    }

    implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
      createObjectEncoder(_ => throw new Exception("Inconceivable!"))

    implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
        implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
      val typeName = witness.value.name
      createObjectEncoder {
        case Inl(h) =>
          JsonObject(Map(typeName -> hEncoder.value.encode(h)))
        case Inr(t) =>
          tEncoder.encode(t)
      }
    }

    implicit def objectEncoder[A, H](
        implicit
        labelledGeneric: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonObjectEncoder[H]]): JsonEncoder[A] =
      createEncoder { a =>
        val product = labelledGeneric.to(a)
        hEncoder.value.encode(product)
      }
  }
}
