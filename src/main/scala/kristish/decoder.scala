package kristish

import kristish.ast.JsonValue
import kristish.ast.JsonValue._
import shapeless._
import shapeless.labelled.FieldType
import cats.implicits._
import shapeless.labelled.field

object decoder {
  trait JsonDecoder[A] {
    def decode(json: JsonValue): Either[Throwable, A]
  }

  object JsonDecoder {
    def createDecoder[A](
        fn: JsonValue => Either[Throwable, A]): JsonDecoder[A] =
      x => fn(x)

    def wrapJsonEncoder[A](
        name: String,
        fn: PartialFunction[JsonValue, A]): JsonValue => Either[Throwable, A] =
      x => {
        fn.andThen(_.asRight[Throwable])
          .applyOrElse[JsonValue, Either[Throwable, A]](
            x,
            x1 => new Throwable(s"$x1 is not supported, expected $name").asLeft)
      }

    implicit val stringDecoder: JsonDecoder[String] =
      createDecoder(wrapJsonEncoder("string", {
        case JsonString(str) => str
      }))

    implicit val boolDecoder: JsonDecoder[Boolean] =
      createDecoder(wrapJsonEncoder("bool", {
        case JsonBoolean(bool) => bool
      }))

    implicit val intDecoder: JsonDecoder[Int] =
      createDecoder(wrapJsonEncoder("int", {
        case JsonNumber(int) => int.toInt
      }))

    implicit val longDecoder: JsonDecoder[Long] =
      createDecoder(wrapJsonEncoder("long", {
        case JsonNumber(long) => long.toLong
      }))

    implicit val doubleDecoder: JsonDecoder[Double] =
      createDecoder(wrapJsonEncoder("double", {
        case JsonNumber(double) => double
      }))

    implicit def optionDecoder[A](
        implicit dec: JsonDecoder[A]): JsonDecoder[Option[A]] =
      createDecoder {
        case JsonNull => none[A].asRight
        case other    => dec.decode(other).map(_.some)
      }

    implicit def listDecoder[A](
        implicit dec: JsonDecoder[A]): JsonDecoder[List[A]] = {
      createDecoder {
        case JsonArray(items) => items.traverse(dec.decode)
        case other            => new Throwable(s"unexpected $other, expected array").asLeft
      }
    }

    implicit val hnilDecoder: JsonDecoder[HNil] =
      createDecoder {
        case JsonObject(_) => HNil.asRight
        case other         => new Throwable(s"expected empty object, got $other").asLeft
      }

    implicit def hlistObjectDecoder[K <: Symbol, H, T <: HList](
        implicit
        witness: Witness.Aux[K],
        hDecoder: Lazy[JsonDecoder[H]],
        tDecoder: JsonDecoder[T]
    ): JsonDecoder[FieldType[K, H] :: T] = {
      createDecoder {
        case JsonObject(map) if map.nonEmpty =>
          val fieldName: String = witness.value.name
          val e = map.getOrElse(fieldName, JsonNull)

          for {
            head <- hDecoder.value.decode(e)
            tail <- tDecoder.decode(JsonObject(map))
          } yield field[K](head) :: tail
      }
    }

    implicit val cnilObjectDecoder: JsonDecoder[CNil] =
      createDecoder(_ => throw new Exception("Inconceivable!"))

    implicit def coproductObjectDecoder[K <: Symbol, H, T <: Coproduct](
        implicit
        witness: Witness.Aux[K],
        hDecoder: Lazy[JsonDecoder[H]],
        tDecoder: Lazy[JsonDecoder[T]]
    ): JsonDecoder[FieldType[K, H] :+: T] = {
      val typeName = witness.value.name
      createDecoder {
        case v @ JsonObject(map) =>
          map.get(typeName) match {
            case Some(value) =>
              hDecoder.value
                .decode(value)
                .map(x => Inl(field[K](x))) // .map(Inl.apply[H, T])
            case None =>
              tDecoder.value.decode(v).map(Inr.apply[FieldType[K, H], T])
          }
      }
    }

    implicit def objectDecoder[A, H](
        implicit
        labelledGeneric: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonDecoder[H]]): JsonDecoder[A] =
      createDecoder { json =>
        hEncoder.value.decode(json).map(labelledGeneric.from)
      }

    def apply[A](implicit enc: JsonDecoder[A]): JsonDecoder[A] = enc
  }
}
