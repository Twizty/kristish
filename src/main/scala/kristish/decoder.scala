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

    def wrapJsonDecoderF[A](name: String)(
        fn: PartialFunction[JsonValue, Either[Throwable, A]])
      : JsonValue => Either[Throwable, A] =
      x => {
        fn.applyOrElse[JsonValue, Either[Throwable, A]](
          x,
          x1 => new Throwable(s"$x1 is not supported, expected $name").asLeft)
      }

    def wrapJsonDecoder[A](name: String)(
        fn: PartialFunction[JsonValue, A]): JsonValue => Either[Throwable, A] =
      wrapJsonDecoderF(name)(fn.andThen(_.asRight))

    implicit val stringDecoder: JsonDecoder[String] =
      createDecoder(wrapJsonDecoder("string") {
        case JsonString(str) => str
      })

    implicit val boolDecoder: JsonDecoder[Boolean] =
      createDecoder(wrapJsonDecoder("bool") {
        case JsonBoolean(bool) => bool
      })

    implicit val intDecoder: JsonDecoder[Int] =
      createDecoder(wrapJsonDecoder("int") {
        case JsonNumber(int) => int.toInt
      })

    implicit val longDecoder: JsonDecoder[Long] =
      createDecoder(wrapJsonDecoder("long") {
        case JsonNumber(long) => long.toLong
      })

    implicit val doubleDecoder: JsonDecoder[Double] =
      createDecoder(wrapJsonDecoder("double") {
        case JsonNumber(double) => double
      })

    implicit def optionDecoder[A](
        implicit dec: JsonDecoder[A]): JsonDecoder[Option[A]] =
      createDecoder {
        case JsonNull => none[A].asRight
        case other    => dec.decode(other).map(_.some)
      }

    implicit def listDecoder[A](
        implicit dec: JsonDecoder[A]): JsonDecoder[List[A]] = {
      createDecoder(wrapJsonDecoderF("array") {
        case JsonArray(items) => items.traverse(dec.decode)
      })
    }

    implicit val hnilDecoder: JsonDecoder[HNil] =
      createDecoder(wrapJsonDecoderF("empty object") {
        case JsonObject(_) => HNil.asRight
      })

    implicit def hlistObjectDecoder[K <: Symbol, H, T <: HList](
        implicit
        witness: Witness.Aux[K],
        hDecoder: Lazy[JsonDecoder[H]],
        tDecoder: JsonDecoder[T]
    ): JsonDecoder[FieldType[K, H] :: T] = {
      createDecoder(
        wrapJsonDecoderF("non empty object") {
          case JsonObject(map) if map.nonEmpty =>
            val fieldName: String = witness.value.name
            val e = map.getOrElse(fieldName, JsonNull)

            for {
              head <- hDecoder.value.decode(e)
              tail <- tDecoder.decode(JsonObject(map))
            } yield field[K](head) :: tail
        }
      )
    }

    implicit val cnilObjectDecoder: JsonDecoder[CNil] =
      createDecoder(_ => new Throwable("unexpected CNil").asLeft)

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
