package codecs

object app extends App{
  val decoded = implicitly[Decoder[Int]].decode(Json.Num(4.2))
  println(decoded)
}