package trial.identification.kata

case class GenderParseFailure(invalidValue: String)

sealed trait Gender
object Gender {
  case object Male extends Gender
  case object Female extends Gender

  def fromString(stringValue: String): Either[GenderParseFailure, Gender] = {
    stringValue.toLowerCase() match {
      case "male"   => Right(Gender.Male)
      case "female" => Right(Gender.Female)
      case _        => Left(GenderParseFailure(stringValue))
    }
  }
}
