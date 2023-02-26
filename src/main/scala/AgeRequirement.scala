package trial.identification.kata
import trial.identification.kata.Operator.<
import scala.util.Try

sealed trait AgeRequirementParseFailure
case class UnableToParse(invalidAgeRequirement: String)
    extends AgeRequirementParseFailure

sealed trait AgeParseFailure extends AgeRequirementParseFailure
case class NegativeAge(invalidAge: Int) extends AgeParseFailure
case class InvalidIntegerValue(invalidAge: String) extends AgeParseFailure

sealed trait OperatorParseFailure extends AgeRequirementParseFailure
case class InvalidOperator(invalidOperator: String) extends OperatorParseFailure

case class Age(value: Int)
object Age {
  def apply(ageValue: Int): Either[AgeParseFailure, Age] = {
    if (ageValue >= 0) {
      Right(new Age(ageValue))
    } else {
      Left(NegativeAge(ageValue))
    }
  }

  def fromString(ageValue: String): Either[AgeParseFailure, Age] = {
    Try(ageValue.toInt).toOption match {
      case Some(age) => Age(age)
      case None      => Left(InvalidIntegerValue(ageValue))
    }
  }
}

sealed trait Operator {
  override def toString(): String = {
    this match {
      case Operator.>  => ">"
      case Operator.>= => ">="
      case Operator.<  => "<"
      case Operator.<= => "<="
      case Operator.== => "="
    }
  }
}
object Operator {
  case object > extends Operator
  case object >= extends Operator
  case object < extends Operator
  case object <= extends Operator
  case object == extends Operator

  def fromString(
      stringOperator: String
  ): Either[OperatorParseFailure, Operator] = {
    stringOperator match {
      case ">"                => Right(>)
      case ">="               => Right(>=)
      case "<"                => Right(<)
      case "<="               => Right(<=)
      case "="                => Right(==)
      case invalidStringValue => Left(InvalidOperator(invalidStringValue))
    }
  }
}

// Note: Could be expanded in the future with additional requirements
//  for example: "">18 && <60"
//  but stuck with the single expression represented in the existing data
case class AgeRequirement(operator: Operator, age: Age) {
  def verify(candidateAge: Age): Boolean = {
    val ageValue = age.value
    val candidateAgeValue = candidateAge.value

    this.operator match {
      case Operator.<  => candidateAgeValue < ageValue
      case Operator.<= => candidateAgeValue <= ageValue
      case Operator.>  => candidateAgeValue > ageValue
      case Operator.>= => candidateAgeValue >= ageValue
      case Operator.== => candidateAgeValue == ageValue
    }
  }
}
object AgeRequirement {
  private val AGE_RANGE_REGEX = "^\\s*(>|>=|<|<=|=)\\s*(\\d*)\\s*$".r

  def fromString(
      ageRangeString: String
  ): Either[AgeRequirementParseFailure, AgeRequirement] = {
    ageRangeString match {
      case AGE_RANGE_REGEX(operatorString, ageString) => {
        for {
          operator <- Operator.fromString(operatorString)
          age <- Age.fromString(ageString)
        } yield AgeRequirement(operator, age)
      }
      case invalidString => Left(UnableToParse(invalidString))
    }
  }
}
