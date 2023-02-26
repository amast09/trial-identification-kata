package trial.identification.kata

import cats.data.NonEmptyList
import kantan.csv._
import kantan.csv.ops._

case class Trial(
    id: String,
    title: String,
    description: String,
    phase: Int,
    condition: String,
    anatomicSite: AnatomicSite,
    diagnoses: NonEmptyList[String],
    ageRequirement: AgeRequirement
)

object Trial {
  def apply(
      id: String,
      title: String,
      description: String,
      phase: Int,
      condition: String,
      anatomicSite: AnatomicSite,
      diagnoses: NonEmptyList[String],
      ageRequirement: AgeRequirement
  ): Trial = {
    new Trial(
      id,
      title,
      description,
      phase,
      condition,
      anatomicSite,
      diagnoses.map(_.trim),
      ageRequirement
    )
  }

  // Note: Can create different evaluators for trial evaluation algorithms,
  //  this evaluator implements the algorithm requested in the `INSTRUCTIONS.md`
  val kataEvaluator = TrialEvaluator(
    requiredCriteria = List(DiagnosisCriterion, AnatomicSiteCriterion),
    optionalCriteria = List(AgeCriterion)
  )(_)

  // Note: Currently the program blows up when there is invalid data. This could
  //  be improved with further requirements around how error handling should function.
  //  Currently this is a naive implementation expecting all data to be valid that will
  //  blow up immediately if it encounters any invalid data
  implicit val csvRowDecoder: RowDecoder[Trial] = {
    RowDecoder.decoder[
      String,
      String,
      String,
      Int,
      String,
      String,
      String,
      String,
      Trial
    ](0, 1, 2, 3, 4, 5, 6, 7)(
      (
          idColumnValue: String,
          titleColumnValue: String,
          descriptionColumnValue: String,
          phaseColumnValue: Int,
          conditionColumnValue: String,
          anatomicSiteColumnValue: String,
          diagnosesColumnValue: String,
          ageRequirementColumnValue: String
      ) => {
        val diagnosesList =
          diagnosesColumnValue.split("\\|").filter(_ != "").toList
        val diagnoses =
          NonEmptyList.fromList(diagnosesList).get
        val anatomicSite =
          AnatomicSite.fromString(anatomicSiteColumnValue).toOption.get
        val ageRequirement =
          AgeRequirement.fromString(ageRequirementColumnValue).toOption.get

          Trial(
            id = idColumnValue,
            title = titleColumnValue,
            description = descriptionColumnValue,
            phase = phaseColumnValue,
            condition = conditionColumnValue,
            anatomicSite = anatomicSite,
            diagnoses = diagnoses,
            ageRequirement = ageRequirement
          )
      }
    )
  }

  def trialsFromCSVFile(fileName: String): List[Trial] = getClass
    .getResource(fileName)
    .asCsvReader[Trial](rfc.withHeader)
    .toList
    .flatMap(_.toOption)
}
