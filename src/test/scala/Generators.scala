package trial.identification.kata

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import cats.data.NonEmptyList

object Generators {
  val anatomicSiteGen: Gen[AnatomicSite] = Gen.oneOf(
    AnatomicSite.Lung,
    AnatomicSite.Kidney,
    AnatomicSite.Brain,
    AnatomicSite.Bladder
  )

  val genderGen: Gen[Gender] = Gen.oneOf(Gender.Male, Gender.Female)

  val operatorGen: Gen[Operator] = Gen.oneOf(
    Operator.>,
    Operator.>=,
    Operator.<,
    Operator.<=,
    Operator.==
  )

  val ageGen: Gen[Age] =
    Gen.chooseNum[Int](0, 100).map(a => Age(a).toOption.get)

  val ageRequirementGen: Gen[AgeRequirement] = for {
    operator <- operatorGen
    age <- ageGen
  } yield AgeRequirement(operator, age)

  val alphaStringGen: Gen[String] = for {
    stringLength <- Gen.chooseNum[Int](1, 12)
    randomAlphaCharacters <- Gen.listOfN(stringLength, Gen.alphaChar)
  } yield randomAlphaCharacters.mkString

  val patientGen: Gen[Patient] =
    for {
      id <- Gen.uuid.map(_.toString)
      maybeAge <- Gen.option(ageGen)
      maybeGender <- Gen.option(genderGen)
      diagnosis <- alphaStringGen
      maybeAnatomicSite <- Gen.option(anatomicSiteGen)
    } yield Patient(
      id,
      maybeAge,
      maybeGender,
      diagnosis,
      maybeAnatomicSite
    )

  val validPatientForTrial: Gen[(Patient, Trial)] = for {
    patient <- patientGen
    trial <- trialGen
    // Match up the anatomic site
    anatomicSite <- anatomicSiteGen
    // Match up one of the trial diagnosis's with the patient diagnosis
    patientDiagnosis <- Gen.oneOf(trial.diagnoses.toList)
    // Match the patient's age with the age requirement
    patientAge = trial.ageRequirement.age
    ageRequirement = trial.ageRequirement.copy(operator = Operator.==)
  } yield (
    patient.copy(
      diagnosis = patientDiagnosis,
      maybeAnatomicSite = Some(anatomicSite),
      maybeAge = Some(patientAge)
    ),
    trial.copy(
      anatomicSite = anatomicSite,
      ageRequirement = ageRequirement
    )
  )

  val trialGen: Gen[Trial] =
    for {
      id <- Gen.uuid.map(_.toString)
      title <- alphaStringGen
      description <- alphaStringGen
      phase <- Gen.chooseNum[Int](1, 10)
      condition <- alphaStringGen
      anatomicSite <- anatomicSiteGen
      headDiagnosis <- alphaStringGen
      diagnoses <- Gen.listOf(alphaStringGen)
      ageRequirement <- ageRequirementGen
    } yield Trial(
      id,
      title,
      description,
      phase,
      condition,
      anatomicSite,
      NonEmptyList.ofInitLast(diagnoses, headDiagnosis),
      ageRequirement
    )

  case object PassingCriterion extends TrialCriterion {
    def evaluate(
        clinicalTrial: Trial,
        candidatePatient: Patient
    ): TrialCriterionEvaluation = TrialCriterionEvaluation.Pass
  }

  case object FailingCriterion extends TrialCriterion {
    def evaluate(
        clinicalTrial: Trial,
        candidatePatient: Patient
    ): TrialCriterionEvaluation = TrialCriterionEvaluation.Fail
  }

  case object InsufficientCriterion extends TrialCriterion {
    def evaluate(
        clinicalTrial: Trial,
        candidatePatient: Patient
    ): TrialCriterionEvaluation = TrialCriterionEvaluation.InsufficientData
  }

  val trialCriterionGen: Gen[TrialCriterion] =
    Gen.oneOf(PassingCriterion, FailingCriterion, InsufficientCriterion)
}
