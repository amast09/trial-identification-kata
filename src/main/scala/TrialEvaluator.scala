package trial.identification.kata

sealed trait TrialCriterionEvaluation
object TrialCriterionEvaluation {
  case object Pass extends TrialCriterionEvaluation
  case object Fail extends TrialCriterionEvaluation
  case object InsufficientData extends TrialCriterionEvaluation
}

trait TrialCriterion {
  def evaluate(
      clinicalTrial: Trial,
      candidatePatient: Patient
  ): TrialCriterionEvaluation
}

case object AgeCriterion extends TrialCriterion {
  def evaluate(
      trial: Trial,
      candidatePatient: Patient
  ): TrialCriterionEvaluation = {
    val maybeVerifiedAgeRequirement =
      candidatePatient.maybeAge.map(trial.ageRequirement.verify)

    maybeVerifiedAgeRequirement match {
      case Some(true)  => TrialCriterionEvaluation.Pass
      case Some(false) => TrialCriterionEvaluation.Fail
      case None        => TrialCriterionEvaluation.InsufficientData
    }
  }
}

case object DiagnosisCriterion extends TrialCriterion {
  def evaluate(
      trial: Trial,
      candidatePatient: Patient
  ): TrialCriterionEvaluation = {
    trial.diagnoses
      .map(_.toLowerCase)
      .exists(trialDiagnosis =>
        // Note: I think this is the behavior required, it could be adjusted based on requirements changing though
        candidatePatient.diagnosis.toLowerCase().endsWith(trialDiagnosis)
      ) match {
      case true  => TrialCriterionEvaluation.Pass
      case false => TrialCriterionEvaluation.Fail
    }
  }
}

case object AnatomicSiteCriterion extends TrialCriterion {
  def evaluate(
      trial: Trial,
      candidatePatient: Patient
  ): TrialCriterionEvaluation = {
    val maybeSatisfiedDiagnosis =
      candidatePatient.maybeAnatomicSite.map(_ == trial.anatomicSite)

    maybeSatisfiedDiagnosis match {
      case Some(true)  => TrialCriterionEvaluation.Pass
      case Some(false) => TrialCriterionEvaluation.Fail
      case None        => TrialCriterionEvaluation.InsufficientData
    }
  }
}

case class TrialEvaluator(
    requiredCriteria: List[TrialCriterion],
    optionalCriteria: List[TrialCriterion]
)(trial: Trial) {
  def isPatientEligible(candidatePatient: Patient): Boolean = {
    val patientSatisfiesRequiredCriteria =
      requiredCriteria.forall((criterion) => {
        criterion.evaluate(
          trial,
          candidatePatient
        ) == TrialCriterionEvaluation.Pass
      })
    val patientSatisfiesOptionalCriteria =
      optionalCriteria.forall(criterion => {
        val criterionEvaluationResult =
          criterion.evaluate(trial, candidatePatient)
        criterionEvaluationResult == TrialCriterionEvaluation.Pass || criterionEvaluationResult == TrialCriterionEvaluation.InsufficientData
      })

    patientSatisfiesRequiredCriteria && patientSatisfiesOptionalCriteria
  }
}
