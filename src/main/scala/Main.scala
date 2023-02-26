package trial.identification.kata

import kantan.csv._
import kantan.csv.ops._

object Main {

  def main(args: Array[String]) = {
    // Note: it wouldn't be too difficult to move these into streams to
    //  avoid loading all patient data and trial data into memory at once
    val trials = Trial.trialsFromCSVFile("/clinical_trial.csv")
    val patients = Patient.patientsFromCSVFile("/patient_feed.csv")

    val trialsWithEligiblePatients = trials.foldLeft(
      List.empty[(Trial, List[Patient])]
    )((trialsWithPatients, nextTrial) => {
      printTrialBeingProcessed(nextTrial, patients.length)

      val trialEvaluator = Trial.kataEvaluator(nextTrial)
      val eligiblePatients = patients
        .tapEach(printPatientBeingProcessed(nextTrial.id))
        .filter(trialEvaluator.isPatientEligible)

      trialsWithPatients :+ (nextTrial, eligiblePatients)
    })

    trialsWithEligiblePatients.foreach(printTrialEvaluationResult)
  }

  def printTrialBeingProcessed(trial: Trial, numberOfPatients: Int): Unit = {
    println(
      s"Processing trial id ${trial.id} for condition ${trial.condition} with ${numberOfPatients} potential patients"
    )
  }

  def printPatientBeingProcessed(trialId: String)(patient: Patient): Unit = {
    val patientAge = patient.maybeAge
      .map(age => s" age ${age.value},")
      .getOrElse("")
    val patientGender = patient.maybeGender
      .map(gender => s" gender ${gender},")
      .getOrElse("")

    println(
      s"Processing patient ${patient.id},${patientAge}${patientGender} with diagnosis ${patient.diagnosis}, for trial ${trialId}"
    )
  }

  def printEligiblePatient(patient: Patient): Unit = {
    println(s"-- Patient ${patient.id}")
  }

  def printTrialEvaluationResult(
      trialWithEligiblePatients: (Trial, List[Patient])
  ): Unit = {
    trialWithEligiblePatients match {
      case (trial, Nil) =>
        println(s"No patients were identified for trial ${trial.id}")
      case (trial, patient :: Nil) => {
        println(s"The following patient was identified for trial ${trial.id}:")
        printEligiblePatient(patient)
      }
      case (trial, patients) => {
        println(
          s"The following ${patients.length} patients were identified for trial ${trial.id}:"
        )
        patients.foreach(printEligiblePatient)
      }
    }
  }
}
