## Trial Identification Kata

One of our main goals at [Paradigm](https://paradigm.inc) is to identify patients that may be eligible for clinical trials. A patient may be eligible for a trial based on numerous criteria. Some are fairly straightforward, such as matching on a age range, and others are more challenging, such as determining a patient's treatment plan.

Your task for this kata is to build a simplified version of a trial identification engine.

In the data folder there are two files (`clinical_trial.csv` and `patient_feed.csv`). The clinical trial csv contains the information for single clinical trial you will be identifying patients for. The patient feed csv contains a list of patients that you will be matching against this trial. These two files must be read in at the start of your program.

As reviewers of your kata we have some basic assumptions for how the kata is completed. 

- This kata is language agnostic as long as you can effectively complete a program that fulfills the acceptance criteria
- There should be a single entry point to the program that can run in a single command to produce the output expected by the acceptance criteria
- The program should be idempotent, if it is run multiple times in a row, the output should be exactly the same
- The input and output of the program is text, so there is not an expectation of a graphical user interface
- Please feel free to email us questions about the acceptance criteria!

### Acceptance Criteria

#### 1. When the trial identification program is run, the system should log each of the trials that is being processed and the number of patients that are being considered.

_Example:_

```
Processing trial id NCT03840902 for condition Non-small Cell Lung Cancer with 8 potential patients
```

#### 2. When the trial identification program is run, each patient should be evaluated for each trial. The system should log every time this evaluation is happening.

_Example:_
```
Processing patient \<patient_id>, age \<age>, gender \<gender>, with diagnosis \<diagnosis>, for trial NCT03840902
```

#### 3. When a patient is evaulated, they should be checked against each of the criterion defined for the trial.

**Age** - Ensure that the patient's age falls within the defined range of the trial (e.g. greater than 18)

**Diagnosis** - Ensure that the patient's diagnosis matches one of the diagnoses defined on the trial. (note: the diagnosis list is a pipe delimited list within the csv). This match should be case _insensitive_.

**Anatomic Site** - Ensure that the anatomic site provided for the patient's diagnosis matches what is defined within the trial. This match should be case _insensitive_.

#### 4. When a patient is evaluated, they are considered identified for a trial if they meet _all_ of the required criteria. For criteria that are not required, a patient can still be considered identified if their data matches or is not provided. If the data is available and does not match the criterion, the patient is _not_ considered identified.

**Age** - Not required

**Diagnosis** - Required

**Anatomic Site** - Required

#### 5. When all the patients are finished, it should log all of the patient's that were identified for each of the trials.

_Example_:
```
The following 2 patients were identified for trial NCT03840902:
 -- Patient 1
 -- Patient 5
```

## Submitting Your Kata
To submit your work to Paradigm, please email either a git bundle or zip file
of our code. Please include any necessary instructions or requirements for running your code.

## THANK YOU!
From everyone at Paradigm, we'd like to thank you for taking the time to put together your solution. We know we're
asking you to take time away from family, friends, or life in general to put this together. Regardless of the
outcome, that means the world to us. You rock!



