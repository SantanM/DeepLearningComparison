
if (!require("remotes")) install.packages("remotes"); library(remotes)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("Strategus")) remotes::install_github('ohdsi/Strategus@v0.1.0', upgrade = "never"); library(Strategus)
if (!require("PatientLevelPrediction")) remotes::install_github('ohdsi/PatientLevelPrediction@6.3.5', upgrade = "never"); library(PatientLevelPrediction)
if (!require("DeepPatientLevelPrediction")) remotes::install_github('ohdsi/DeepPatientLevelPrediction@v2.0.2', upgrade = "never"); library(DeepPatientLevelPrediction)

# MODEL TRANSFER ----------------------------------------------------------

source('https://raw.githubusercontent.com/OHDSI/ModelTransferModule/v0.0.10/SettingsFunctions.R')

bucket <- "s3://ohdsi-dlc/"
region <- "eu-west-1"
analysisIds <- list(dementia = c("a5", "a6"),
                    lungcancer = c("a1", "a2"),
                    bipolar = c("a3", "a4"))
allModels <- aws.s3::get_bucket_df(bucket = bucket, prefix = "models")$Key

organizedModels <- list()                    
s3Settings <- list()
for (analysis in names(analysisIds)) {
  
  analysisKey <- analysisIds[[analysis]]
  
  organizedModels[[analysis]] <-
    unlist(lapply(analysisKey, function(x) allModels[grepl(glob2rx(paste0("*-",x,".*")), 
                                                            allModels)]))
  s3Settings[[analysis]] <- tibble(
    modelZipLocation = organizedModels[[analysis]],
    bucket = bucket,
    region = region
  )
}

modelTransferModuleSpecsDementia <- createModelTransferModuleSpecifications(
  s3Settings = s3Settings$dementia
)

modelTransferModuleSpecsLungcancer <- createModelTransferModuleSpecifications(
  s3Settings = s3Settings$lungcancer
)

modelTransferModuleSpecsBipolar <- createModelTransferModuleSpecifications(
  s3Settings = s3Settings$bipolar
)

# COHORTS -----------------------------------------------------------------

cohortIds <- list(dementia = list(target = 11931, outcome = 6243),
                  lungCancer = list(target = 11932, outcome = 298),
                  bipolar = list(target = 11454, outcome = 10461))

cohortIds <- list(rapso = list(target=6372, outcome = 10657)

# EXTRACTING COHORTS
baseUrl <- keyring::key_get('webapi', 'baseurl')
ROhdsiWebApi::authorizeWebApi(
  baseUrl = baseUrl,
  authMethod = 'windows',
  webApiUsername = keyring::key_get('webapi', 'username'),
  webApiPassword = keyring::key_get('webapi', 'password')
)

cohortDefinitions <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = unlist(cohortIds),
  generateStats = F
)
# modify the cohort
cohortDefinitions <- lapply(1:length(cohortDefinitions$atlasId), function(i){list(
  cohortId = cohortDefinitions$cohortId[i],
  cohortName = cohortDefinitions$cohortName[i],
  cohortDefinition = cohortDefinitions$json[i]
)})

createCohortSharedResource <- function(cohortDefinitionSet) {
  sharedResource <- list(cohortDefinitions = cohortDefinitionSet)
  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}


# COHORT GENERATION SETTINGS

# source the cohort generator settings function
source("https://raw.githubusercontent.com/OHDSI/CohortGeneratorModule/v0.2.1/SettingsFunctions.R")
# this loads a function called createCohortGeneratorModuleSpecifications that takes as
# input incremental (boolean) and generateStats (boolean)

# specify the inputs to create the cohort generator specification
cohortGeneratorModuleSpecifications <- createCohortGeneratorModuleSpecifications(
  incremental = TRUE,
  generateStats = F
)

# UNIVERSAL ANALYSIS SETTINGS ---------------------------------------------

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = T,
  useDemographicsAge = T,
  useConditionOccurrenceLongTerm  = T,
  useDrugEraLongTerm = T,
  useCharlsonIndex = T,
  longTermStartDays = -365,
  endDays = 0
)

restrictPlpDataSettings <- createRestrictPlpDataSettings(
  sampleSize = NULL,
)

#  POPULATION SETTINGS ----------------------------------------------------

# lung cancer
lungCancerPopulationSettings <- createStudyPopulationSettings(
  removeSubjectsWithPriorOutcome = T,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk = 1,
  riskWindowStart = 1,
  startAnchor = 'cohort start',
  riskWindowEnd = 1095,
  endAnchor = 'cohort start'
)

# dementia
dementiaPopulationSettings <- createStudyPopulationSettings(
  binary = T,
  includeAllOutcomes = T,
  firstExposureOnly = T,
  washoutPeriod = 365,
  removeSubjectsWithPriorOutcome = F,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk = 1,
  riskWindowStart = 1,
  startAnchor = 'cohort start',
  endAnchor = 'cohort start',
  riskWindowEnd = 1825
)

# bipolar
bipolarPopulationSettings <- createStudyPopulationSettings(
  removeSubjectsWithPriorOutcome = T,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk = 1,
  riskWindowStart = 1,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = 'cohort start'
)


source('https://raw.githubusercontent.com/OHDSI/PatientLevelPredictionValidationModule/v0.0.11/SettingsFunctions.R')
source('https://raw.githubusercontent.com/OHDSI/DeepPatientLevelPredictionValidationModule/v0.0.3/SettingsFunctions.R')

validationComponentsList <- list(
  list(
    targetId = cohortIds$dementia$target,
    outcomeId = cohortIds$dementia$outcome,
    restrictPlpDataSettings = restrictPlpDataSettings,
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    ),
    populationSettings = dementiaPopulationSettings  
  ),
  list(
    targetId = cohortIds$lungcancer$target,
    outcomeId = cohortIds$lungcacner$outcome,
    restrictPlpDataSettings = restrictPlpDataSettings,
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    ),
    populationSettings = lungCancerPopulationSettings
  ),
  list(
    targetId = cohortIds$bipolar$target,
    outcomeId = cohortIds$bipolar$outcome,
    restrictPlpDataSettings = restrictPlpDataSettings,
    validationSettings = PatientLevelPrediction::createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = TRUE
    ),
    populationSettings = bipolarPopulationSettings
  )
  
)

predictionValidationModuleSpecifications <- createPatientLevelPredictionValidationModuleSpecifications(
  validationComponentsList = validationComponentsList
)

# predictionValidationModuleSpecificationsDeep <- createDeepPatientLevelPredictionValidationModuleSpecifications(
#   validationComponentsList = validationComponentsList
# )

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addModuleSpecifications(modelTransferModuleSpecsDementia) |>
  addModuleSpecifications(modelTransferModuleSpecsBipolar) |>
  addModuleSpecifications(modelTransferModuleSpecsLungcancer) |>
  addSharedResources(createCohortSharedResource(cohortDefinitions)) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(predictionValidationModuleSpecifications) # |>
  # addModuleSpecifications(predictionValidationModuleSpecificationsDeep)

# SAVING TO SHARE
ParallelLogger::saveSettingsToJson(analysisSpecifications, 'deep_comp_dementia_val_study.json')

