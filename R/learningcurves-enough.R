# Experiment to generate learning curves

library(methods)
library(RSSL)
library(createdatasets)
library(randomForest)
library(parallel)

set.seed(42)

setdatadir("data")

measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest)

detectBatchCPUs <- function() { 
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) 
  if (is.na(ncores)) { 
    ncores <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE")) 
  } 
  if (is.na(ncores)) {
    ncores <- parallel::detectCores()
  } 
  if (is.na(ncores)) {
    stop("Can't detect number of cores.")
  } 
  return(ncores) 
}

datasets<-list("Haberman"=createHaberman(),
               "Ionosphere"=createIonosphere(),
               "Parkinsons"=createParkinsons(),
               "Diabetes"=na.roughfix(createDiabetes()),
               "Sonar"=createSonar(),
               "SPECT"=createSPECT(),
               "SPECTF"=createSPECTF(),
               "Transfusion"=createTransfusion(),
               "WDBC"=createWDBC(),
               "Mammography"=na.roughfix(createMammographicMass()),
               "Digit1"=createDigit1(),
               "USPS"=createUSPS(),
               "COIL2"=createCOIL2(),
               "BCI"=createBCI(),
               "g241c"=createG241C(),
               "g241d"=createG241N())

models <- list("Haberman"=formula(Survival~.),
               "Ionosphere"=formula(Return~.),
               "Parkinsons"=formula(status~ . -subject -recording),
               "Diabetes"=formula(Diabetes~.),
               "Sonar"=formula(Label ~ .),
               "SPECT"=formula(Diagnosis ~ .),
               "SPECTF"=formula(Diagnosis ~ .),
               "Transfusion"=formula(Donated ~ .),
               "WDBC"=formula(Diagnosis ~ . -ID),
               "Mammography"=formula(Severity ~ . -BIRADS),
               "Digit1"=formula(Class ~ .),
               "USPS"=formula(Class ~ .),
               "COIL2"=formula(Class ~ .),
               "BCI"=formula(Class ~ .),
               "g241c"=formula(Class ~ .),
               "g241d"=formula(Class ~ .))

classifiers <- list(
  "Supervised" = function(X,y,X_u,y_u) {
    LeastSquaresClassifier(X,y)
  },
  "Soft" = function(X,y,X_u,y_u) {
    EMLeastSquaresClassifier(X,y,X_u, eps = 1e-8, method="block", objective="soft", max_iter=10000)
  },
  "Hard" = function(X,y,X_u,y_u) {
    EMLeastSquaresClassifier(X,y,X_u, eps = 1e-8, method="block", objective="hard", max_iter=10000)
  },
  "Oracle"=function(X,y,X_u,y_u) {
    LeastSquaresClassifier(rbind(X,X_u),unlist(list(y,y_u)),intercept=TRUE,x_center=TRUE,scale=FALSE) 
  }
)

lc <- LearningCurveSSL(models,datasets,
                       classifiers=classifiers,
                       measures=measures,
                       n_l="enough",repeats=1000,verbose=TRUE,
                       pre_scale = TRUE, pre_pca = TRUE,
                       low_level_cores = detectBatchCPUs(),sizes = 2^(1:10))

save(lc,file="R/learningcurves-enough.RData")
