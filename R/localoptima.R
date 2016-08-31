# Experiment to test how often hard-label and soft-label self-learning 
# for the least squares classifier get stuck in local optima.

library(methods)
library(RSSL)
library(createdatasets)
library(magrittr)
library(createdatasets)
library(randomForest)
library(dplyr)
library(tidyr)

set.seed(4)

# Change this if you want to store the datasets in a different location
setdatadir("data") 

# Load datasets
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

# Set measures
measures <- list("Error"=measure_error,
                 "Average Loss Test"=measure_losstest)

perfs_hard <- list()
perfs_soft <- list()
resamp <- 10 # Number of times we resample from a dataset
reinit <- 50 # Number of times we reinitialize the self-learner

for (d in names(datasets)) {
  df <- datasets[[d]]
  mf <- models[[d]]
  perf_soft <- matrix(NA,reinit+2,resamp)
  perf_hard <- matrix(NA,reinit+2,resamp)
  
  # Do multiple resamplings from the datasets
  for (j in 1:resamp) {
    
    idx_train <- sample(1:nrow(df),round(0.7*nrow(df)))
    df_train <- df[idx_train,,drop=FALSE]
    df_test <- df[-idx_train,,drop=FALSE]
    df_train <- df_train %>% add_missinglabels_mar(mf, 0.8)
    
    c_sup <- LeastSquaresClassifier(mf,df_train)
    
    c_init_hard <- EMLeastSquaresClassifier(mf,df_train,
                      method="block",init="supervised",objective="hard")
    c_init_soft <- EMLeastSquaresClassifier(mf,df_train,
                      method="block",init="supervised",objective="soft")
    
    perf_hard[1,j] <- mean(predict(c_sup,df_test)==df_test[[all.names(mf)[2]]])
    perf_hard[2,j] <- mean(predict(c_init_hard,df_test)==df_test[[all.names(mf)[2]]])
    
    perf_soft[1,j] <- mean(predict(c_sup,df_test)==df_test[[all.names(mf)[2]]])
    perf_soft[2,j] <- mean(predict(c_init_soft,df_test)==df_test[[all.names(mf)[2]]])
    
    # Do multiple initializations
    for (i in 1:reinit) {
      c_rand_hard <- EMLeastSquaresClassifier(mf,df_train,
                        method="block",init="random",objective="hard")
      c_rand_soft <- EMLeastSquaresClassifier(mf,df_train,
                        method="block",init="random",objective="soft")
      
      perf_soft[i+2,j] <-  mean(predict(c_rand_soft,df_test)==df_test[[all.names(mf)[2]]])
      perf_hard[i+2,j] <-  mean(predict(c_rand_hard,df_test)==df_test[[all.names(mf)[2]]])
    }
    print(j)
  }
  perfs_hard[[d]] <- perf_hard
  perfs_soft[[d]] <- perf_soft
}

# Generate data frames for plotting
df_soft <- 
  lapply(names(perfs_soft),function(x){
    perfs_soft[[x]] %>% 
    as.data.frame %>% 
    mutate(.,Type=c("Supervised","Soft",rep("Random",nrow(.)-2))) %>% 
    mutate(Dataset=x) 
  }) %>% 
  bind_rows %>% 
  mutate(Experiment="Soft")

df_hard <- 
  lapply(names(perfs_hard),function(x){
    perfs_hard[[x]] %>% 
      as.data.frame %>% 
      mutate(.,Type=c("Supervised","Hard",rep("Random",nrow(.)-2))) %>% 
      mutate(Dataset=x) 
  }) %>% 
  bind_rows %>% 
  mutate(Experiment="Hard")

save(df_soft,df_hard,file="R/localoptima.RData")
