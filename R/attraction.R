# Experiment to plot the optimization paths of the block coordinate descent algorithm on a single dataset

library(RSSL)
library(magrittr)
library(ggplot2)
library(dplyr,warn.conflicts = FALSE)
library(tidyr)

set.seed(4)

## Save paths for different initializations
df_gauss <- generate2ClassGaussian(1000,d=2,var=0.2,expected = FALSE) %>%
  add_missinglabels_mar(Class~.,prob=0.995)
df_test <- generate2ClassGaussian(400,d=2,var=0.2,expected=FALSE)

c_sup <- LeastSquaresClassifier(Class~.,df_gauss)

results <- matrix(NA,100,6)
results_str <- matrix(NA,100,3)
for (i in 1:100) {
  strt <- c_sup@theta+rnorm(3) 
  print(i)
  results_str[i,] <- strt
  c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",
                                     init=strt, method = "block")
  c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",
                                     init=strt, method = "block")
  results[i,1:3] <- c_soft@theta
  results[i,4:6] <- c_hard@theta
}

## Save paths starting with supervised solution
res_sup_soft <- c(EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",
                                           init=c_sup@theta, method = "block")@theta, 
                  c_sup@theta)

res_sup_hard <- c(EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",
                                           init=c_sup@theta, method = "block")@theta, 
                  c_sup@theta)

save(results,results_str,res_sup_soft,res_sup_hard,file="R/attraction.RData")
