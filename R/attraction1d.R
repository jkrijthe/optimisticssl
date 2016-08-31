# Experiment to generate optimization paths for a 2D dataset

library(RSSL)
library(magrittr)
library(ggplot2)
library(dplyr,warn.conflicts = FALSE)
library(tidyr)

set.seed(4)
df_gauss <- generate2ClassGaussian(n=400,d=1,var = 0.2) %>% 
  add_missinglabels_mar(Class~.,0.99)

library(cowplot)
df_gauss  %>% 
  ggplot(aes(x=X,fill=Class,group=Class),size=0.5,color=NA) +
  geom_dotplot(binpositions="all",method="histodot",
               binwidth=0.1,stackgroups=TRUE) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(legend.position="none")
  

c_sup <- LeastSquaresClassifier(Class~.,df_gauss)

c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",method="block",save_all=TRUE)

c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",method="block",save_all=TRUE)



method <- "block"
trained_soft <- list()
trained_hard <- list()
for (i in 1:100) {
  init <- c_sup@theta+rnorm(2)*1
  trained_soft[[i]] <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="label",method=method,save_all=TRUE,init=init)
  trained_hard[[i]] <- EMLeastSquaresClassifier(Class~.,df_gauss,objective="responsibility",method=method,save_all=TRUE,init=init)
  print(i)
}

classifier2df <- function(trained_classifier) { 
  trained_classifier@intermediate[[2]] %>% 
    unlist %>% 
    matrix(ncol=2,byrow=TRUE) %>% 
    as.data.frame %>% 
    set_colnames(c("Intercept","Slope")) %>% 
    mutate(.,Iteration=1:nrow(.)) 
} 

df_soft <- lapply(trained_soft,classifier2df) %>% bind_rows(.id="Run") 
df_hard <- lapply(trained_hard,classifier2df) %>% bind_rows(.id="Run") 

save(df_soft,df_hard,c_soft,c_hard,file="R/attraction1d.RData")
