# Experiment where we test the effect of the random initializations 
# on the accuracy of the solution of the semi-supervised methods

library(RSSL)
library(magrittr)
library(ggplot2)
library(dplyr,warn.conflicts = FALSE)
library(tidyr)

set.seed(4)
df_gauss <- generate2ClassGaussian(1000,d=2,var=0.2,expected = FALSE) %>%
  add_missinglabels_mar(Class~.,prob=0.995)

df_test <- generate2ClassGaussian(400,d=2,var=0.2,expected=FALSE)

c_sup <- LeastSquaresClassifier(Class~.,df_gauss)

c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss,
                                   objective="label",
                                   method="simple")

c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss,
                                   objective="responsibility",
                                   method="simple")

c_self <- SelfLearning(Class~.,df_gauss,method=LeastSquaresClassifier)

df_sup <- 
  data.frame(Soft=c_soft@opt_res$value,
             Hard=c_hard@opt_res$value,
             Init="Supervised")

df_sup_perf <- 
  data.frame(Soft=mean(predict(c_soft,df_test)==df_test$Class),
             Hard=mean(predict(c_hard,df_test)==df_test$Class),
             Init="Supervised")

df_sup_loss <- 
  data.frame(Soft=mean(loss(c_soft,df_test)),
             Hard=mean(loss(c_hard,df_test)),
             Init="Supervised")

results <- matrix(NA,100,2)
results_perf <- matrix(NA,100,2)
results_loss <- matrix(NA,100,2)
for (i in 1:100) {
  c_soft <- EMLeastSquaresClassifier(Class~.,df_gauss, objective="label",
                                     init="random", method = "simple")
  c_hard <- EMLeastSquaresClassifier(Class~.,df_gauss, objective="responsibility", 
                                     init="random", method="simple")
  results[i,1] <- c_soft@opt_res$value
  results[i,2] <- c_hard@opt_res$value
  results_perf[i,1] <- mean(predict(c_soft,df_test)==df_test$Class)
  results_perf[i,2] <- mean(predict(c_hard,df_test)==df_test$Class)
  results_loss[i,1] <- mean(loss(c_soft,df_test))
  results_loss[i,2] <- mean(loss(c_hard,df_test))
}

df_gauss %>%
  as.tbl %>%
  ggplot() +
  geom_point(aes(x=X1,y=X2),color="black",size=1,
             data=df_gauss %>% filter(is.na(Class))) +
  geom_point(aes(x=X1,y=X2,color=Class,shape=Class),size=6,
             data=df_gauss %>% filter(!is.na(Class)),inherit.aes = FALSE) +
  coord_equal() +
  geom_classifier("Soft"=c_soft,"Hard"=c_hard)

results %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>% 
  rbind(df_sup) %>% 
  gather(Method,Value,-Init) %>% 
  group_by(Method) %>% 
  mutate(Value=Value/min(Value)) %>% 
  ungroup %>%
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Objective/Best Objective Found")

results_perf %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>%
  rbind(df_sup_perf) %>%
  gather(Method,Value,-Init) %>% 
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Accuracy on Test")

results_loss %>% 
  as.data.frame %>% 
  set_colnames(c("Soft","Hard")) %>% 
  mutate(Init="Random") %>% 
  rbind(df_sup_loss) %>% 
  gather(Method,Value,-Init) %>% 
  group_by(Method) %>% 
  #mutate(Value=Value/min(Value)) %>% 
  ungroup %>%
  rename(Initialization=Init) %>% 
  ggplot(aes(x=0,y=Value,color=Initialization)) +
  facet_wrap(~Method,scales = "free") +
  geom_point() +
  ylab("Loss on test")
