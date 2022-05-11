
features<-as.data.frame(read.csv("features_data.csv"))

View(features)
hamartomas<-subset(features,features$class=="1")
carcinoides<-subset(features,features$class=="2")

library(care)
library(class)
library(tree)

library(tidyverse)
library(rstatix)
library(ggpubr)
mydata<-features[,2:112]
mydata.long <- mydata %>%
  pivot_longer(-class, names_to = "variables", values_to = "value")
mydata.long %>% sample_n(6)


T.test <- mydata.long %>%
  group_by(variables) %>%
  t_test(value ~ class, var.equal = TRUE) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
View(T.test)

welch.T.test <- mydata.long %>%
  group_by(variables) %>%
  t_test(value ~ class, var.equal = FALSE) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
View(welch.T.test)

wilcos.test <- mydata.long %>%
  group_by(variables) %>%
  wilcox_test(value ~ class) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
View(wilcos.test)

library(dplyr)      # for data manipulation functions
library(tidyr)      # for data manipulation functions
library(data.table) # for function `fread`
library(broom)      # for function `tidy`

sw_test_results <- features %>% 
  gather(key = "variable_name", value = "value", diagnostics_Image.original_Mean:original_ngtdm_Strength) %>% 
  group_by(variable_name, class)  %>% 
  do(tidy(shapiro.test(.$value))) %>% 
  ungroup() %>% 
  select(-method)
View(sw_test_results)

library(car)
library(carData)
#normalidad y homocedasticidad de las q no concuerdan los 3 test

shapiro.test(hamartomas$original_firstorder_10Percentile)#si
shapiro.test(carcinoides$original_firstorder_10Percentile)#si
leveneTest(features$original_firstorder_10Percentile,features$class)#si
#signif
shapiro.test(hamartomas$original_glrlm_GrayLevelNonUnifor)#no
shapiro.test(carcinoides$original_glcm_DifferenceEntropy)#si
leveneTest(features$original_glcm_DifferenceEntropy,features$class)#si
#signig
shapiro.test(hamartomas$original_glcm_DifferenceVariance)#si
shapiro.test(carcinoides$original_glcm_DifferenceVariance)#no
leveneTest(features$original_glcm_DifferenceVariance,features$class)#si
#signif
shapiro.test(hamartomas$original_glrlm_RunEntropy)#si
shapiro.test(carcinoides$original_glrlm_RunEntropy)#si
leveneTest(features$original_glrlm_RunEntropy,features$class)#no
#no signif

shapiro.test(hamartomas$original_ngtdm_Contrast)#si
shapiro.test(carcinoides$original_ngtdm_Contrast)#si
leveneTest(features$original_ngtdm_Contrast,features$class)#si
#no signif

shapiro.test(hamartomas$original_glszm_GrayLevelNonUniformityNormalized)#si
shapiro.test(carcinoides$original_glszm_GrayLevelNonUniformityNormalized)#si
leveneTest(features$original_glszm_GrayLevelNonUniformityNormalized,features$class)#no
#no signif

#Hemos sacado 13

#original_firstorder_10Percentile,original_firstorder_InterquartileRange, original_firstorder_MeanAbsoluteDeviation, original_firstorder_Variance,
#original_glcm_ClusterProminence,  original_glcm_ClusterTendency, original_glcm_DifferenceEntropy, original_glcm_DifferenceVariance, 
#original_glcm_SumEntropy, original_glcm_SumSquares, original_gldm_GrayLevelVariance, original_glrlm_GrayLevelVariance, 	
#original_firstorder_RobustMeanAbsoluteDeviation




