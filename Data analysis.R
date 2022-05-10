# get wd for user-kirk only
setwd("D:/R files/Thesis")
getwd()
.libPaths("D:/R files/Packages")
.libPaths()
# install packages
install.packages("readxl")
install.packages("rstatix")
install.packages("ggstatsplot")
install.packages("psychometric")

# load packages
library(tidyverse)
library(readxl)
library(rstatix)
library(performance)
library(corrplot)
library(ggstatsplot)
library(psychometric)

### ======study 1======

# read data from Excel
study1 <- read_excel("Raw Dataset.xlsx", sheet = "Study 1")

# data transformation
study1 <- as.data.frame(study1)
study1 <- study1 %>%
                 filter(Q_R_Familiarity==0)%>%
                 select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
                 mutate_if(is.character, as.numeric)
View(study1) 

# Calculate the total score of replication scale and extension scale in Study 1

study1 <- study1 %>% 
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=500,0))%>%
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==500,1))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=10,0))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==10,1))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=0.1,0))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==0.1,1))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4!=1,0))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4==1,1))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5!=10,0))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5==10,1))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6!=2,0))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6==2,1))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7!=2,0))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7==2,1))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A!=10,0))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A==10,1))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B!=100,0))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B==100,1))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9!=20,0))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9==20,1))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10!=5,0))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10==5,1))

study1 <-
  study1 %>%
  rowwise()%>%
  mutate(Replicate_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,Replicate_Scale_Q3,Replicate_Scale_Q4,
                                            Replicate_Scale_Q6,Replicate_Scale_Q7,Replicate_Scale_Q8A,Replicate_Scale_Q8B,
                                            Replicate_Scale_Q9,Replicate_Scale_Q10))
study1 <- study1 %>% 
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=5,0))%>%
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==5,1))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=47,0))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==47,1))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=9,0))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==9,1))

study1 <-
  study1 %>%
  rowwise()%>%
  mutate(Extension_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,
                                           Replicate_Scale_Q3,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Extension_Scale_Q1,
                                           Extension_Scale_Q2,Extension_Scale_Q3))

View(study1)

## Study 1 Demongraphics
study1_age <- study1 %>%
  select(`age`)

studyage <- summary(study1_age)
boxplot(study1_age) + violinblot



ggplot(aes(x=" ", y = studyage)) +
geom_violin() + 
geom_boxplot()


## Study 1 Data analysis
study1_positive <- study1 %>%
  select(`1a_Positive`, `1b_Positive`,`1c_Positive`,`1d_Positive`,                
         `1e_Positive`, `Study 1_p_Confidence`, `Study 1_n_Confidence`,       
         `Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)
  
study1_negative <- study1 %>%
  select(`1a_Negative`, `1b_Negative`,`1c_Negative`,`1d_Negative`,
         `1e_Negative`, `Study 1_n_Confidence`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

colnames(study1_positive) <- c('a','b','c','d','e','p_confidence','n_confidence','replicate_total','extension_total')
colnames(study1_negative) <- c('a','b','c','d','e','p_confidence','n_confidence','replicate_total','extension_total')


View(study1_positive)
View(study1_negative)

# Stduy 1 data inspection
dim(study1)
str(study1)

dim(study1_positive)
str(study1_positive)
dim(study1_negative)
str(study1_negative)

## Original paper statistical method

# Mixed with-subject ANOVA (High/Low numerate * Positive/Negative * Five students)
summary(study1)
study1_long <- study1 %>%
  gather(participant,score,`1a_Positive`:`1e_Negative`, factor_key = TRUE)

study1.ANOVA <- study1_long %>%
  mutate( `High_Numeracy` = `Replicate_Scale_Total_Score` > 9, Low_Numeracy = `Replicate_Scale_Total_Score` <= 8)%>%
  select (`participant`,`High_Numeracy`,`Low_Numeracy`,`score`)
  
study1.mixedanova <- anova_test(
  data = study1.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)

study1bxp <- ggboxplot(
  stidy1.ANOVA, x="participant", y="score",
  color ="High_Numeracy", palette = "jco"
)
study1bxp

## Replication statistical method

# linear regression: positive condition with replication scale
lm_positive_replicate = lm( replicate_total ~ a + b + c + d + e, data = study1_positive)
summary(lm_positive_replicate)
coef(lm_positive_replicate)
confint(lm_positive_replicate)
ggplot(data = study1_positive, aes(y=replicate_total, x= a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)


CI.Rsq(rsq = 0.0205,
       n = 1000,
       k = 1,
       level = 0.95)

  
# linear regression: negative condition with replication scale
lm_negative_replicate = lm( replicate_total ~ a + b + c + d + e, data = study1_negative)
summary(lm_negative_replicate)
coef(lm_negative_replicate)
confint(lm_negative_replicate)
ggplot(data = study1_negative, aes(y=replicate_total, x= a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)

CI.Rsq(rsq = 0.01994,
    n = 1000,
    k = 1,
    level = 0.95)

# Compare linear regression between positive and negative conditions with replication scale 


# linear regression: positive condition with extension scale
lm_positive_extension = lm( extension_total ~ a + b + c + d + e, data = study1_positive)
summary(lm_positive_extension)
coef(lm_positive_extension)
confint(lm_positive_extension)
ggplot(data = study1_positive, aes(y=extension_total, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: negative condition with extension scale
lm_negative_extension = lm( extension_total ~ a + b + c + d + e, data = study1_negative)
summary(lm_negative_extension)
coef(lm_negative_extension)
confint(lm_negative_extension)
ggplot(data = study1_negative, aes(y=extension_total, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear regression between positive negative conditions with replication scale 


## Study 1 Confidence

# linear regression: confidence and replication scale under positive condition
lm_confidence_positive_replicate = lm( `replicate_total` ~ `p_confidence`, data = study1_positive)
summary(lm_confidence_positive_replicate)
coef(lm_confidence_positive_replicate)
confint(lm_confidence_positive_replicate)
ggplot(data = study1_positive, aes(y= replicate_total, x = p_confidence)) + 
  geom_point()+
  geom_smooth(method = lm)

# linear regression: confidence and replication scale under negative condition
lm_confidence_negative_replicate = lm( `replicate_total`~`n_confidence` , data = study1_negative)
summary(lm_confidence_negative_replicate)
coef(lm_confidence_negative_replicate)
confint(lm_confidence_negative_replicate)
ggplot(data = study1_negative, aes(y= replicate_total, x = n_confidence)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: positive condition confidence with extension scale
lm_confidence_positive_extension = lm( extension_total ~ p_confidence, data = study1_positive)
summary(lm_confidence_positive_extension)
coef(lm_confidence_positive_extension)
confint(lm_confidence_positive_extension)
ggplot(data = study1_positive, aes(y = p_confidence, x = extension_total))+ 
  geom_point()+
  geom_smooth(method = lm)

# linear regression: negative condition confidence with extension scale
lm_confidence_negative_extension  = lm( extension_total ~ n_confidence, data = study1_negative)
summary(lm_confidence_negative_extension)
coef(lm_confidence_negative_extension)
confint(lm_confidence_negative_extension)
ggplot(data = study1_negative, aes(y= n_confidence, x = extension_total)) + 
  geom_point()+
  geom_smooth(method = lm)

### ======study 2======
# read data
study2 <- read_excel("Raw Dataset.xlsx", sheet = "Study 2")

# data transformation
study2 <- as.data.frame(study2)
study2 <- study2 %>%
  filter(`Study2-F-familiar`==0 | `Study2-P-familiarity`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study2) 

# Calculate the total score of replication scale and extension scale in Study 1 
study2 <- study2 %>% 
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=500,0))%>%
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==500,1))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=10,0))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==10,1))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=0.1,0))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==0.1,1))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4!=1,0))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4==1,1))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5!=10,0))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5==10,1))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6!=2,0))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6==2,1))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7!=2,0))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7==2,1))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A!=10,0))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A==10,1))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B!=100,0))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B==100,1))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9!=20,0))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9==20,1))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10!=5,0))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10==5,1))

study2 <-
  study2 %>%
  rowwise()%>%
  mutate(Replicate_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,Replicate_Scale_Q3,Replicate_Scale_Q4,
                                           Replicate_Scale_Q6,Replicate_Scale_Q7,Replicate_Scale_Q8A,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Replicate_Scale_Q10))
study2 <- study2 %>% 
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=5,0))%>%
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==5,1))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=47,0))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==47,1))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=9,0))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==9,1))

study2 <-
  study2 %>%
  rowwise()%>%
  mutate(Extension_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,
                                           Replicate_Scale_Q3,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Extension_Scale_Q1,
                                           Extension_Scale_Q2,Extension_Scale_Q3))

study2_frequency <- study2 %>%
  select(`Replicate_Scale_Q1`,`Replicate_Scale_Q2`, `Replicate_Scale_Q3`, `Replicate_Scale_Q4`,         
         `Replicate_Scale_Q5`,`Replicate_Scale_Q6`, `Replicate_Scale_Q7`, `Replicate_Scale_Q8A`,        
         `Replicate_Scale_Q8B`,`Replicate_Scale_Q9`,`Replicate_Scale_Q10`,`Q_R_Familiarity`,            
         `Extension_Scale_Q1`,`Extension_Scale_Q2`, `Extension_Scale_Q3`, `Q_E_Familiarity`,            
         `Study2-F-risk`,`Study2-F-confident`,`Study2-F-familiar`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)
  
study2_percentage <- study2 %>%
  select(`Replicate_Scale_Q1`,`Replicate_Scale_Q2`, `Replicate_Scale_Q3`, `Replicate_Scale_Q4`,         
                   `Replicate_Scale_Q5`,`Replicate_Scale_Q6`, `Replicate_Scale_Q7`, `Replicate_Scale_Q8A`,        
                   `Replicate_Scale_Q8B`,`Replicate_Scale_Q9`,`Replicate_Scale_Q10`,`Q_R_Familiarity`,            
                   `Extension_Scale_Q1`,`Extension_Scale_Q2`, `Extension_Scale_Q3`, `Q_E_Familiarity`,            
                   `Study2-P-risk`,`Study2-P-confidence`,`Study2-P-familiarity`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

View(study2_frequency)
View(study2_percentage)

## Original paper statistical methods

# Factorial ANOVA (High/Low numeracy * Frequency/Percentage)
summary(study2)
study2_long <- study2 %>%
  gather(participant,score,`Study2-F-risk`,`Study2-P-risk`, factor_key = TRUE)
study2.ANOVA <- study2_long %>%
  mutate( `High_Numeracy` = `Replicate_Scale_Total_Score` > 9, Low_Numeracy = `Replicate_Scale_Total_Score` <= 8)%>%
  select (`participant`,`High_Numeracy`,`Low_Numeracy`,`score`)

colnames(study2.ANOVA) <- c("participant","Low_Numeracy","High_Numeracy","score")

res.anova <- anova_test(
  data = study2.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)

study2bxp <- ggboxplot(
  stidy1.ANOVA, x="participant", y="score",
  color ="High_Numeracy", palette = "jco"
)
study2bxp

## Replication statistical methods

# linear regression: frequency condition with replication scale
lm_frequency_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-F-risk`, data = study2_frequency)
summary(lm_frequency_replicate)
coef(lm_frequency_replicate)
confint(lm_frequency_replicate)
ggplot(data = study2_frequency, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-F-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: percentage condition with replication scale
lm_percentage_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-P-risk`, data = study2_percentage)
summary(lm_percentage_replicate)
coef(lm_percentage_replicate)
confint(lm_percentage_replicate)
ggplot(data = study2_percentage, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-P-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare the linear models: frequency and percentage under replication scale



# linear regression: frequency condition with extension scale
lm_frequency_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-F-risk`, data = study2_frequency)
summary(lm_frequency_extension)
coef(lm_frequency_extension)
confint(lm_frequency_extension)
ggplot(data = study2_frequency, aes(y=`Extension_Scale_Total_Score`, x= `Study2-F-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: percentage condition with extension scale
lm_percentage_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-P-risk`, data = study2_percentage)
summary(lm_percentage_extension)
coef(lm_percentage_extension)
confint(lm_percentage_extension)
ggplot(data = study2_percentage, aes(y=`Extension_Scale_Total_Score`, x= `Study2-P-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)


# Compare the linear models: frequency and percentage under extension scale


## Study 2 Confidence

# linear regression: confidence with replication scale under percentage condition
lm_confidence_percentage_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-P-confidence`, data = study2_percentage)
summary(lm_confidence_percentage_replicate)
coef(lm_confidence_percentage_replicate)
confint(lm_confidence_percentage_replicate)
ggplot(data = study2_percentage, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-P-confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence with replication scale under frequency condition
lm_confidence_frequency_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-F-confident`, data = study2_frequency)
summary(lm_confidence_frequency_replicate)
coef(lm_confidence_frequency_replicate)
confint(lm_confidence_frequency_replicate)
ggplot(data = study2_frequency, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-F-confident`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence with extension scale under percentage condition
lm_confidence_percentage_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-P-confidence`, data = study2_percentage)
summary(lm_confidence_percentage_extension)
coef(lm_confidence_percentage_extension)
confint(lm_confidence_percentage_extension)
ggplot(data = study2_percentage, aes(y=`Extension_Scale_Total_Score`, x= `Study2-P-confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence with extension scale under frequency condition
lm_confidence_frequency_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-F-confidence`, data = study2_frequency)
summary(lm_confidence_frequency_extension)
coef(lm_confidence_frequency_extension)
confint(llm_confidence_frequency_extension)
ggplot(data = study2_frequency, aes(y=`Extension_Scale_Total_Score`, x= `Study2-F-confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)


### ======study 3======
# read data
study3 <- read_excel("Raw Dataset.xlsx", sheet = "Study 3")

# data transformation
study3 <- as.data.frame(study3)
study3 <- study3 %>%
  filter(`Study 3_Familiarity`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study3) 

# Calculate the total score of replication scale and extension scale in Study 1
study3 <- study3 %>% 
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=500,0))%>%
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==500,1))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=10,0))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==10,1))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=0.1,0))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==0.1,1))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4!=1,0))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4==1,1))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5!=10,0))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5==10,1))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6!=2,0))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6==2,1))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7!=2,0))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7==2,1))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A!=10,0))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A==10,1))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B!=100,0))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B==100,1))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9!=20,0))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9==20,1))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10!=5,0))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10==5,1))

study3 <-
  study3 %>%
  rowwise()%>%
  mutate(Replicate_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,Replicate_Scale_Q3,Replicate_Scale_Q4,
                                           Replicate_Scale_Q6,Replicate_Scale_Q7,Replicate_Scale_Q8A,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Replicate_Scale_Q10))
study3 <- study3 %>% 
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=5,0))%>%
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==5,1))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=47,0))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==47,1))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=9,0))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==9,1))

study3 <-
  study3 %>%
  rowwise()%>%
  mutate(Extension_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,
                                           Replicate_Scale_Q3,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Extension_Scale_Q1,
                                           Extension_Scale_Q2,Extension_Scale_Q3))

study3_bowl_preference <- study3%>%
  select(`Study3_Bowl-prefer`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

study3_bowl_type <- study3%>%
    select(`Study 3_Bowl type`, Replicate_Scale_Total_Score  ,Extension_Scale_Total_Score)

study3_bowl_clearness<- study3%>%
  select(`Study 3_A_Clear`, `Study 3_B_Clear`, Replicate_Scale_Total_Score  ,Extension_Scale_Total_Score)

study3_bowl_feeling <- study3%>%
  select(`Study 3_A_Feeling`,`Study 3_B_Feeling` ,Replicate_Scale_Total_Score  ,Extension_Scale_Total_Score)

study3_bowl_confidence <- study3 %>%
  select(`Study 3_Confidence`,Replicate_Scale_Total_Score  ,Extension_Scale_Total_Score)

View(study3_bowl_preference)
View(study3_bowl_type)
View(study3_bowl_clearness)
View(study3_bowl_feeling)
View(study3_bowl_confidence)

## Original paper Statistical methods

# chi-square test (High/Low numeracy * Bowl Choice for A/B)
bowl_choice <- study3_bowl_type%>%
  mutate( `High_Numeracy` = `Replicate_Scale_Total_Score` > 9, Low_Numeracy = `Replicate_Scale_Total_Score` <= 8)%>%
  select (`Study 3_Bowl type`,`High_Numeracy`,`Low Numeracy`)

chisq <- chisq.test(bowl_choice)
chisq


round(chisq$residuals,3)
corrplot(chisq$residuals, is.corr = F)

# Independent t-test: High/Low numeracy (replication scale) * Bowl preference 
bowl_preference <- study3_bowl_preference %>%
  mutate( `High_Numeracy` = `Replicate_Scale_Total_Score` > 9, Low_Numeracy = `Replicate_Scale_Total_Score` <= 8)%>%
  select (`Study3_Bowl-prefer`,`High_Numeracy`,`Low_Numeracy`)

t.test(`Study3_Bowl-prefer` ~ `High_Numeracy`, data = bowl_preference, var.equal = TRUE)
t.test(`Study3_Bowl-prefer` ~ `Low_Numeracy`, data = bowl_preference, var.equal = TRUE)


# Independent t-test: High/Low numeracy (replication scale) * Affect precision(clear)
t.test(`Study3_A_Clear` ~ `High_Numeracy`, data = bowl_preference, var.equal = TRUE)
t.test(`Study3_A_Clear` ~ `Low_Numeracy`, data = bowl_preference, var.equal = TRUE)

# Independent t-test: High/Low numeracy (replication scale) * Affect(feeling)
t.test(`Study3_A_Feeling` ~ `High_Numeracy`, data = bowl_preference, var.equal = TRUE)
t.test(`Study3_A_Feeling` ~ `Low_Numeracy`, data = bowl_preference, var.equal = TRUE)


## Replication Statistical methods 

# Correlation: Bowl choice and replication scale
cor_bowl_replicate <- cor.test(study3_bowl_type$`Study 3_Bowl type`, study3_bowl_type$Replicate_Scale_Total_Score, 
                method = "pearson")
cor_bowl_replicate

# Linear regression: Bowl preference and replication scale
lm__Bowlprefer_replicate = lm( Replicate_Scale_Total_Score ~ `Study3_Bowl-prefer`, data = study3_bowl_preference)
summary(lm__Bowlprefer_replicate)
coef(lm__Bowlprefer_replicate)
confint(lm__Bowlprefer_replicate)
ggplot(data = study3_bowl_preference, aes(y=Replicate_Scale_Total_Score, x= `Study3_Bowl-prefer`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Linear regression: clear for Bowl A and replication scale
lm_clear_BowlA_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_A_Clear`, data = study3_bowl_clearness)
summary(lm_clear_BowlA_replicate)
coef(lm_clear_BowlA_replicate)
confint(lm_clear_BowlA_replicate)
ggplot(data = study3_bowl_clearness, aes(y=Replicate_Scale_Total_Score, x= `Study 3_A_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Linear regression: clear for Bowl B and replication scale
lm_clear_BowlB_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_B_Clear`, data = study3_bowl_clearness)
summary(lm_clear_BowlB_replicate)
coef(lm_clear_BowlB_replicate)
confint(lm_clear_BowlB_replicate)
ggplot(data = study3_bowl_clearness, aes(y=Replicate_Scale_Total_Score, x= `Study 3_B_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: affect precision(clear) for Bowl A&B with replication scale


# Linear regression: feeling for Bowl A and replication scale
lm_feeling_BowlA_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_A_Feeling`, data = study3_bowl_feeling)
summary(lm_feeling_BowlA_replicate )
coef(lm_feeling_BowlA_replicate)
confint(lm_feeling_BowlA_replicate)
ggplot(data = study3_bowl_feeling, aes(y=Replicate_Scale_Total_Score, x= `Study 3_A_Feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Linear regression: feeling for Bowl B and replication scale
lm_feeling_BowlB_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study 3_B_Feeling`, data = study3_bowl_feeling)
summary(lm_feeling_BowlB_replicate)
coef(lm_feeling_BowlB_replicate)
confint(lm_feeling_BowlB_replicate)
ggplot(data = study3_bowl_feeling, aes(y=Replicate_Scale_Total_Score, x= `Study 3_B_Feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: affect(feeling) for Bowl A&B with replication scale



# Correlation: Bowl choice and extension scale
cor_bowl_extension <- cor.test(study3_bowl_type$`Study 3_Bowl type`, study3_bowl_type$Extension_Scale_Total_Score, 
                               method = "pearson")
cor_bowl_extension

# Linear regression: Bowl preference and extension scale
lm__Bowlprefer_extension = lm( Extension_Scale_Total_Score ~ `Study3_Bowl-prefer`, data = study3_bowl_preference)
summary(lm__Bowlprefer_extension)
coef(lm__Bowlprefer_extension)
confint(lm__Bowlprefer_extension)
ggplot(data = study3_bowl_preference, aes(y=Replicate_Scale_Total_Score, x= `Study3_Bowl-prefer`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: clear for Bowl A and extension scale
lm_clear_BowlA_extension = lm( Extension_Scale_Total_Score ~ `Study 3_A_Clear`, data = study3_bowl_clearness)
summary(lm_clear_BowlA_extension)
coef(lm_clear_BowlA_extension)
confint(lm_clear_BowlA_extension)
ggplot(data = study3_bowl_clearness, aes(y=Extension_Scale_Total_Score, x= `Study 3_A_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: clear for Bowl B and extension scale
lm_clearb_extension = lm( Extension_Scale_Total_Score ~ `Study 3_B_Clear`, data = study3_bowl_clearness)
summary(lm_clearb_extension)
coef(lm_clearb_extension)
confint(lm_clearb_extension)
ggplot(data = study3_bowl_clearness, aes(y=Extension_Scale_Total_Score, x= `Study 3_B_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: clear for Bowl A&B with extension scale


# linear regression: feeling for Bowl A and extension scale
lm_feeling_BowlA_extension = lm( Extension_Scale_Total_Score ~ `Study 3_A_Feeling`, data = study3_bowl_feeling)
summary(lm_feeling_BowlA_extension)
coef(lm_feeling_BowlA_extension)
confint(lm_feeling_BowlA_extension)
ggplot(data = study3_bowl_feeling, aes(y=Extension_Scale_Total_Score, x= `Study 3_A_Feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: feeling for Bowl B and extension scale
lm_feeling_BowlB_extension = lm( Extension_Scale_Total_Score ~ `Study 3_B_Feeling`, data = study3_bowl_feeling)
summary(lm_feeling_BowlB_extension)
coef(lm_feeling_BowlB_extension)
confint(lm_feeling_BowlB_extension)
ggplot(data = study3_bowl_feeling, aes(y=Extension_Scale_Total_Score, x= `Study 3_B_Feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: affect(feeling) for Bowl A&B with extension scale


## Study 3 Confidence

# linear regression: confidence and replication scale
lm_confidence_Bowl_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_Confidence`, data = study3_bowl_confidence)
summary(lm_confidence_Bowl_replicate)
coef(lm_confidence_Bowl_replicate)
confint(lm_confidence_Bowl_replicate)
ggplot(data = study3_bowl_confidence, aes(y=Replicate_Scale_Total_Score, x=`Study 3_Confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence and extension scale
lm_confidence_Bowl_extension = lm(`Extension_Scale_Total_Score` ~ `Study 3_Confidence`, data = study3_bowl_confidence)
summary(lm_confidence_Bowl_extension)
coef(lm_confidence_Bowl_extension)
confint(lm_confidence_Bowl_extension)
ggplot(data = study3_bowl_confidence, aes(y=Extension_Scale_Total_Score, x=`Study 3_Confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)


### ======study 4======
# read data
study4 <- read_excel("Raw Dataset.xlsx", sheet = "Study 4")

# data transformation
study4 <- as.data.frame(study4)
study4 <- study4 %>%
  filter(`Study4-NL-familiar`==0 | `Study4-L-familiar`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study4) 

# Calculate the total score of replication scale and extension scale in Study 1
study4 <- study4 %>% 
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=500,0))%>%
  mutate(Replicate_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==500,1))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=10,0))%>%
  mutate(Replicate_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==10,1))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=0.1,0))%>%
  mutate(Replicate_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==0.1,1))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4!=1,0))%>%
  mutate(Replicate_Scale_Q4=replace(Replicate_Scale_Q4,Replicate_Scale_Q4==1,1))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5!=10,0))%>%
  mutate(Replicate_Scale_Q5=replace(Replicate_Scale_Q5,Replicate_Scale_Q5==10,1))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6!=2,0))%>%
  mutate(Replicate_Scale_Q6=replace(Replicate_Scale_Q6,Replicate_Scale_Q6==2,1))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7!=2,0))%>%
  mutate(Replicate_Scale_Q7=replace(Replicate_Scale_Q7,Replicate_Scale_Q7==2,1))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A!=10,0))%>%
  mutate(Replicate_Scale_Q8A=replace(Replicate_Scale_Q8A,Replicate_Scale_Q8A==10,1))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B!=100,0))%>%
  mutate(Replicate_Scale_Q8B=replace(Replicate_Scale_Q8B,Replicate_Scale_Q8B==100,1))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9!=20,0))%>%
  mutate(Replicate_Scale_Q9=replace(Replicate_Scale_Q9,Replicate_Scale_Q9==20,1))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10!=5,0))%>%
  mutate(Replicate_Scale_Q10=replace(Replicate_Scale_Q10,Replicate_Scale_Q10==5,1))

study4 <-
  study4 %>%
  rowwise()%>%
  mutate(Replicate_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,Replicate_Scale_Q3,Replicate_Scale_Q4,
                                           Replicate_Scale_Q6,Replicate_Scale_Q7,Replicate_Scale_Q8A,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Replicate_Scale_Q10))
study4 <- study4 %>% 
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1!=5,0))%>%
  mutate(Extension_Scale_Q1=replace(Replicate_Scale_Q1,Replicate_Scale_Q1==5,1))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2!=47,0))%>%
  mutate(Extension_Scale_Q2=replace(Replicate_Scale_Q2,Replicate_Scale_Q2==47,1))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3!=9,0))%>%
  mutate(Extension_Scale_Q3=replace(Replicate_Scale_Q3,Replicate_Scale_Q3==9,1))

study4 <-
  study4 %>%
  rowwise()%>%
  mutate(Extension_Scale_Total_Score = sum(Replicate_Scale_Q1,Replicate_Scale_Q2,
                                           Replicate_Scale_Q3,Replicate_Scale_Q8B,
                                           Replicate_Scale_Q9,Extension_Scale_Q1,
                                           Extension_Scale_Q2,Extension_Scale_Q3))

View(study4)


study4_NL <- study4 %>%
  select(`Study4-NL-rate`,`Study4-NL-clear`,`Study4-NL-feeling`,`Study4-NL-confident`, `Study4-NL-familiar`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)
  
  
study4_L <- study4%>%
  select(`Study4-L-rate`, `Study4-L-clear`,`Study4-L-feeling`,`Study4-L-confident`, `Study4-L-familiar`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

View(study4_NL)
View(study4_L)
study4 <- study4[,]
View(study4)
## Original paper statistical methods 

# factorial ANOVA:High/Low numeracy(replication scale) * Loss/No loss
summary(study4)
study4_long <- study4 %>%
  gather(participant,score,`Study4-NL-rate`,`Study4-L-rate`, factor_key = TRUE)
study4.ANOVA <- study4_long %>%
  mutate( `High_Numeracy` = `Replicate_Scale_Total_Score` > 9, Low_Numeracy = `Replicate_Scale_Total_Score` <= 8)%>%
  select (`participant`,`Replicate_Scale_Total_Score`,`High_Numeracy`,`score`)

colnames(study4.ANOVA) <- c("participant","Low_Numeracy","High_Numeracy","score")

res.anova <- anova_test(
  data = study4.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)

bxp <- ggboxplot(
  stidy1.ANOVA, x="participant", y="score",
  color ="High_Numeracy", palette = "jco"
)
bxp

## Replication statistical methods

# linear regression: Attractiveness(No loss condition) and replication scale
lm_Attractiveness_NL_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-NL-rate`, data = study4_NL)
summary(lm_Attractiveness_NL_replicate)
coef(lm_Attractiveness_NL_replicate)
confint(lm_Attractiveness_NL_replicate)
ggplot(data = study4_NL, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-NL-rate`)) + 
  geom_point()+

# linear regression: Attractiveness(Loss condition) and replication scale
lm_Attractiveness_L_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_Attractiveness_L_replicate)
coef(lm_Attractiveness_L_replicate)
confint(lm_Attractiveness_L_replicate)
ggplot(data = study4_L, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Attractiveness(No loss and Loss) with replication scale


# linear regression: Clear (No loss condition) and replication scale
lm_clear_NL_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-NL-clear`, data = study4_NL)
summary(lm_clear_NL_replicate)
coef(lm_clear_NL_replicate)
confint(lm_clear_NL_replicate)
ggplot(data = study4_NL, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-NL-clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: Clear (Loss condition) and replication scale
lm_clear_L_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_clear_L_replicate)
coef(lm_clear_L_replicate)
confint(lm_clear_L_replicate)
ggplot(data = study4_L, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Clear(No loss and Loss) with replication scale


# linear regression: Feeling (No loss condition) and replication scale
lm_feeling_NL_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-NL-feeling`, data = study4_NL)
summary(lm_feeling_NL_replicate)
coef(lm_feeling_NL_replicate)
confint(lm_feeling_NL_replicate)
ggplot(data = study4_NL, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-NL-feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: Feeling (Loss condition) and replication scale
lm_feeling_L_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-L-feeling`, data = study4_L)
summary(lm_feeling_L_replicate)
coef(lm_feeling_L_replicate)
confint(lm_feeling_L_replicate)
ggplot(data = study4_L, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-L-feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Feeling (No loss and Loss) with replication scale


# linear regression: Attractiveness(No loss condition) and extension scale
lm_Attractiveness_NL_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-NL-rate`, data = study4_NL)
summary(lm_Attractiveness_NL_extension)
coef(lm_Attractiveness_NL_extension)
confint(lm_Attractiveness_NL_extension)
ggplot(data = study4_NL, aes(y=`Extension_Scale_Total_Score`, x= `Study4-NL-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: Attractiveness(Loss condition) and extension scale
lm_Attractiveness_L_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_Attractiveness_L_extension)
coef(lm_Attractiveness_L_extension)
confint(lm_Attractiveness_L_extension)
ggplot(data = study4_L, aes(y=`Extension_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Attractiveness(No loss and Loss) with extension scale




# linear regression: Clear (No loss condition) and extension scale
lm_clear_NL_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-NL-clear`, data = study4_NL)
summary(lm_clear_NL_extension)
coef(lm_clear_NL_extension)
confint(lm_clear_NL_extension)
ggplot(data = study4_NL, aes(y=`Extension_Scale_Total_Score`, x= `Study4-NL-clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: Clear (Loss condition) and extension scale
lm_clear_L_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_clear_L_extension)
coef(lm_clear_L_extension)
confint(lm_clear_L_extension)
ggplot(data = study4_L, aes(y=`Extension_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Clear(No loss and Loss) with extension scale


# linear regression: Feeling (No loss condition) and extension scale
lm_feeling_NL_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-NL-feeling`, data = study4_NL)
summary(lm_feeling_NL_extension)
coef(lm_feeling_NL_extension)
confint(lm_feeling_NL_extension)
ggplot(data = study4_NL, aes(y=`Extension_Scale_Total_Score`, x= `Study4-NL-feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: Feeling (Loss condition) and extension scale
lm_feeling_L_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-L-feeling`, data = study4_L)
summary(lm_feeling_L_extension)
coef(lm_feeling_L_extension)
confint(lm_feeling_L_extension)
ggplot(data = study4_L, aes(y=`Extension_Scale_Total_Score`, x= `Study4-L-feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

# Compare linear models: Feeling (No loss and Loss) with extension scale


## Study 4 Confidence

# linear regression: confidence No loss condition with replication scale
lm_confidence_NL_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-NL-confident`, data = study4_N)
summary(lm_confidence_NL_replicate )
coef(lm_confidence_NL_replicate )
confint(lm_confidence_NL_replicate )
ggplot(data = study4_NL, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-NL-confident`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence loss condition with replication scale
lm_confidence_L_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-L-confident`, data = study4_L)
summary(lm_confidence_L_replicate)
coef(lm_confidence_L_replicate)
confint(lm_confidence_L_replicate)
ggplot(data = study4_L, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-L-confident`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence No loss condition with extension scale
lm_confidence_NL_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-NL-confident`, data = study4_NL)
summary(lm_confidence_NL_extension)
coef(lm_confidence_NL_extension)
confint(lm_confidence_NL_extension)
ggplot(data = study4_NL, aes(y=`Extension_Scale_Total_Score`, x= `Study4-NL-confident`)) + 
  geom_point()+
  geom_smooth(method=lm)

# linear regression: confidence Loss condition with extension scale
lm_confidence_L_extension  = lm( `Extension_Scale_Total_Score` ~ `Study4-L-confident`, data = study4_L)
summary(lm_confidence_L_extension)
coef(lm_confidence_L_extension)
confint(lm_confidence_L_extension)
ggplot(data = study4_L, aes(y=`Extension_Scale_Total_Score`, x= `Study4-L-confident`)) + 
  geom_point()+
  geom_smooth(method=lm)
