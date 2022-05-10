# load packages
library(tidyverse)
library(readxl)
library(rstatix)

# ===study 1===

# read data
study1 <- read_excel("Raw Dataset.xlsx", sheet = "Study 1")

# transformation
study1 <- as.data.frame(study1)
study1 <- study1 %>%
                 filter(Q_R_Familiarity==0)%>%
                 select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
                 mutate_if(is.character, as.numeric)
View(study1) 

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

study1_positive <- study1 %>%
  select(`1a_Positive`, `1b_Positive`,`1c_Positive`,`1d_Positive`,                
         `1e_Positive`, `1_p_confidence`, `Study 1_n_Confidence`,       
         `Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)
  
study1_negative <- study1 %>%
  select(`1a_Negative`, `1b_Negative`,`1c_Negative`,`1d_Negative`,
         `1e_Negative`, `Study 1_n_Confidence`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

colnames(study1_positive) <- c('a','b','c','d','e','confidence','n_confidence','replicate_total','extension_total')
colnames(study1_negative) <- c('a','b','c','d','e','confidence','n_confidence','replicate_total','extension_total')


View(study1_positive)
View(study1_negative)

# data inspection
dim(study1)
str(study1)

dim(study1_positive)
str(study1_positive)
dim(study1_negative)
str(study1_negative)

# model building and plot

# ANOVA
summary(study1)
study1_long <- study1 %>%
  gather(participant,score,`1a_Positive`:`1e_Negative`, factor_key = TRUE)
study1.ANOVA <- study1_long %>%
  mutate(, High_Numeracy = 0)%>%
  select (`participant`,`Replicate_Scale_Total_Score`,`High_Numeracy`,`score`)

colnames(study1.ANOVA) <- c("participant","Low_Numeracy","High_Numeracy","score")
  
res.anova <- anova_test(
  data = study1.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)

# positive linear replicate
lm_positive_replicate = lm( replicate_total ~ a + b + c + d + e, data = study1_positive)
summary(lm_positive_replicate)
coef(lm_positive_replicate)
confint(lm_positive_replicate)
ggplot(data = study1_positive, aes(y=replicate_total, x= a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)
  
# negative linear replicate
lm_negative_replicate = lm( replicate_total ~ a + b + c + d + e, data = study1_negative)
summary(lm_negative_replicate)
coef(lm_negative_replicate)
confint(lm_negative_replicate)
ggplot(data = study1_negative, aes(y=replicate_total, x= a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)


# positive linear extension
lm_positive_extension = lm( extension_total ~ a + b + c + d + e, data = study1_positive)
summary(lm_positive_extension)
coef(lm_positive_extension)
confint(lm_positive_extension)
ggplot(data = study1_positive, aes(y=extension_total, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)

# negative linear extension
lm_negative_extension = lm( extension_total ~ a + b + c + d + e, data = study1_negative)
summary(lm_negative_extension)
coef(lm_negative_extension)
confint(lm_negative_extension)
ggplot(data = study1_negative, aes(y=extension_total, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)

# positive linear confidence
lm_positive_confidence = lm( confidence ~ a + b + c + d + e, data = study1_positive)
summary(lm_positive_confidence)
coef(lm_positive_confidence)
confint(lm_positive_confidence)
ggplot(data = study1_positive, aes(y=confidence, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method = lm)

# negative linear confidence
lm_negative_confidence = lm( confidence ~ a + b + c + d + e, data = study1_negative)
summary(lm_negative_confidence)
coef(lm_negative_confidence)
confint(lm_negative_confidence)
ggplot(data = study1_negative, aes(y=confidence, x = a + b + c + d + e)) + 
  geom_point()+
  geom_smooth(method=lm)


# ===study 2===
# read data
study2 <- read_excel("Raw Dataset1.xlsx", sheet = "Study 2")

# transformation
study2 <- as.data.frame(study2)
study2 <- study2 %>%
  filter(`Study2-F-familiar`==0 | `Study2-P-familiarity`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study2) 

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

# models

# ANOVA
summary(study2)
study2_long <- study2 %>%
  gather(participant,score,`Study2-F-risk`,`Study2-P-risk`, factor_key = TRUE)
study2.ANOVA <- study2_long %>%
  mutate(, High_Numeracy = 0)%>%
  select (`participant`,`Replicate_Scale_Total_Score`,`High_Numeracy`,`score`)

colnames(study2.ANOVA) <- c("participant","Low_Numeracy","High_Numeracy","score")

res.anova <- anova_test(
  data = study2.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)

# frequency linear replicate
lm_frequency_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-F-risk`, data = study2_frequency)
summary(lm_frequency_replicate)
coef(lm_frequency_replicate)
confint(lm_frequency_replicate)
ggplot(data = study2_frequency, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-F-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# percentage linear replicate
lm_percentage_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study2-P-risk`, data = study2_percentage)
summary(lm_percentage_replicate)
coef(lm_percentage_replicate)
confint(lm_percentage_replicate)
ggplot(data = study2_percentage, aes(y=`Replicate_Scale_Total_Score`, x= `Study2-P-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# frequency linear extension
lm_frequency_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-F-risk`, data = study2_frequency)
summary(lm_frequency_extension)
coef(lm_frequency_extension)
confint(lm_frequency_extension)
ggplot(data = study2_frequency, aes(y=`Extension_Scale_Total_Score`, x= `Study2-F-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# percentage linear extension
lm_percentage_extension = lm( `Extension_Scale_Total_Score` ~ `Study2-P-risk`, data = study2_percentage)
summary(lm_percentage_extension)
coef(lm_percentage_extension)
confint(lm_percentage_extension)
ggplot(data = study2_percentage, aes(y=`Extension_Scale_Total_Score`, x= `Study2-P-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# percentage linear confidence
lm_percentage_confidence = lm( `Study2-P-confidence` ~ `Study2-P-risk`, data = study2_percentage)
summary(lm_percentage_confidence)
coef(lm_percentage_confidence)
confint(lm_percentage_confidence)
ggplot(data = study2_percentage, aes(y=`Study2-P-confidence`, x= `Study2-P-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)

# frequency linear confidence
lm_frequency_confidence = lm( `Study2-F-confident` ~`Study2-F-risk`, data = study2_frequency)
summary(lm_frequency_confidence)
coef(lm_frequency_confidence)
confint(lm_frequency_confidence)
ggplot(data = study2_frequency, aes(y=`Study2-F-confident`, x= `Study2-F-risk`)) + 
  geom_point()+
  geom_smooth(method=lm)


# ===study 3===
# read data
study3 <- read_excel("Raw Dataset.xlsx", sheet = "Study 3")

# transformation
study3 <- as.data.frame(study3)
study3 <- study3 %>%
  filter(`Study 3_Familiarity`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study3) 

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

study3_bowl_con <- study3 %>%
  select(`Study 3_Confidence`,Replicate_Scale_Total_Score  ,Extension_Scale_Total_Score)

View(study3_bowl_preference)
View(study3_bowl_type)
View(study3_bowl_clearness)
View(study3_bowl_feeling)
View(study3_bowl_con)

# chi-square
bowl_choice <- study3_bowl_type[1:2]
chisq <- chisq.test(bowl_choice)
chisq
chisq$observed
chisq$expected
round(chisq$residuals,3)
library(corrplot)
corrplot(chisq$residuals, is.corr = F)

# t-test
res <- t.test(`Study3_Bowl-prefer` ~ `Replicate_Scale_Total_Score`, data = study3_bowl_preference, var.equal = TRUE)
res


res <- t.test(`Study3_A_Clear` ~ `Replicate_Scale_Total_Score`, data = study3_bowl_clearness, var.equal = TRUE)
res


res <- t.test(`Study3_A_Feeling` ~ `Replicate_Scale_Total_Score`, data = study3_bowl_feeling, var.equal = TRUE)
res

# correlation
res <- cor.test(study3_bowl_type$`Study 3_Bowl type`, study3_bowl_type$Replicate_Scale_Total_Score, 
                method = "pearson")
res

res <- cor.test(study3_bowl_type$`Study 3_Bowl type`, study3_bowl_type$Extension_Scale_Total_Score, 
                method = "pearson")
res

res <- cor.test(study3_bowl_type$`Study 3_Bowl type`, study3_bowl_type$Extension_Scale_Total_Score, 
                method = "pearson")
res

# LM
# clearness
lm_cleara_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_A_Clear`, data = study3_bowl_clearness)
summary(lm_cleara_replicate)
coef(lm_cleara_replicate)
confint(lm_cleara_replicate)
ggplot(data = study3_bowl_clearness, aes(y=Replicate_Scale_Total_Score, x= `Study 3_A_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

lm_clearb_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_B_Clear`, data = study3_bowl_clearness)
summary(lm_clearb_replicate)
coef(lm_clearb_replicate)
confint(lm_clearb_replicate)
ggplot(data = study3_bowl_clearness, aes(y=Replicate_Scale_Total_Score, x= `Study 3_B_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# feeling
lm_feelinga_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_A_Feeling`, data = study3_bowl_feeling)
summary(lm_feelinga_replicate )
coef(lm_feelinga_replicate )
confint(lm_feelinga_replicate )
ggplot(data = study3_bowl_feeling, aes(y=Replicate_Scale_Total_Score, x= `Study 3_A_Feeling`)) + 
  geom_point()+
  geom_smooth(method=lm)

lm_feelingb_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_B_Clear`, data = study3_bowl_clearness)
summary(lm_clearb_replicate)
coef(lm_clearb_replicate)
confint(lm_clearb_replicate)
ggplot(data = study3_bowl_clearness, aes(y=Replicate_Scale_Total_Score, x= `Study 3_B_Clear`)) + 
  geom_point()+
  geom_smooth(method=lm)

# confidence
lm_confidence_replicate = lm( Replicate_Scale_Total_Score ~ `Study 3_Confidence`, data = study3_bowl_con)
summary(lm_confidence_replicate)
coef(lm_confidence_replicate)
confint(lm_confidence_replicate)
ggplot(data = lm_confidence_replicate, aes(y=Replicate_Scale_Total_Score, x=`Study 3_Confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)

lm_confidence_extension = lm( Replicate_Scale_Total_Score ~ `Study 3_Confidence`, data = study3_bowl_con)
summary(lm_confidence_replicate)
coef(lm_confidence_replicate)
confint(lm_confidence_replicate)
ggplot(data = lm_confidence_replicate, aes(y=Replicate_Scale_Total_Score, x=`Study 3_Confidence`)) + 
  geom_point()+
  geom_smooth(method=lm)

# ===study 4===
# read data
study4 <- read_excel("Raw Dataset.xlsx", sheet = "Study 4")

# transformation
study4 <- as.data.frame(study4)
study4 <- study4 %>%
  filter(`Study4-NL-familiar`==0 | `Study4-L-familiar`==0)%>%
  select(-`familiarity-y-where`,-`familiarity-y-answer`)%>%
  mutate_if(is.character, as.numeric)
View(study4) 

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
study4 <- study4[,17:24]
View(study4)

study4_N <- study4 %>%
  select( `Study4-NL-rate`,`Study4-NL-confident`, `Study4-NL-familiar`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)
  
  
study4_L <- study4%>%
  select(`Study4-L-rate`, `Study4-L-confident`, `Study4-L-familiar`,`Replicate_Scale_Total_Score`, `Extension_Scale_Total_Score`)

View(study4_N)
View(study4_L)

# models

# ANOVA
summary(study4)
study4_long <- study4 %>%
  gather(participant,score,`Study4-NL-rate`,`Study4-L-rate`, factor_key = TRUE)
study4.ANOVA <- study4_long %>%
  mutate(, High_Numeracy = 0)%>%
  select (`participant`,`Replicate_Scale_Total_Score`,`High_Numeracy`,`score`)

colnames(study4.ANOVA) <- c("participant","Low_Numeracy","High_Numeracy","score")

res.anova <- anova_test(
  data = study4.ANOVA, dv=score, wid = participant,
  between = `High_Numeracy`, within=`Low_Numeracy`
)


# N linear replicate
lm_N_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-NL-rate`, data = study4_N)
summary(lm_N_replicate)
coef(lm_N_replicate)
confint(lm_N_replicate)
ggplot(data = study4_N, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-NL-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# L linear replicate
lm_L_replicate = lm( `Replicate_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_L_replicate)
coef(lm_L_replicate)
confint(lm_L_replicate)
ggplot(data = study4_L, aes(y=`Replicate_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# N linear extension
lm_N_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-NL-rate`, data = study4_N)
summary(lm_N_extension)
coef(lm_N_extension)
confint(lm_N_extension)
ggplot(data = study4_N, aes(y=`Extension_Scale_Total_Score`, x= `Study4-NL-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# L linear extension
lm_L_extension = lm( `Extension_Scale_Total_Score` ~ `Study4-L-rate`, data = study4_L)
summary(lm_L_extension)
coef(lm_L_extension)
confint(lm_L_extension)
ggplot(data = study4_L, aes(y=`Extension_Scale_Total_Score`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# N linear confidence
lm_N_confidence = lm( `Study4-NL-confident` ~ `Study4-NL-rate`, data = study4_N)
summary(lm_N_confidence)
coef(lm_N_confidence)
confint(lm_N_confidence)
ggplot(data = study4_N, aes(y=`Study4-NL-confident`, x= `Study4-NL-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

# L linear confidence
lm_L_confidence = lm( `Study4-L-confident` ~ `Study4-L-rate`, data = study4_L)
summary(lm_L_confidence)
coef(lm_L_confidence)
confint(lm_L_confidence)
ggplot(data = study4_L, aes(y=`Study4-L-confident`, x= `Study4-L-rate`)) + 
  geom_point()+
  geom_smooth(method=lm)

