## ----setup, include=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=F, warning=F----------------------------
#install.packages("nortest")
library("nortest")
#install.packages("caret")
library("caret")
#install.packages("rstatix")
library("rstatix")
#install.packages("agricolae")
library("agricolae")
#install.packages("dplyr")
library("dplyr")
#install.packages("tidyverse")
library("tidyverse")
#install.packages("lsr")
library("lsr")


## ----message=F, warning=F-----------------------------
claim <-read.csv("train_1.csv")


## -----------------------------------------------------
str(claim)


## -----------------------------------------------------
claim$DateTimeOfAccident<-as.Date(claim$DateTimeOfAccident)
claim$DateReported<-as.Date(claim$DateReported)


## -----------------------------------------------------
claim$Delay <- claim$DateReported-claim$DateTimeOfAccident


## -----------------------------------------------------
claim$Delay <- gsub("days", "",claim$Delay)
claim$Delay <- as.numeric(claim$Delay)
claim$Classifier <- cut(claim$Delay, breaks = c(-100,15,30,89,10000), labels = c("Very Fast","Fast","Slow","Very Slow"))


## -----------------------------------------------------
unique_descriptions <- unique(claim$ClaimDescription)
length(unique_descriptions)

table(claim$Gender)
table(claim$MaritalStatus)


## -----------------------------------------------------
claim$MaritalStatus <- gsub("U",NA,claim$MaritalStatus)
claim$Gender <- gsub("U",NA,claim$Gender)
claim$MaritalStatus[nchar(claim$MaritalStatus)==0] <- NA
claim <- na.omit(claim)
sum(colSums(is.na(claim)))


## -----------------------------------------------------
claim$MHRisk <- claim$ClaimDescription
claim$MHRisk <- gsub(pattern= ".*STRESS.*", 1, claim$MHRisk)
claim$MHRisk <- gsub(".*ANXIETY.*", 1, claim$MHRisk)
claim$MHRisk <- gsub(".*HARASSMENT.*", 1, claim$MHRisk)
claim$MHRisk <- gsub(".*DEPRESSION.*", 1, claim$MHRisk)
claim$MHRisk <- 
gsub("[[:blank:]]","",claim$MHRisk)
claim$MHRisk <- gsub("[a-zA-Z]+", 0, claim$MHRisk)
claim$MHRisk <- gsub("0=0", "0", claim$MHRisk)
claim$MHRisk <- gsub("0ï¿½ï¿½0", "0", claim$MHRisk)

table(claim$MHRisk)


## -----------------------------------------------------
boxplot(claim$UltimateIncurredClaimCost~claim$Gender, main="Final Expendure and Gender", xlab="", ylab="Final Expendure (log)", log="y")

boxplot(claim$UltimateIncurredClaimCost~claim$MaritalStatus, main="Final Expendure and Marital Status", xlab="", ylab="Final Expendure (log)", log="y")

boxplot(claim$UltimateIncurredClaimCost~claim$Classifier, main="Final Expendure and Classifier", xlab="", ylab="Final Expendure (log)", log="y")

boxplot(claim$UltimateIncurredClaimCost~claim$MHRisk, main="Final Expendure and MH Risk", xlab="", ylab="Final Expendure (log)", log="y")


## -----------------------------------------------------
hist(claim$UltimateIncurredClaimCost,main="Histogram")

plot(density(claim$UltimateIncurredClaimCost),main="Density")

qqnorm(claim$UltimateIncurredClaimCost, main="QQ Plot")
qqline(claim$UltimateIncurredClaimCost, main="QQ Plot")


## -----------------------------------------------------
library("nortest")
lillie.test(claim$UltimateIncurredClaimCost)


## -----------------------------------------------------

hist(log(claim$UltimateIncurredClaimCost),main="Histogram")

plot(density(log(claim$UltimateIncurredClaimCost)),main="Density")

qqnorm(log(claim$UltimateIncurredClaimCost), main="QQ Plot")
qqline(log(claim$UltimateIncurredClaimCost), main="QQ Plot")


## -----------------------------------------------------
lillie.test(log(claim$UltimateIncurredClaimCost))


## -----------------------------------------------------
m <- mean(claim$UltimateIncurredClaimCost)
desv <- sd(claim$UltimateIncurredClaimCost)
n <- length(claim$UltimateIncurredClaimCost)
sig <- qt(0.05/2, df=(n-1), lower.tail=FALSE)

lim_inf <- m - sig*(desv/sqrt(n))
lim_sup <- m + sig*(desv/sqrt(n))

lim_inf
lim_sup


## -----------------------------------------------------
comparison_men<-claim$UltimateIncurredClaimCost[claim$Gender=="M"]
comparison_women<-claim$UltimateIncurredClaimCost[claim$Gender=="F"]


## ----test varianzas-----------------------------------
testVariances <- function(x,y){
  var.test(x,y)
}

testVariances(comparison_women,comparison_men)


## -----------------------------------------------------
function_test <- function(a, b){
  mean_a <- mean(a)
  mean_b <- mean(b)
  num_a <- length(a)
  num_b <- length(b)
  var_a <- sd(a)
  var_b <- sd(b)
  den_t <- sqrt((var_a^2/num_a)+(var_b^2/num_b))
  t <- (mean_a-mean_b-1000)/den_t
  gl <- (var_a^2/num_a+var_b^2/num_b)^2/(var_a^4/(num_a^2*(num_a-1))+var_b^4/(num_b^2*(num_b-1)))
  alfa <- (0.05)
  tcritico <- qt( alfa, gl, lower.tail=F )
  p<-pt( t, gl, lower.tail=F )
  return (data.frame("mean_Mujeres"=mean_a, "mean_Hombres"=mean_b, "n_Mujeres"=num_a, "n_Hombres"=num_b, "obs_value"=t, "critical"=tcritico, "pvalue"=p, "freedom_deg"=gl))
}

function_test(comparison_women,comparison_men)


## -----------------------------------------------------
claim_lm<-lm(log(UltimateIncurredClaimCost)~Age+Gender+MaritalStatus+DependentChildren+DependentsOther+WeeklyWages+PartTimeFullTime+HoursWorkedPerWeek+DaysWorkedPerWeek+Classifier+MHRisk+log(InitialIncurredCalimsCost), data=claim)

summary(claim_lm)


## -----------------------------------------------------
residuals <- rstandard(claim_lm)
summary(residuals)

adjustment <- fitted(claim_lm)
plot(adjustment, residuals)


## -----------------------------------------------------
PersonalData = data.frame(Age=24, Gender="F", MaritalStatus="S", DependentChildren=1, DependentsOther=0, WeeklyWages=500, PartTimeFullTime="F", HoursWorkedPerWeek=40, DaysWorkedPerWeek=5, Classifier="Slow", MHRisk="1", InitialIncurredCalimsCost=10000)

prediction_logarithm <- predict(claim_lm,PersonalData)
prediction_mean <- exp(prediction_logarithm+var(claim_lm$residuals)/2)
prediction_median <- exp(prediction_logarithm)

#Logarithm of value
prediction_logarithm
#Expected value
prediction_mean
#Median value
prediction_median



## -----------------------------------------------------
claim$Deficit <- claim$UltimateIncurredClaimCost

claim$Deficit <- as.numeric(claim$Deficit)
claim$InitialIncurredCalimsCost <- as.numeric(claim$InitialIncurredCalimsCost)

for (i in 1:length(claim$Deficit))
{
  if(claim[i,15] > claim[i,14]) {
    claim[i,19] = 1
  } else {
      claim[i,19] = 0
    }
}

claim_glm<-glm(Deficit~Age+Gender+MaritalStatus+DependentChildren+DependentsOther+WeeklyWages+PartTimeFullTime+HoursWorkedPerWeek+Classifier+MHRisk+log(InitialIncurredCalimsCost), family=binomial, data=claim)
summary(claim_glm)


## -----------------------------------------------------
library("caret")
prediction<- ifelse(predict(claim_glm, type="response") > 0.5, "1", "0")
real <- claim$Deficit
confusionMatrix(table( prediction, real),positive="1")


## -----------------------------------------------------
PersonalData_glm = data.frame(Age=20, Gender="M", MaritalStatus="S", DependentChildren=0, DependentsOther=0, WeeklyWages=300, PartTimeFullTime="P", HoursWorkedPerWeek=30, DaysWorkedPerWeek=5, Classifier="Very Slow", MHRisk="0", InitialIncurredCalimsCost=10000)
predict(claim_glm, PersonalData_glm,type="response")


## -----------------------------------------------------
anova_classification <- aov(log(UltimateIncurredClaimCost) ~ Classifier, claim)
anova_1 <- anova(anova_classification)
anova_1


## -----------------------------------------------------
library("rstatix")
eta_squared(anova_classification)


## -----------------------------------------------------
library("agricolae")
HSD.test(anova_classification,"Classifier",console=T)


## -----------------------------------------------------
residuals_anova<-residuals(anova_classification)

qqnorm(residuals_anova)
qqline(residuals_anova)


## -----------------------------------------------------
plot(anova_classification,1)


## -----------------------------------------------------
lillie.test(residuals(anova_classification))


## -----------------------------------------------------
bartlett.test(log(UltimateIncurredClaimCost) ~ Classifier, data= claim)


## -----------------------------------------------------
library("dplyr")

group_by(claim, Gender, MHRisk) -> group
summary <- summarise(group,
    total = length(log(UltimateIncurredClaimCost)),
    mean = mean(log(UltimateIncurredClaimCost)),
    sd = sd(log(UltimateIncurredClaimCost))
  )
summary


## -----------------------------------------------------
library("ggplot2")
ggplot(summary, aes(x=MHRisk, y=mean, group=Gender, color=Gender)) +
geom_point() + geom_line() 


## -----------------------------------------------------
anova_gender_MHRisk<-aov(log(UltimateIncurredClaimCost) ~ Gender*MHRisk,claim)
anova(anova_gender_MHRisk)


## -----------------------------------------------------
anova_gender_MHRisk_sum<-aov(log(UltimateIncurredClaimCost) ~ Gender+MHRisk,claim)
anova(anova_gender_MHRisk_sum)


## ----warning=F----------------------------------------
LSD.test(anova_gender_MHRisk,"MHRisk",group=F,p.adj="bonferroni",console=T)
LSD.test(anova_gender_MHRisk,"Gender",group=F,p.adj="bonferroni",console=T)


## -----------------------------------------------------
plot(anova_gender_MHRisk,1)
plot(anova_gender_MHRisk,2)

