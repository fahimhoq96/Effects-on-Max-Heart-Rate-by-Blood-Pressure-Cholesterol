# Loading Data
df = read.csv('/home/fahim/Documents/ClevelandHeartData.csv', header=T)

# About the data
head(df)
dim(df)
names(df)

# CleanData
library(dplyr)
df2 = df
df2 <- df2 %>%
  mutate(across(everything(), ~gsub("[?]", NA, .x)))
df2 = na.omit(df2)
df2 = df
df2= as.data.frame(gsub("[?]", NA, as.matrix(df)))
df2 = na.omit(df2)
df2 = mutate_all(df2, function(x) as.numeric(as.character(x)))


# Uni variate Plots

# Histograms
library(ggplot2)
ggplot(df, aes(x = age)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  xlab("Age of Patient in years") +
  ylab("Number of Patients") +
  ggtitle("Histogram of Age")

ggplot(df, aes(x = trestbps)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab(" resting blood pressure (in mm Hg on admission to the hospital)") +
  ggtitle("Histogram of Resting Blood Pressure")

ggplot(df, aes(x = chol)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("serum cholestoral in mg/dl") +
  ggtitle("Histogram of Cholesterol")

ggplot(df, aes(x = thalach)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Maximum Heart rate achieved in beats per minute") +
  ggtitle("Histogram of Heart Rate")

ggplot(df, aes(x = oldpeak)) +
  geom_histogram(bins=30, fill = "blue", colour = "black", boundary = 25, alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  ggtitle("Histogram of depression induced by exercise relative to rest")


# Barplots

ggplot(df, aes(x = factor(sex))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("(1 = male; 0 = female)") +
  ggtitle("Barplot of Gender")

ggplot(df, aes(x = factor(cp))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("chest pain type") +
  ggtitle("Barplot of ChestPain")

ggplot(df, aes(x = factor(fbs))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("> 120 mg/dl (1 = true; 0 = false)") +
  ggtitle("Barplot of Fasting Blood Sugar")

ggplot(df, aes(x = factor(restecg))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  ggtitle("Barplot of resting electrocardiographic results")

ggplot(df, aes(x = factor(exang))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("exercise induced angina (1 = yes; 0 = no)") +
  ggtitle("Barplot of exercise induced angina")

ggplot(df, aes(x = factor(slope))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("Value 1:upsloping 2:flat 3:downsloping") +
  ggtitle("Barplot of peak exercise")

ggplot(df, aes(x = factor(ca))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("(0-3) colored by flourosopy (for calcification of vessels)") +
  ggtitle("Barplot of no. of major vessels")

ggplot(df, aes(x = factor(thal))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("(3 = normal; 6 = fixed defect; 7 = reversable defect)") +
  ggtitle("Barplot of nuclear stress test")

ggplot(df, aes(x = factor(num))) +
  geom_bar(fill = "blue", alpha = 0.75) +
  theme_bw() +
  ylab("Number of Patients") +
  xlab("diameter narrowing") +
  ggtitle("Barplot of diagnosis of heart disease")


# Bivariate Plots

# Scatter Plots

ggplot(df, aes(x = age, y = thalach)) +
  geom_point(fill = "blue", shape = 21, size = 0.9, alpha = 0.8) +
  theme_bw() +
  xlab("age of patients in years") +
  ylab("Heart Rate") +
  ggtitle("ScatterPlot of Heart rate by age")

ggplot(df, aes(x = trestbps, y = thalach)) +
  geom_point(fill = "blue", shape = 21, size = 0.9, alpha = 0.8) +
  theme_bw() +
  xlab("resting blood pressure (in mm Hg on admission to the hospital)") +
  ylab("Heart Rate") +
  ggtitle("ScatterPlot of Heart rate by Resting Blood Pressure")

ggplot(df, aes(x = chol, y = thalach)) +
  geom_point(fill = "blue", shape = 21, size = 0.9, alpha = 0.8) +
  theme_bw() +
  xlab("serum cholestoral in mg/dl") +
  ylab("Heart Rate") +
  ggtitle("ScatterPlot of Heart rate by Cholesterol")

ggplot(df, aes(x = oldpeak, y = thalach)) +
  geom_point(fill = "blue", shape = 21, size = 0.9, alpha = 0.8) +
  theme_bw() +
  xlab("depression induced by exercise relative to rest") +
  ylab("Heart Rate") +
  ggtitle("ScatterPlot of Heart rate by Depression")


# Boxplots

ggplot(df, aes(x = as.factor(sex), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("1 = male; 0 = female") +
  ylab("Maximum Heart Rate") +
  ggtitle("Boxplot of HeartRate by Gender")

ggplot(df, aes(x = as.factor(cp), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("chest pain type") +
  ylab("Maximum Heart Rate") +
  ggtitle("Boxplot of HeartRate by ChestPain")

ggplot(df, aes(x = as.factor(fbs), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("> 120 mg/dl (1 = true; 0 = false)") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of Fasting Blood Sugar")

ggplot(df, aes(x = as.factor(restecg), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of resting electrocardiographic results")

ggplot(df, aes(x = as.factor(exang), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("exercise induced angina (1 = yes; 0 = no)") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of exercise induced angina")

ggplot(df, aes(x = as.factor(slope), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("Value 1:upsloping 2:flat 3:downsloping") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of peak exercise")

ggplot(df, aes(x = as.factor(ca), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("(0-3) colored by flourosopy (for calcification of vessels)") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of no. of major vessels")

ggplot(df, aes(x = as.factor(thal), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("(3 = normal; 6 = fixed defect; 7 = reversable defect)") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of nuclear stress test")

ggplot(df, aes(x = as.factor(num), y = thalach)) +
  geom_boxplot(fill = "blue", colour = "black" ,alpha= 0.75) +
  theme_bw() +
  xlab("diameter narrowing") +
  ylab("Maximum Heart Rate") +
  ggtitle("Barplot of diagnosis of heart disease")

# HeatMaps

K = cor(df2)
heatmap(K)

# Tables

library(htmlTable)
library(table1)
df3 = df

df3$sex <- 
  factor(df3$sex, levels=c(0,1),
         labels=c("Female", "Male"))
df3$cp <- 
  factor(df3$cp, levels=c(1,2,3,4),
         labels=c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic "))
df3$fbs <- 
  factor(df3$fbs, levels=c(0,1),
         labels=c("< 120 mg/dl", "> 120 mg/dl"))
df3$restecg <- 
  factor(df3$restecg, levels=c(0,1,2),
         labels=c("normal", "having ST-T wave abnormality", "probable or definite left ventricular hypertrophy"))
df3$exang <- 
  factor(df3$exang, levels=c(0,1),
         labels=c("No", "Yes"))
df3$slope <- 
  factor(df3$slope, levels=c(1,2,3),
         labels=c("upsloping", "flat", "downsloping"))
df3$ca <- 
  factor(df3$ca, levels=c(0,1,2,3),
         labels=c("Major vessels:0", "Major vessels:1", "Major vessels:2","Major vessels:3"))
df3$thal <- 
  factor(df3$thal, levels=c(3,6,7),
         labels=c("normal", "fixed defect","reversable defect"))
df3$num <- 
  factor(df3$num, levels=c(0,1),
         labels=c("< 50% diameter narrowing", "> 50% diameter narrowing"))

label(df3$thalach)        <- "Maximum Heart Rate achieved"
label(df3$age)            <- "Age of Patients"
label(df3$trestbps)       <- "Resting Blood Pressure in mm Hg"
label(df3$chol)           <- "serum Cholesterol in mg/dl"
label(df3$oldpeak)        <- "depression induced by exercise"

label(df3$sex)            <- "Gender"
label(df3$cp)             <- "Chest Pain"
label(df3$fbs)            <- "Fasting Blood Sugar"
label(df3$restecg)        <- "resting electrocardiographic results"
label(df3$exang)          <- "exercise induced angina"
label(df3$slope)          <- "slope of the peak exercise"
label(df3$ca)             <- "number of major vessels"
label(df3$thal)           <- "results of nuclear stress test"
label(df3$num)            <- "angiographic disease status"

caption  <- "Summary of Descriptive Statistics"
footnote <- "Table 1"

table1(~ thalach + age + trestbps + chol + oldpeak + sex + cp + fbs + restecg + exang + slope + ca + thal + num, data = df3, overall=c(left="Total"), caption=caption,footnote = footnote, topclass="Rtable1-zebra")

# Modeling

# 1. Does Blood Pressure effect Heart Rate?
#HeartRate ~ BloodPressure + age + sex + BloodSugar + ChestPain + exercises

a8 = lm(thalach~trestbps + age + sex + fbs + cp + oldpeak + exang + slope, data=df)
summary(a8)

# Diagnostic plots
par(mfrow = c(2,2))
plot(a8)
par(mfrow = c(1,1))

# Multicolinearity
library(car)
vif(model)
#create vector of VIF values
vif_values <- vif(a8)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Model1", horiz = F, col = "steelblue")
#add vertical line at 5
abline(h = 5, lwd = 2, lty = 3)


# Summary of Model in nice table
ifelse1 <- function (test, yes, no){
  if (test) yes
  else no
}

lmCI <- function( model, expcoef=FALSE, robust=FALSE ){
  coef <- summary( model )$coef[,1]
  se <- ifelse1( robust, robust.se.lm(model)[,2], summary( model )$coef[,2] )
  tvalue <- coef / se
  pvalue <- 2*(1-pt(abs(tvalue), model$df.residual))
  if( expcoef ){
    ci95.lo <- exp( coef - qt(.975, model$df.residual) * se )
    ci95.hi <- exp( coef + qt(.975, model$df.residual) * se )
    est <- exp( coef )
  }
  else{
    ci95.lo <- coef - qt(.975, model$df.residual) * se
    ci95.hi <- coef + qt(.975, model$df.residual) * se
    est <- coef
  }
  rslt <- round( cbind( est, ci95.lo, ci95.hi, tvalue, pvalue ), 4 )
  colnames( rslt ) <- ifelse1( 	robust,
                                c("Est", "robust ci95.lo", "robust ci95.hi", "robust t value", "robust Pr(>|t|)"),
                                c("Est", "ci95.lo", "ci95.hi", "t value", "Pr(>|t|)") )
  colnames( rslt )[1] <- ifelse( expcoef, "exp( Est )", "Est" )
  rslt
}

lmCI(model = a8, expcoef=FALSE, robust=FALSE )

library(knitr)
kable(lmCI(model = a8, expcoef=F, robust=F ),
      format = "html",
      booktabs = T,
      caption = "Summary of the 1st model")

# 2 Does Cholesterol effect Heart Rate?
#HeartRate ~ Cholesterol + age + sex + ChestPain +BloodSugar + exercises


c8 = lm(thalach~chol+age + sex + fbs+cp +oldpeak + exang + slope,data=df)
summary(c8)

# Diagnostic plots
par(mfrow = c(2,2))
plot(c8)
par(mfrow = c(1,1))

# Multicolinearity
library(car)
vif(model)
#create vector of VIF values
vif_values <- vif(c8)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Model2", horiz = F, col = "steelblue")
#add vertical line at 5
abline(h = 5, lwd = 2, lty = 3)


# Summary of Model in nice table
lmCI(model = c8, expcoef=FALSE, robust=FALSE )
library(knitr)
kable(lmCI(model = c8, expcoef=F, robust=F ),
      format = "html",
      booktabs = T,
      caption = "Summary of the 2nd model")


# Citations
citation()
citation("dplyr")
citation("ggplot2")
citation("car")
citation("table1")
citation("htmlTable")
citation("knitr")








