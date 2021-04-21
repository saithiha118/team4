# team4
stroke
library('cluster') library('dendextend') library('factoextra') library(ggvis) library("ggdendro") library("infotheo") library("dplyr") library("rms") library(tidyverse) library(DataExplorer) library(ggplot2) library(plyr) library(magrittr) library(patchwork) setwd("C:/Users/sheri/Desktop") df<-read.csv("healthcare-dataset-stroke-data.csv") nrow(df)

[1] 5110
df <- na.omit(df) nrow(df)

[1] 5110
EDA / Data Preprocessing

id gender age hypertension heart_disease ever_married work_type
1 9046 Male 67 0 1 Yes Private
2 51676 Female 61 0 0 Yes Self-employed
3 31112 Male 80 0 1 Yes Private
4 60182 Female 49 0 0 Yes Private
5 1665 Female 79 1 0 Yes Self-employed
Residence_type avg_glucose_level bmi smoking_status stroke
1 Urban 228.69 36.6 formerly smoked 1
2 Rural 202.21 N/A never smoked 1
3 Rural 105.92 32.5 never smoked 1
4 Urban 171.23 34.4 smokes 1
5 Rural 174.12 24 never smoked 1
Variable Description:

id: unique identifier
gender: “Male”,“Female” or “Other”
hypertension: 0 = no hypertension, 1 = hypertension is present
heart_disease: 0 = no heart disease, 1 = heart disease is present
ever_married: “No” or “Yes”
work_type: “children”,“govt_job”,“Never_worked”,“Private” or “Self-employed”
Residence_type: “Rural” or “Urban”
avg_glucose_level: average glucose level present in individual sample
bmi: body mass index per individual sample
smoking_status: “formally smoked”,“never smoked”,“smokes” or “unknown”
stroke: 0 = no stroke recorded, 1 = stroke recorded
checking dimensions and data types with visualizations
plot_str(df,fontSize=40) ## we notice that the bmi variable is a factor

snapshot of data
summary(df)

id gender age hypertension
Min. : 67 Length:5110 Min. : 0.08 Min. :0.00000
1st Qu.:17741 Class :character 1st Qu.:25.00 1st Qu.:0.00000
Median :36932 Mode :character Median :45.00 Median :0.00000
Mean :36518 Mean :43.23 Mean :0.09746
3rd Qu.:54682 3rd Qu.:61.00 3rd Qu.:0.00000
Max. :72940 Max. :82.00 Max. :1.00000
heart_disease ever_married work_type Residence_type
Min. :0.00000 Length:5110 Length:5110 Length:5110
1st Qu.:0.00000 Class :character Class :character Class :character
Median :0.00000 Mode :character Mode :character Mode :character
Mean :0.05401
3rd Qu.:0.00000
Max. :1.00000
avg_glucose_level bmi smoking_status stroke
Min. : 55.12 Length:5110 Length:5110 Min. :0.00000
1st Qu.: 77.25 Class :character Class :character 1st Qu.:0.00000
Median : 91.89 Mode :character Mode :character Median :0.00000
Mean :106.15 Mean :0.04873
3rd Qu.:114.09 3rd Qu.:0.00000
Max. :271.74 Max. :1.00000
Predictor variables to investigate:

gender
hypertension
heart_disease
smoking status
age
bmi
avg_glucose_lvl
prior to visualizations, convert columns with binary variables into categorical variables (0=No, 1=Yes)
df$hypertension<-factor(ifelse(df$hypertension==1,"Yes","No")) df$heart_disease<-factor(ifelse(df$heart_disease==1,"Yes","No")) df$stroke<-factor(ifelse(df$stroke==1,"Yes","No"))

plotting stroke unique counts / visualization.
p1<-df%>%ggplot(aes(stroke))+geom_bar(aes(fill=stroke))+ theme_minimal()+ggtitle("Total Count of Stroke Diagnoses")

p2<-df%>%ggplot(aes(stroke))+geom_bar(aes(fill=gender))+ ggtitle("Stroke Counts by Gender")

p1 + p2

Insight 1: We can see that the vast majority of patients did not experience a stroke - statistically, there is quite the class imbalance present. stroke counts by gender - there are far more women recorded than men. However, positive stroke diagnoses seem to be fairly even among genders.

plotting stroke counts with hypertension and heart disease columns
p3<-df%>%ggplot(aes(stroke))+geom_bar(aes(fill=hypertension))+ theme_minimal()+ggtitle("Stroke Counts by Hypertension")

p4<-df%>%ggplot(aes(stroke))+geom_bar(aes(fill=heart_disease))+ ggtitle("Stroke Counts by Heart Disease")

p3 + p4

Insight #2: We can definitively see heart disease and hypertension are prelvant among patience who suffered a stroke.

prior to investigating the bmi and average glucose levels columns, we noticed that there are missing values and potential outliers. The missing values in the bmi column appear to be string values which need to be converted in order to continue.
##the column is to be separated into two different frames. one with NA values and the rest of the numerically converted data. Then, the second frame will include just the missing values.

stroke_numeric<-df%>%filter(bmi!="N/A")%>%mutate(bmi=as.numeric(bmi)) stroke_na<-df%>%filter(bmi=="N/A")%>%mutate(bmi=ifelse(bmi=="N/A",NA,bmi))

Once that is done, they will be bound together in the main data frame for further analysis.
df<-rbind(stroke_na,stroke_numeric)

search for potential NA values w/ visualizations after data and N/A conversions
plot_missing(df)

colSums(is.na(df))

id gender age hypertension
0 0 0 0
heart_disease ever_married work_type Residence_type
0 0 0 0
avg_glucose_level bmi smoking_status stroke
0 201 0 0
compute and replace NA values with the column mean
df$bmi[which(is.na(df$bmi))]<-mean(df$bmi,na.rm=TRUE)

we can note that the missing values in the bmi column have been converted successfully to the column's mean.
#colSums(is.na(df)) plot_missing(df)

colSums(is.na(df))

id gender age hypertension
0 0 0 0
heart_disease ever_married work_type Residence_type
0 0 0 0
avg_glucose_level bmi smoking_status stroke
0 0 0 0
plotting stroke counts with smoking status of the patients.
p5<-df%>%ggplot(aes(stroke))+geom_bar(aes(fill=smoking_status))+ theme_minimal()+ggtitle("Stroke Counts by Smoking Status") p5

Insight #3: We can deduce quite a bit from this plot. The majority of both positive and negatively diagnosed stroke patients are former smokers, then those that have never smoked. Unknown (discuss what to do with this) Outlier Detection p6<-df%>%ggplot(aes(age))+geom_boxplot(aes(fill=stroke))+ theme_minimal()+coord_flip() p7<-df%>%ggplot(aes(avg_glucose_level))+geom_boxplot(aes(fill=stroke))+ theme_minimal()+coord_flip() p8<-df%>%ggplot(aes(bmi))+geom_boxplot(aes(fill=stroke))+ theme_minimal()+coord_flip() p6 + p7 + p8

df <- subset( df, select = c( gender, age, hypertension, heart_disease, Residence_type, avg_glucose_level, bmi, smoking_status, stroke ) ) #Dependent variable transformation #Assumption if Urban resident there is a more chance of having high glucose level related to stroke and hypertension df$Residence_type <- ifelse(df$Residence_type=="Urban", yes = 1, no=0) Data Transformation: Standardize Numeric Fields for age and average glucose level df$age_z <- scale(x=df$age) df$avg_glucose_level_z <- scale(x=df$avg_glucose_level)

Data Transformation: Binning and create our own categorical variable
df$age_binned <- cut(x=df$age, breaks = c(0, 18, 40, 60, 80, 100), right = FALSE, labels = c("Under 18", "25 to 40", "40 to 60", "60 to 80", "over 80")) ggplot(df, aes(age_binned)) + geom_bar(aes(fill=hypertension)) + coord_flip()

Data visualization shows age between 40 and 60 has the highest risk in getting stroke Data Visualization for dependent and independent variables #plot gender, smoking_status , and a graph with both ggplot(df)+geom_bar(aes(x=smoking_status),fill="blue")+ggtitle("Plot of Smoking Status")

ggplot(df)+geom_bar(aes(x=gender),fill="green")+ggtitle("Plot of Gender")

hist(df$avg_glucose_level,breaks = 100,freq = TRUE)

print(summary(df$avg_glucose_level))

Min. 1st Qu. Median Mean 3rd Qu. Max.
55.12 77.25 91.89 106.15 114.09 271.74
paste("std dev:",sd(df$avg_glucose_level))

[1] "std dev: 45.283560150582"
hist(df$age,breaks = 100,freq = TRUE)

print(summary(df$age))

Min. 1st Qu. Median Mean 3rd Qu. Max.
0.08 25.00 45.00 43.23 61.00 82.00
Pattern Evaluation : Data Pattern -age increase average glucose level increase- hypeertension level close to 1 ggplot(df,aes(age, avg_glucose_level, color=hypertension))+geom_point()+geom_smooth(method = 'lm') + scale_x_log10() + scale_y_log10()

geom_smooth() using formula 'y ~ x'
print(summary(df$avg_glucose_level))

Min. 1st Qu. Median Mean 3rd Qu. Max.
55.12 77.25 91.89 106.15 114.09 271.74
print(summary(df$age))

Min. 1st Qu. Median Mean 3rd Qu. Max.
0.08 25.00 45.00 43.23 61.00 82.00
paste("Variance:", var(df$age))

[1] "Variance: 511.331791824335"
paste("std dev:", sd(df$age))

[1] "std dev: 22.6126467231135"
#paste("Variance:", var(df$avg_glucose_level)) #paste("std dev:", sd(df$avg_glucose_level)) #paste("Correlation:", cor(df$age, df$hypertension)) Initial observation suggest that age and glucose level are not strongly correlated enought to run into a colinearity problem.age variable and average glucose varialble are good enough to add ad predictors to predict stroke ggplot(df,aes(age,avg_glucose_level,color=heart_disease))+geom_point()+geom_smooth(method = 'lm')+ scale_x_log10() + scale_y_log10()

geom_smooth() using formula 'y ~ x'
ggplot(df,aes(age,avg_glucose_level,color=stroke))+geom_point()+geom_smooth(method = 'lm') + scale_x_log10() + scale_y_log10()

geom_smooth() using formula 'y ~ x'
Pattern Evaluation: somking status and stroke happen. People never smoked also have high risk of getting smoke ggplot(df, aes(smoking_status)) + geom_bar(aes(fill=stroke)) + coord_flip()

Exploratory Data Analysis : contingency table for smoking status vs. stroke (1 is stroke happens) h.v1 <- table(df$smoking_status, df$stroke)

h.v2 <- addmargins(A=h.v1, FUN = list(total=sum), quiet = TRUE) round(prop.table(h.v1, margin = 2)*100,1)

No Yes
formerly smoked 16.8 28.1
never smoked 37.1 36.1
smokes 15.4 16.9
Unknown 30.8 18.9
Partition the Data set for Classification Model: Logistic Regression set.seed(7) n <- dim(df) [1] train_health_data <- runif(n) < 0.75
health_train <- df[train_health_data, ] health_test <- df[!train_health_data, ] health_train <- subset( health_train, select = c(age, avg_glucose_level, stroke) ) health_test <- subset( health_test, select = c(age, avg_glucose_level, stroke) ) model<- glm(stroke ~ ., family = binomial(link = "logit"), data = health_train, maxit = 100) #print summary model summary(model)

Call:
glm(formula = stroke ~ ., family = binomial(link = "logit"),
data = health_train, maxit = 100)
Deviance Residuals:
Min 1Q Median 3Q Max
-0.9060 -0.3264 -0.1827 -0.0885 3.7397
Coefficients:
Estimate Std. Error z value Pr(>|z|)
(Intercept) -7.369466 0.394618 -18.675 < 2e-16 ***
age 0.069030 0.005580 12.371 < 2e-16 ***
avg_glucose_level 0.004071 0.001341 3.037 0.00239 **
---
Signif. codes: 0 '' 0.001 '' 0.01 '' 0.05 '.' 0.1 ' ' 1
(Dispersion parameter for binomial family taken to be 1)
Null deviance: 1484.7 on 3855 degrees of freedom
Residual deviance: 1213.3 on 3853 degrees of freedom
AIC: 1219.3
Number of Fisher Scoring iterations: 7
We observed that age and average glucose level is significant. What can interpret from these findings as follows; As age increase and glucose level increase and likely to have high risk of stroke We can now observe our stroke_model aginst the test values fitted.results <- predict(model, newdata= health_test, type = 'response') fitted.results <- ifelse(fitted.results > 0.5, 1, 0) difPred <- fitted.results != health_test$stroke misClasError <- mean(fitted.results != health_test$stroke) print(paste("Accuracy", 1 - misClasError))

[1] "Accuracy 0"
The accuracy of our model in predicting stroke is 95% accuracy which indicate that the model performed well. Now we can plot the Receiver Operatin Curve and calculate the area under the cure library(ROCR) p <- predict(model,newdata = health_test, type = 'response') pr <- prediction(p, health_test$stroke) prf <- performance(pr, measure = "tpr", x.measure = "fpr") plot(prf)

auc <- performance(pr, measure = "auc") print(auc@y.values)

[[1]]
[1] 0.8648766
Given that our AUC is closer to 1 than to 0.5 at 85%, #we have a balanced classification model when prediction stroke based on age and glucose level.
