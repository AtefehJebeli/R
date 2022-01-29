
# This file contains two parts. D2, D3,D4, and D5

# Install packages:
install.packages("SmartEDA")
install.packages("readxl")
install.packages("corrplot")
install.packages("writexl")
install.packages('dplyr')
install.packages("Hmisc")
install.packages("summarytools")
install.packages("tidyverse")
install.packages("caret")
install.packages("boot")
install.packages("xlsx")
install.packages("readxl")
install.packages("glmnet")
install.packages("openxlsx")
install.packages("gam")
install.packages("splines")
install.packages("leaps")




# Load libraries
library(readxl)
library(summarytools)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(SmartEDA)
library(class)
library(tidyverse)
library(caret)
library(MASS)
library(corrplot)
library(boot)
library(openxlsx)
library (gam)
library (splines)
library(leaps)
library(data.table)
library(glmnet)
library (boot)

# D2
# Part one: Reading the data and pre-processing steps

# One: Read data from Google Drive:

Covid = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRVA4sY5T-Sz6FX_GOV8Mh40NYm6VhgPX7ijYOx7QNIZwWA79DQPiZkZzEFa2UQXA/pub?output=xlsx",sheet=1)
fix(Covid)

# Two: Or read data from your device
# You should set your own directory
# Covid = ("C:\\Users\\mahsa\\Documents\\Project\\Jebeli-IS777\\Covid.xlsx")


# Part two: Knowing data

# Data exploratory analysis

# Look at correlations
cor(Covid$LOS,Covid)

# The number of columns and rows
dim(Covid)

# The number of rows
nrow(Covid)

# The name of columns
names(Covid)

# Checking null values
# There is no null value
NROW(na.omit(Covid))
mean(is.na(Covid))

# Part three: Missing values

# The below lines address missing values in each column. 
# Since in the excel file, all null values are filled by zero. Firstly, we replace zero with NULL. Then apply an imputation technique.
# The imputation technique fills null values with mean, max, and so on.


# Replacing 0 with null in data set's rows

Covid$Lympho[Covid$Lympho == 0] = NA
Covid$WBC[Covid$WBC == 0] = NA
Covid$ALT[Covid$ALT== 0] = NA
Covid$AST[Covid$AST == 0] = NA
Covid$Sodium[Covid$Sodium == 0] = NA
Covid$Creatinine[Covid$Creatinine == 0] = NA
Covid$BUN[Covid$BUN == 0] = NA
Covid$INR[Covid$INR == 0] = NA
Covid$Plts[Covid$Plts == 0] = NA
Covid$Temp[Covid$Temp == 0] = NA
Covid$Temp[Covid$Temp < 0] = NA
Covid$OsSats[Covid$OsSats == 0] = NA
Covid$Age[Covid$Age == 0] = NA
Covid$MAP[Covid$MAP == 0] = NA

# Show the result of replacing zeroes with null.
View(Covid)

# The number of null cells
# There are 2953 null cells.
sum(is.na(Covid))


# Look at the summary of rows before imputation.
summary(Covid)

# The imputation technique fills null values with mean, max, and so on. 

# Imputation:
Covid$Lympho = with(Covid, impute(Lympho, mean))
Covid$WBC = with(Covid, impute(WBC, mean))
Covid$ALT = with(Covid, impute(ALT, mean))
Covid$AST = with(Covid, impute(AST, mean))
Covid$Sodium = with(Covid, impute(Sodium, mean))
Covid$Creatinine = with(Covid, impute(Creatinine, mean))
Covid$BUN = with(Covid, impute(BUN, mean))
Covid$INR = with(Covid, impute(INR, mean))
Covid$Plts = with(Covid, impute(Plts, mean))
Covid$Temp = with(Covid, impute(Temp, mean))
Covid$OsSats = with(Covid, impute(OsSats, mean))
Covid$Age = with(Covid, impute(Age, mean))
Covid$MAP = with(Covid, impute(MAP, mean))

# Save the imputed data set.

# Save imputed data set in your local device:

# You should set your own directory
# write_xlsx(Covid,"C:\\Users\\mahsa\\Documents\\Project\\Jebeli-IS777\\Covid_Imputed.xlsx")
#write_xlsx(Covid,"Covid_Imputed1.xlsx")

# Read the original and imputed data set to compare them.

# 1 . Read data sets from your device:

# You should set your own directory
# Covid_Imputed = read_excel("C:\\Users\\mahsa\\Documents\\Project\\Jebeli-IS777\\Covid_Imputed.xlsx")
# Covid_Original = read_excel("C:\\Users\\mahsa\\Documents\\Project\\Jebeli-IS777\\Covid.xlsx")


# 2. The imputed data set is uploaded on Google Drive.
# So, the bellow code read the original data set and imputed data set from Google drive to compare them.


Covid_Imputed = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFvOWuklh2HTxl0-opXMhm_ZWyi1MCt7aOL_ac3Jdb_JESkhdmFKDjGQpp2_NUtg/pub?output=xlsx",sheet=1)
Covid_Original =read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRVA4sY5T-Sz6FX_GOV8Mh40NYm6VhgPX7ijYOx7QNIZwWA79DQPiZkZzEFa2UQXA/pub?output=xlsx",sheet=1)


# Look at the imputed columns.
View(Covid_Imputed)

# Part four: Summary Statistics

# Compare the summary of each column before and after imputation.

summary(Covid_Imputed$Lympho)
summary(Covid_Original$Lympho)

summary(Covid_Imputed$WBC)
summary(Covid_Original$WBC)

summary(Covid_Imputed$ALT)
summary(Covid_Original$ALT)

summary(Covid_Imputed$AST)
summary(Covid_Original$AST)

summary(Covid_Imputed$Sodium)
summary(Covid_Original$Sodium)

summary(Covid_Imputed$Creatinine)
summary(Covid_Original$Creatinine)

summary(Covid_Imputed$BUN)
summary(Covid_Original$BUN)

summary(Covid_Imputed$INR)
summary(Covid_Original$INR)

summary(Covid_Imputed$Plts)
summary(Covid_Original$Plts)

summary(Covid_Imputed$Temp)
summary(Covid_Original$Temp)

summary(Covid_Imputed$OsSats)
summary(Covid_Original$OsSats)

summary(Covid_Imputed$Age)
summary(Covid_Original$Age)




# Summary statistics of categorical variables

ExpCTable(Covid_Imputed,Target=NULL,clim=5,nlim=15,round=2,bin=NULL,per=F) 


# Summary statistics of all variables
describe(Covid_Imputed)

# Or
# The code below shows the summary of all columns.

summarytools::descr(Covid_Imputed)

# Look at correlations.
cor(Covid_Imputed$LOS,Covid_Imputed)


# Part five: Data visualization


# Bar Charts for Qualitative Variables


#BLACK
ggplot(Covid_Imputed, aes(x=Black,fill= factor(Black))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Doesn't identify as Black","Does identify as Black"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients Identified Race (Black)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#WHITE
ggplot(Covid_Imputed, aes(x=White,fill= factor(White))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Doesn't identify as White","Does identify as White"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients Identified Race (White)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#ASIAN
ggplot(Covid_Imputed, aes(x=Asian,fill= factor(Asian))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Doesn't identify as Asian","Does identify as Asian"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients Identified Race (Asian)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#LATINO
ggplot(Covid_Imputed, aes(x=Latino,fill= factor(Latino))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Doesn't identify as Latino","Does identify as Latino"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients Identified Race (Latino)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#MI
ggplot(Covid_Imputed, aes(x=MI,fill= factor(MI))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Didn't experience MI",
                                "Experienced MI"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patient had a myocardial infraction") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#PVD
ggplot(Covid_Imputed, aes(x=PVD,fill= factor(PVD))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen","orange"),breaks = c("0","1","2"), 
                     labels = c("didn't experience PVD","experienced PVD once","experienced PVD twice"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients experienced congestive heart failure ") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#CHF
ggplot(Covid_Imputed, aes(x=CHF,fill= factor(CHF))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("didn't experience CHF","experienced CHF"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients experienced congestive heart failure ") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#CVD
ggplot(Covid_Imputed, aes(x=CVD,fill= factor(CVD))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("wasn't diagnosed with CVD","was diagnosed with CVD"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with cardiovascular disease ") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#DEMENT
ggplot(Covid_Imputed, aes(x=DEMENT,fill= factor(DEMENT))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with dimentia","diagnosed with dimentia"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with dimentia") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#COPD
ggplot(Covid_Imputed, aes(x=COPD,fill= factor(COPD))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with COPD","diagnosed with COPD"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with chronic obstructive pulmonary disease") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")





#DM Complicated
ggplot(Covid_Imputed, aes(x=DM.Complicated,fill= factor(DM.Complicated))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with DM","diagnosed with DM"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with with diabetes mellitus complicated") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")



#DM SIMPLE
ggplot(Covid_Imputed, aes(x=DM.Simple,fill= factor(DM.Simple))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with DM Simple","diagnosed with DM Simple"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with with diabetes mellitus simple") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#RENAL DISEASE
ggplot(Covid_Imputed, aes(x=Renal.Disease,fill= factor(Renal.Disease))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with renal disease","diagnosed with renal disease"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with renal disease") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#ALL CNS
ggplot(Covid_Imputed, aes(x=All.CNS,fill= factor(All.CNS))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with All CNS","diagnosed with All CNS"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with All CNS") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#PURE CNS
ggplot(Covid_Imputed, aes(x=Pure.CNS,fill= factor(Pure.CNS))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("isn't diagnosed with Pure CNS","diagnosed with Pure CNS"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients diagnosed with Pure CNS") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#STROKE
ggplot(Covid_Imputed, aes(x=Stroke,fill= factor(Stroke))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Have never had a stroke","have had a stroke"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients that experienced stroke") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#SEIZURE 
ggplot(Covid, aes(x=Seizure,fill= factor(Seizure))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("hasn't experienced seizures","experienced seizures"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients eperiencing seizures") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#OLDSYNCOPE
ggplot(Covid_Imputed, aes(x=OldSyncope,fill= factor(OldSyncope))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Not OldSynCope","OldSynCope"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients with OldSynCope") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#OLDOTHERNEURO
ggplot(Covid_Imputed, aes(x=OldOtherNeuro,fill= factor(OldOtherNeuro))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Not Old Other Neuro","Old Other Neuro"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients with OldOtherNeuro") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")


#OTHERBRNLSN
ggplot(Covid_Imputed, aes(x=OtherBrnLsn,fill= factor(OtherBrnLsn))) +
  geom_bar(alpha=0.5,stat="count",width = 0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.6)+
  scale_color_manual(values = c("skyblue","seagreen"),breaks = c("0","1"), 
                     labels = c("Not BRN LSN","BRN LSN"),
                     aesthetics = c("color","fill"))+
  labs(fill = "Patient:")+
  ggtitle("Patients with OldBrnLsn") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), 
        legend.position = "bottom")




# QUANTITATIVE DATA
# Histograms for quantitative variables:


#AGE
ggplot(data = Covid_Imputed, aes(x=Age, color=Age)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Age") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))


#OSSATS
ggplot(data = Covid_Imputed, aes(x=OsSats, color=OsSats)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Oxygen Saturation") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#TEMP
ggplot(data = Covid_Imputed, aes(x=Temp, color=Temp)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Temperature") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#MAP
ggplot(data = Covid_Imputed, aes(x=MAP, color=MAP)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Mean Arterial Pressure (mmHg)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))


#PLTS
ggplot(data = Covid_Imputed, aes(x=Plts, color=Plts)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Platelet Count (k per mm3)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))


#INR
ggplot(data = Covid_Imputed, aes(x=INR, color=INR)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("International Normalized Ratio ") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#BUN
ggplot(data = Covid_Imputed, aes(x=BUN, color=BUN)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Blood Urea Nitrogen (mg/dL)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#CREATININE
ggplot(data = Covid_Imputed, aes(x=Creatinine, color=Creatinine)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Creatinine Levels") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#SODIUM
ggplot(data = Covid_Imputed, aes(x=Sodium, color=Sodium)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Sodium Levels") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#AST
ggplot(data = Covid_Imputed, aes(x=AST, color=AST)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Alanine Aminotransferase (U/liter)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#ALT
ggplot(data = Covid_Imputed, aes(x=ALT, color=ALT)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("ALT Levels") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#WBC
ggplot(data = Covid_Imputed, aes(x=WBC, color=WBC)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("While Blood Cells (per mm3)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))

#LYMPHO
ggplot(data = Covid_Imputed, aes(x=Lympho, color=Lympho)) +
  geom_histogram(position="identity",color="darkblue",fill="lightblue")+
  ggtitle("Lympho levels (pg/ml)") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))



# D3

Covid_Imputed = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFvOWuklh2HTxl0-opXMhm_ZWyi1MCt7aOL_ac3Jdb_JESkhdmFKDjGQpp2_NUtg/pub?output=xlsx",sheet=1)


# Correlation Matrix 

head(Covid)

pairs(~Covid_Imputed$LOS+Covid_Imputed$OsSats+Covid_Imputed$Temp+Covid_Imputed$PVD+Covid_Imputed$Plts+Covid_Imputed$OldSyncope+Covid_Imputed$OldOtherNeuro, data=Covid_Imputed)

C = cor(Covid_Imputed)
head(round(C,2))

corrplot(C, method="color",tl.col = "black",tl.cex = .75)

# REGRESSION

# Steps for finding the stronger predictors among all 33 predictors
cor(Covid_Imputed$LOS,Covid_Imputed)

#Applying the validation set approach to break the data into training and test sets
set.seed (1)
train=sample (4370 ,2185)

# Multivariate Linear Regression using all 33 predictors on training set
lm.fit =lm(LOS ~ .,data=Covid_Imputed ,subset =train)

# Checking multidisciplinary
library (car)
vif(lm.fit)

# Correlation Matrix
cor(Covid_Imputed[c("LOS","Temp", "OsSats", "Plts", "PVD","OldSyncope","OldOtherNeuro")])

# Simple linear regression on each predicting variable
lm.fit =lm(LOS ~ Temp,data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict(lm.fit ,Covid_Imputed))[-train ]^2)
lm.fit =lm(LOS ~ OsSats,data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict(lm.fit ,Covid_Imputed))[-train ]^2)
lm.fit =lm(LOS ~ PVD,data=Covid_Imputed ,subset =train)
summary(lm.fit)
lm.fit =lm(LOS ~ Plts,data=Covid_Imputed ,subset =train)
summary(lm.fit)
lm.fit =lm(LOS ~ OldSyncope,data=Covid_Imputed ,subset =train)
summary(lm.fit)
lm.fit =lm(LOS ~ OldOtherNeuro,data=Covid_Imputed ,subset =train)
summary(lm.fit)


#Multiple Linear Regression
lm.fit =lm(LOS ~ OsSats+Plts+PVD+Temp+OldSyncope+OldOtherNeuro,data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict(lm.fit ,Covid_Imputed))[-train ]^2)
par(mfrow =c(2,2))
plot(lm.fit)

#interaction models
lm.fit =lm(LOS ~ Temp*OsSats,data=Covid_Imputed ,subset =train)
lm.fit =lm(LOS ~ OsSats+Plts+Temp+OldSyncope+(PVD*OldOtherNeuro),data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict (lm.fit ,Covid_Imputed))[-train ]^2)
lm.fit =lm(LOS ~ OsSats+Plts+Temp+(OldSyncope*PVD)+OldOtherNeuro,data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict (lm.fit ,Covid_Imputed))[-train ]^2)
lm.fit =lm(LOS ~ (OsSats*Temp)+Plts+PVD+OldSyncope+OldOtherNeuro,data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict (lm.fit ,Covid_Imputed))[-train ]^2)
lm.fit =lm(LOS ~ (Temp*OsSats)+Plts+OldSyncope+(PVD*OldOtherNeuro),data=Covid_Imputed ,subset =train)
summary(lm.fit)
mean((Covid_Imputed$LOS-predict (lm.fit ,Covid_Imputed))[-train ]^2)


# Non-linear models
lm.fit2=lm(LOS ~ OsSats +I(OsSats^2),data=Covid_Imputed ,subset =train)
summary(lm.fit2)
lm.fit5=lm(LOS ~ poly(OsSats ,5),data=Covid_Imputed ,subset =train)
summary(lm.fit5)
lm.fit3=lm(LOS ~ I(OsSats^3),data=Covid_Imputed ,subset =train)
summary(lm.fit3)
anova(lm.fit, lm.fit3)
lm.fitlog=lm(LOS ~ OsSats+log(OsSats),data=Covid_Imputed ,subset =train)
summary(lm.fitlog)
anova(lm.fit,lm.fitlog)
lm.fit5=lm(LOS ~poly(Temp ,5),data=Covid_Imputed ,subset =train)
summary(lm.fit5)
lm.fit5=lm(LOS ~poly(Plts,5),data=Covid_Imputed ,subset =train)
summary(lm.fit5)
lm.fitlog=lm(LOS ~ Temp+log(Temp),data=Covid_Imputed ,subset =train)
summary(lm.fitlog)
lm.fitlog=lm(LOS ~ Plts+log(Plts),data=Covid_Imputed ,subset =train)
summary(lm.fitlog)
lm.fit=lm(LOS ~ plts,data=Covid_Imputed ,subset =train)

lm.fitlog =lm(LOS ~ OsSats+log(Plts)+PVD+Temp+OldSyncope+OldOtherNeuro,data=Covid_Imputed ,subset =train)
summary(lm.fitlog)

#Comparison of a multiple regression with log transform of Plts and a regular multiple regression
lm.fit =lm(LOS ~ OsSats+Plts+PVD+Temp+OldSyncope+OldOtherNeuro,data=Covid_Imputed ,subset =train)
mean((Covid_Imputed$LOS-predict(lm.fitlog ,Covid_Imputed))[-train ]^2)
mean((Covid_Imputed$LOS-predict(lm.fit ,Covid_Imputed))[-train ]^2)
plot(lm.fit)
plot(lm.fitlog)


# Classification

# Look at data

# Covid=read_excel("C:\\Users\\jebeli\\Downloads\\Covid_imputed.xlsx")
Covid_Imputed = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFvOWuklh2HTxl0-opXMhm_ZWyi1MCt7aOL_ac3Jdb_JESkhdmFKDjGQpp2_NUtg/pub?output=xlsx",sheet=1)
# Adding a categorical variable
Covid_Imputed$CatLOS = cut(Covid$LOS, breaks=c(0,3,7,10,14,20,29,39,60), 
                            labels=c("< 3", "4-7", "8-10", 
                                     "11-14","15-20","21-29","30-39",
                                     "40-60"))
# Looking at the Covid data type
sapply(Covid_Imputed, class)
summary(Covid_Imputed)

# converting to categorical
Covid_Imputed$White = as.factor(Covid_Imputed$White) 
Covid_Imputed$Black = as.factor(Covid_Imputed$Black)
Covid_Imputed$Asian = as.factor(Covid_Imputed$Asian)
Covid_Imputed$Latino = as.factor(Covid_Imputed$Latino)
Covid_Imputed$MI = as.factor(Covid_Imputed$MI)
Covid_Imputed$PVD = as.factor(Covid_Imputed$PVD)
Covid_Imputed$CVD = as.factor(Covid_Imputed$CVD)
Covid_Imputed$CHF = as.factor(Covid_Imputed$CHF)
Covid_Imputed$DEMENT = as.factor(Covid_Imputed$DEMENT)
Covid_Imputed$COPD = as.factor(Covid_Imputed$COPD)
Covid_Imputed$DM.Complicated = as.factor(Covid_Imputed$DM.Complicated)
Covid_Imputed$DM.Simple = as.factor(Covid_Imputed$DM.Simple)
Covid_Imputed$Renal.Disease = as.factor(Covid_Imputed$Renal.Disease)
Covid_Imputed$All.CNS = as.factor(Covid_Imputed$All.CNS)
Covid_Imputed$Pure.CNS = as.factor(Covid_Imputed$Pure.CNS)
Covid_Imputed$Stroke = as.factor(Covid_Imputed$Stroke)
Covid_Imputed$Seizure = as.factor(Covid_Imputed$Seizure)
Covid_Imputed$OldSyncope = as.factor(Covid_Imputed$OldSyncope)
Covid_Imputed$OldOtherNeuro = as.factor(Covid_Imputed$OldOtherNeuro)
Covid_Imputed$OtherBrnLsn = as.factor(Covid_Imputed$OtherBrnLsn)

dim(Covid_Imputed)
names(Covid_Imputed)
Covid_Imputed=Covid_Imputed[,-1]

# Logistic regression classification
glm.fits = glm(CatLOS ~., data = Covid_Imputed, family = binomial)
summary(glm.fits)
summary(glm.fits)$coef

# LDA classification

theme_set(theme_classic())



# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples = Covid_Imputed$CatLOS %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data = Covid_Imputed[training.samples, ]
test.data = Covid_Imputed[-training.samples, ]


# Estimate pre processing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train_transformed <- preproc.param %>% predict(train.data)
test_transformed<- preproc.param %>% predict(test.data)

# Fit the model
model = lda(CatLOS~., data = train_transformed)
model
# plot(model)
# Make predictions
predictions = model %>% predict(test_transformed)
# Make prediction
mean(predictions$class==test_transformed$CatLOS)
names(predictions)
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class membership.
head(predictions$posterior, 6) 
# Linear discriminant
head(predictions$x, 3)

lda.data = cbind(train_transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = CatLOS))

# Model accuracy is: 0.352
mean(predictions$class==test_transformed$CatLOS)

# Change cut off
sum(predictions$posterior[ ,1] >=.5)

# QLDA
# Fit the model
model <- qda(CatLOS~., data = train_transformed)
model
# Make predictions
predictions <- model %>% predict(test_transformed)
# Model accuracy
mean(predictions$class == test_transformed$CatLOS)



#KNN

# Splitting data into train
set.seed(123)
training.samples = Covid_Imputed$CatLOS %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data = Covid_Imputed[training.samples, ]
test.data = Covid_Imputed[-training.samples, ]

# Estimate pre processing parameters

preproc.param = train.data %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train_transform = preproc.param %>% predict(train.data[,-34])
test_transform = preproc.param %>% predict(test.data[,-34])
names(train_transform)

# Fitting KNN Model 
# to training data set
knn.pred= knn(train_transform,
              test_transform,
              train.data$CatLOS,
              k = 5)
knn
mean(test.data$CatLOS != knn.pred)

# Confusion Matrix

#confusion Matrix(table(knn ,test.data$CatLOS)) or 

table(knn.pred,test.data$CatLOS)

test.data$CatLOS

#KNN Accuracy

# Model Evaluation - Choosing K
i=1
k.optm=1
for (i in 1:30){
  knn.mod = knn(train=train_transform, test=test_transform, train.data$CatLOS, k=i)
  k.optm[i] = 100 * sum(test.data$CatLOS == knn.mod)/NROW(test.data$CatLOS)
  k=i
  cat(k,'=',k.optm[i],'')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

mean(test.data$CatLOS != knn.pred)



# D4

Covid_Imputed = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFvOWuklh2HTxl0-opXMhm_ZWyi1MCt7aOL_ac3Jdb_JESkhdmFKDjGQpp2_NUtg/pub?output=xlsx",sheet=1)

#Train by entire data set

#using I function
training_mses = list()
for (i in 1:10) {
  train_mod<-lm(LOS~ I((OsSats+Plts+Temp)^i)+PVD+OldSyncope+OldOtherNeuro,data=Covid_Imputed)
  training_mses[i]<-mean((Covid_Imputed$LOS-predict(train_mod ,Covid_Imputed))^2)
}
training_mses

#Using Poly function
training_mses <- list()
for (i in 1:10) {
  train_mod= lm(LOS ~ poly(OsSats+Temp+Plts, i, raw=T)+PVD+OldSyncope+OldOtherNeuro, data = Covid_Imputed)
  training_mses[i]= mean((Covid_Imputed$LOS-predict(train_mod ,Covid_Imputed))^2)
}
training_mses

# MSE by model degree plot
tibble(
  polynomial = 1:10,
  MSE = unlist(training_mses)
) %>%
  ggplot(aes(x = polynomial, y = MSE)) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = 1:10)


# Applying The Validation Set Approach to break the data into training and test sets
set.seed (1)
train=sample (4370 ,2185)


training_mses <- list()
for (i in 1:10) {
  train_mod<-lm(LOS ~ poly(OsSats+Temp+Plts, i, raw=T)+PVD+OldSyncope+OldOtherNeuro, data = Covid_Imputed,subset =train)
  training_mses[[i]]<-mean((Covid_Imputed$LOS-predict(train_mod ,Covid_Imputed))[-train ]^2)
}

# MSE by model degree plot
tibble(
  polynomial = 1:10,
  MSE = unlist(training_mses)
) %>%
  ggplot(aes(x = polynomial, y = MSE)) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = 1:10)

# Leave-one-out cross validation
library(boot)
cv.error=rep (0,10)
for (i in 1:10){
  glm.fit=glm(LOS ~ poly(OsSats+Temp+Plts, i, raw=T)+PVD+OldSyncope+OldOtherNeuro, data = Covid_Imputed)
  cv.error[i]=cv.glm (Covid_Imputed ,glm.fit)$delta[1]
  }
cv.error

# Leave-one-out CV error by model degree plot 
tibble(
  polynomial = 1:10,
  CV_Error = unlist(cv.error)
) %>%
  ggplot(aes(x = polynomial, y = CV_Error)) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = 1:10)

# 10-fold Cross Validation
set.seed (1)
cv.error.10= rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(LOS ~ poly(OsSats+Temp+Plts, i, raw=T)+PVD+OldSyncope+OldOtherNeuro, data = Covid_Imputed)
  cv.error.10[i]=cv.glm (Covid_Imputed ,glm.fit ,K=10)$delta [1]
  }
cv.error.10

# 10-fold CV error by model degree plot
tibble(
  polynomial = 1:10,
  CV_Error = unlist(cv.error.10)
) %>%
  ggplot(aes(x = polynomial, y = CV_Error)) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = 1:10)


# Bootstrap

boot.fn = function(data, index)
  coef(
    lm(LOS ~ OsSats+Plts+Temp+PVD+OldSyncope+OldOtherNeuro, 
       data = data, subset = index)
  )
set.seed(1)
boot(Covid_Imputed, boot.fn, 4370)
summary(
  lm(LOS ~OsSats+Plts+Temp+PVD+OldSyncope+OldOtherNeuro, data = Covid_Imputed)
)$coef



### D5

# D5-Part A

Covid_Imputed = read.xlsx("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFvOWuklh2HTxl0-opXMhm_ZWyi1MCt7aOL_ac3Jdb_JESkhdmFKDjGQpp2_NUtg/pub?output=xlsx",sheet=1)

names(Covid_Imputed)
Covid_Imputed=na.omit(Covid_Imputed)
Covid_Imputed = data.frame(Covid_Imputed)
attach(Covid_Imputed)
names(Covid_Imputed)

# Forward Subset Selection
# Defining the prediction function

predict.regsubsets = function(object, newdata, id,...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi= coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
 
}

K= 10
set.seed(1)
# Defining folds
folds = sample(rep(1:K, length = nrow(Covid_Imputed)))
table(folds)
# Defining a matrix to store results
cv.errors = matrix(NA, K, 33, dimnames = list(NULL, paste(1:33)))

# For each fold: Fit the model with each subset of predictors on the training part of the fold
# Then, we make prediction on each model size and compute test errors on the appropriate subset and save it in the matrix
# We have 10*33 matrix corresponds to MSE for the ith cross validation fold for the best j variable model

for (j in 1:K) {
  best.fit = regsubsets(LOS ~ .,data = Covid_Imputed[folds != j, ],nvmax = 33,method = "forward")
  for (i in 1:33) {
    pred =predict(best.fit, Covid_Imputed[folds == j, ], id = i)
    cv.errors[j, i] = mean((LOS[folds == j] - pred)^2)
  }
}
# Use apply function to average over columns of the error matrix
mean.cv.errors = apply(cv.errors, 2, mean)

# Looking for the lowest error rate , 
# Selection of 8 variables has the lowest error rate
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
points(8, mean.cv.errors[8], col = "red", cex = 2, pch = 20)

# perform Forward Stepwise Selection to the full data set to obtain the 8-variable model. 

reg.best = regsubsets(LOS ~ ., data = Covid_Imputed, nvmax = 33,method = "forward")
# Parameters
coef(reg.best,8)


# Backward Variable Selection

K = 10
set.seed(1)
folds = sample(rep(1:K, length = nrow(Covid_Imputed)))
cv.errors = matrix(NA, K, 33, dimnames = list(NULL, paste(1:33)))
table(folds)

# For each fold: Fit the model with each subset of predictors on the training part of the fold
# Then, we make prediction on each model size and compute test errors on the appropriate subset and save it in the matrix
#  We have 10*33 matrix corresponds to MSE for the ith cross validation fold for the best j variable model

for (j in 1:K) {
  best.fit = regsubsets(LOS ~ .,data = Covid_Imputed[folds != j, ],nvmax = 33, method = "backward")
  for (i in 1:33) {
    pred = predict(best.fit, Covid_Imputed[folds == j, ], id = i)
    cv.errors[j, i] = mean((LOS[folds == j] - pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
coef(reg.best,10)

# The result shows that selecting 10 variables out of 33 variables has the lowest test error rate
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
points(10, mean.cv.errors[10], col = "red", cex = 2, pch = 20)

# perform Backward Stepwise Selection to the full data set to obtain the 8-variable model. 
reg.best = regsubsets(LOS ~ ., data = Covid_Imputed,nvmax = 33,method = "backward")
coef(reg.best, 10)

# look at the summary
reg.summary = summary(best.fit )
names(reg.summary)
# Look at Cp, BIC, R^2 
reg.summary$rss
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables",ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(20, reg.summary$adjr2[20], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(14, reg.summary$cp[14], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(9, reg.summary$bic[9], col = "red", cex = 2,pch = 20)

# Ridge Regression

names((Covid_Imputed)[, -1])

# Defining matrix of variables and vector of response
x = model.matrix(LOS ~ ., Covid_Imputed)[, -1]
y = LOS

# Defining Lambda  from 10^-2 to 10^10
# set alpha to Zero to fit ridge regression model
grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# Split data set to train/test
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

# Finding best lambda via cross validation
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
# The value of the lambda that result in the smallest cross validation is 3.25
bestlam = cv.out$lambda.min
bestlam

ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:33, ]

### Lasso
# define alpha= 1 to use the Lasso
lasso.mod = glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2)

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients",s = bestlam)[1:33, ]
lasso.coef
# The parameter that are not equal to Zero
lasso.coef[lasso.coef != 0]

# D5-Part B-Moving Beyond Linearity

# Using 7 variables of forward selection PVD,All.CNS, OldSyncope, OsSats, Temp, Plts, Creatinine

# GAMs

cv.error=rep (0,3)
# GAM Linear
gam.linear=glm(LOS ~ OsSats+Temp+Plts+Creatinine+PVD+All.CNS+OldSyncope,data=Covid_Imputed)
summary(gam.linear)
AIC(gam.linear)
BIC(gam.linear)
par(mfrow =c(1,4))
plot.Gam(gam.linear, se=TRUE ,col ="blue ")

# Cross-validation Error results

set.seed (1)
cv.error.10=cv.glm(Covid_Imputed ,gam.linear ,K=10)
cv.error.10$delta[1]
cv.error[1]= cv.error.10$delta[1]


#GAM with Cubic Spline

# The default bs values will create a cubic B-spline basis with two boundary knots and one interior knot placed at the median of the observed data values.
# More flexibility can be achieved by the user, by increasing the placement and the number of knots and/or changing their locations.

gam.cubicspline=glm(LOS ~ bs(OsSats)+bs(Temp)+bs(Plts)+bs(Creatinine)+PVD+All.CNS+OldSyncope,data=Covid_Imputed)
summary(gam.cubicspline)

#A lower AIC and BIC score is better
AIC(gam.cubicspline)
BIC(gam.cubicspline)
par(mfrow =c(1,4))
plot.Gam(gam.cubicspline, se=TRUE ,col ="blue ")

set.seed (1)
cv.error.10=cv.glm(Covid_Imputed ,gam.cubicspline ,K=10)
cv.error.10$delta[1]
cv.error[2]= cv.error.10$delta[1]


# GAM with smooth Splines- controls the roughness by landa - The smaller the landa the more wiggly be the function 
# First find the best degree of freedom for each numeric variable
options(warn = -1)
fit2=smooth.spline (Covid_Imputed$OsSats, Covid_Imputed$LOS ,cv=TRUE)
fit2$df
fit2=smooth.spline (Covid_Imputed$Temp, Covid_Imputed$LOS ,cv=TRUE)
fit2$df
fit2=smooth.spline (Covid_Imputed$Plts, Covid_Imputed$LOS ,cv=TRUE)
fit2$df
fit2=smooth.spline (Covid_Imputed$Creatinine, Covid_Imputed$LOS ,cv=TRUE)
fit2$df

# Fit the model with the best degree of freedom for each numeric variable
gam.smooths=gam(LOS ~ s(OsSats,2)+s(Temp,2)+s(Plts,4)+s(Creatinine,6)+PVD+All.CNS+OldSyncope,data=Covid_Imputed)
summary(gam.smooths)
gam.smooths$coefficients
AIC(gam.smooths)
BIC(gam.smooths)

# Use 10-folds cross-validation
set.seed (1)
cv.error.10=cv.glm(Covid_Imputed ,gam.smooths ,K=10)
cv.error.10$delta[1]
cv.error[3]= cv.error.10$delta[1]

# Plot the results
par(mfrow =c(1,2))
plot.Gam(gam.smooths, se=TRUE ,col ="blue ")

#Comparing linear, cubic, and smooth splines using Anova-Results show better fit in smooth splines
anova(gam.linear ,gam.cubicspline ,gam.smooths,test="F")


#CV error by model plot 
Methods=c("linear","cubic splines","smooth splines")
tibble(
  methods=1:3,
  CV_Error = unlist(cv.error)
) %>%
  ggplot(aes(x = methods, y = CV_Error)) +
  geom_line(aes(group = 1)) +
  scale_x_continuous(breaks = 1:3)+
  scale_x_discrete(limit = c("Linear", "Cubic splines", "Smooth splines"))+
  ggtitle("CV error by model")




