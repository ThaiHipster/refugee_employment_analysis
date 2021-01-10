##########################################
# title: "Refugee and Imigrant Employment Analysis"
# author: "Robert B. Alward"
# date: "October 25, 2020"
###########################################

# Load libraries ---------------------------

library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(MASS)
library(car)

#TODO: Insert Summary of Project


# Load data ---------------------------
Refugee_Employ <- read_excel("1.1Refugee_Employment.xlsx")
Labor_Force <- read_excel ("2.Labor_Force_Participation.xlsx")
Gov_Types <- read_excel("3.Government_Types.xlsx")
Democracy_Rate <- read_excel("4.Democracy_Rating.xlsx",sheet=2)
Amnesty_Data <- read_excel ("5.Amnesty_International_Data.xlsx")
Refugee_Num <- read_excel ("7.UNHCR_Refugee_Numbers.xlsx")
GDP_Capita <- read_excel ("8.GDP_per_Capita.xlsx")
Imigrant_Employ <- read_excel ("9.Imigration_Employment.xlsx")
PEW_17 <- read_excel ("10.1PEW_Opinion_17g.xlsx")
PEW_43 <- read_excel("10.2PEW_Opinion_43.xlsx")
Refugee_Democracy <- read_excel("4.Democracy_Rating.xlsx",sheet=3)
Imigrant_Democracy <- read_excel("4.Democracy_Rating.xlsx",sheet=4)

# General Data Cleaning ---------------------------

# Government Type Data
Gov_Types_C <- na.omit(Gov_Types)
names(Gov_Types_C) <- as.matrix(Gov_Types_C[1,])
Gov_Types_C <- Gov_Types_C [-1,] 

# Refugee Employment Data
Refugee_Employ_C <- na.omit(Refugee_Employ)
names(Refugee_Employ_C) <- c("Country Name","Refugee Employment Rate")
Refugee_Employ_C <- Refugee_Employ_C [-1,]
Refugee_Employ_C <- Refugee_Employ_C[,-3:-4]
Refugee_Employ_C$`Refugee Employment Rate` <- as.numeric(Refugee_Employ_C$`Refugee Employment Rate`)
Refugee_Employ_C$`Refugee Employment Rate` <- Refugee_Employ_C$`Refugee Employment Rate`*100

#Democracy Rate Data
Democracy_Rate_C <- Democracy_Rate
Democracy_Rate_C$`Country Name 2` <- substring(Democracy_Rate_C$`Country Name`, 2)
Democracy_Rate_C[1,10]<-"Nepal"
Democracy_Rate_C[2,10]<-"Switzerland"
Democracy_Rate_C$`Country Name` <- Democracy_Rate_C$`Country Name 2`
Democracy_Rate_C <- Democracy_Rate_C[,-10]
names(Democracy_Rate_C)[2] <- "Democracy Score"

Refugee_Democracy_C <- Refugee_Democracy[,1:2]
names(Refugee_Democracy_C)[2]<-"Democracy Rating"
Refugee_Democracy_C$`Democracy Rating`<- as.numeric(Refugee_Democracy_C$`Democracy Rating`)

Imigrant_Democracy_C <- Imigrant_Democracy[,1:2]
names(Imigrant_Democracy_C)[2]<-"Democracy Rating"
Imigrant_Democracy_C$`Democracy Rating`<- as.numeric(Imigrant_Democracy_C$`Democracy Rating`)

#Amnesty International Data: Maybe Delete
Amnesty_Data_Q <- as.matrix(Amnesty_Data[1,])
Amnesty_Data_C <- Amnesty_Data 
names(Amnesty_Data_C) <- as.matrix(Amnesty_Data[1,])
Amnesty_Data_C <- Amnesty_Data_C[-1,]
Amnesty_Data_C <- Amnesty_Data_C[,-3]
Amnesty_Data_C <- Amnesty_Data_C[,-3]
Amnesty_Data_C <- Amnesty_Data_C[,-8]
Amnesty_Data_C <- Amnesty_Data_C[,-13]

#Number of Refugees Data
Refugee_Num_C <- Refugee_Num[-1:-13,]
names(Refugee_Num_C) <- as.matrix(Refugee_Num_C[1,])
Refugee_Num_C <- Refugee_Num_C[-1,]
Refugee_Num_C <- Refugee_Num_C[,-2:-3]

Refugee_Num_2014 <- filter(Refugee_Num_C, Year == 2014)
Refugee_Num_2015 <- filter(Refugee_Num_C, Year == 2015)
Refugee_Num_2016 <- filter(Refugee_Num_C, Year == 2016)
names(Refugee_Num_2016)[2] <- "Country Name"
Refugee_Num_2017 <- filter(Refugee_Num_C, Year == 2017)
Refugee_Num_2018 <- filter(Refugee_Num_C, Year == 2018)
Refugee_Num_2019 <- filter(Refugee_Num_C, Year == 2019)

#Labor Force Participation Data
Labor_Force_C <- Labor_Force [,-3:-4]

Labor_Force_2012_Var <- c("Country Name","Country Code","2012 [YR2012]")
Labor_Force_2012 <- Labor_Force_C[Labor_Force_2012_Var]

Labor_Force_2013_Var <- c("Country Name","Country Code","2013 [YR2013]")
Labor_Force_2013 <- Labor_Force_C[Labor_Force_2013_Var]

Labor_Force_2014_Var <- c("Country Name","Country Code","2014 [YR2014]")
Labor_Force_2014 <- Labor_Force_C[Labor_Force_2014_Var]

Labor_Force_2015_Var <- c("Country Name","Country Code","2015 [YR2015]")
Labor_Force_2015 <- Labor_Force_C[Labor_Force_2015_Var]

Labor_Force_2016_Var <- c("Country Name","Country Code","2016 [YR2016]")
Labor_Force_2016 <- Labor_Force_C[Labor_Force_2016_Var]
Labor_Force_2016$`2016 [YR2016]` <- as.numeric(Labor_Force_2016$`2016 [YR2016]`)

#GDP Per Capita Data
X <- split(GDP_Capita,GDP_Capita$`Series Name`)
str(X)

GDP_Capita_C <- X$'GDP per capita (current US$)'
GDP_Capita_C <- GDP_Capita_C[,-3]
GDP_Capita_2016_Var <- c("Country Name","Country Code","2016 [YR2016]")
GDP_Capita_2016 <- GDP_Capita_C[GDP_Capita_2016_Var]

GDP_Capita_2017_Var <- c("Country Name","Country Code","2017 [YR2017]")
GDP_Capita_2017 <- GDP_Capita_C[GDP_Capita_2017_Var]

GDP_Capita_2018_Var <- c("Country Name","Country Code","2018 [YR2018]")
GDP_Capita_2018 <- GDP_Capita_C[GDP_Capita_2018_Var]

GDP_Capita_2019_Var <- c("Country Name","Country Code","2019 [YR2019]")
GDP_Capita_2019 <- GDP_Capita_C[GDP_Capita_2019_Var]


#GDP Growth Data
GDP_Growth_C <- X$'GDP per capita growth (annual %)'
GDP_Growth_C <- GDP_Growth_C[,-3:-4]
GDP_Gowth_C <- GDP_Growth_C[,-7]

GDP_Growth_2016_Var <- c("Country Name","Country Code","2016 [YR2016]")
GDP_Growth_2016 <- GDP_Growth_C[GDP_Growth_2016_Var]

GDP_Growth_2017_Var <- c("Country Name","Country Code","2017 [YR2017]")
GDP_Growth_2017 <- GDP_Growth_C[GDP_Growth_2017_Var]

GDP_Growth_2018_Var <- c("Country Name","Country Code","2018 [YR2018]")
GDP_Growth_2018 <- GDP_Growth_C[GDP_Growth_2018_Var]

GDP_Growth_2018_Var <- c("Country Name","Country Code","2018 [YR2018]")
GDP_Growth_2018 <- GDP_Growth_C[GDP_Growth_2018_Var]

#Immigrant Employment Data
Imigrant_Employ_C <- Imigrant_Employ [,-2:-5]
Imigrant_Employ_C <- Imigrant_Employ_C [,-4]
names (Imigrant_Employ_C)[1] <- "Country Code"

Imigrant_Employ_2015 <- subset(Imigrant_Employ_C, TIME == 2015, select = c("Country Code","TIME","Value"))
Imigrant_Employ_2016 <- subset(Imigrant_Employ_C, TIME == 2016, select = c("Country Code","TIME","Value"))
Imigrant_Employ_2017 <- subset(Imigrant_Employ_C, TIME == 2017, select = c("Country Code","TIME","Value"))
Imigrant_Employ_2018 <- subset(Imigrant_Employ_C, TIME == 2018, select = c("Country Code","TIME","Value"))
Imigrant_Employ_2019 <- subset(Imigrant_Employ_C, TIME == 2019, select = c("Country Code","TIME","Value"))


#PEW Data: Maybe Delete
PEW_17_Prompt <- PEW_17[0,2]
names(PEW_17) <- as.matrix(PEW_17[1,])
PEW_17 <- PEW_17[-1,]
PEW_17 <- remove_empty (PEW_17,which = c("cols"),quiet=TRUE)
names(PEW_17)[1] <- "Country Name"
PEW_17_C <- PEW_17

PEW_43_Prompt <- PEW_43[0,2]
PEW_43[1,2] <- "Country Name"
PEW_43_C <- PEW_43
PEW_43_C[1,2] <- "A better place to live"
names(PEW_43_C) <- as.matrix(PEW_43_C[1,])
PEW_43_C <- PEW_43_C[-1,]
names(PEW_43_C)[1] <- "Country Name"

# Combined Data: Difference of Labor Force Participation 2016 & Refugee Employment Data
Labor_Force_Refugee_Employ <- merge(x = Labor_Force_2016, y = Refugee_Employ_C, 
                                    by = "Country Name")

Labor_Force_Refugee_Employ <- Labor_Force_Refugee_Employ[,-2]
names(Labor_Force_Refugee_Employ)[2]<- "2016 Labor Force Participation Rate"

Labor_Force_Refugee_Employ$Difference <- (Labor_Force_Refugee_Employ$`2016 Labor Force Participation Rate`
                                          - Labor_Force_Refugee_Employ$`Refugee Employment Rate`)


#Combined Data: Difference of Labor Force Participation 2016 & Immigrant Employment 2016 
Labor_Force_Imigrant_Employ <- merge(x = Labor_Force_2016, y = Imigrant_Employ_2016, by = "Country Code")
Labor_Force_Imigrant_Employ <- Labor_Force_Imigrant_Employ[,c(-1,-4)]
names(Labor_Force_Imigrant_Employ)[2:3]<- c("2016 Labor Force Participation Rate","2016 Imigrant Employment Rate")

Labor_Force_Imigrant_Employ$Difference <- (Labor_Force_Imigrant_Employ$`2016 Labor Force Participation Rate`
                                          - Labor_Force_Imigrant_Employ$`2016 Imigrant Employment Rate`)

# Analysis 1 Data Cleaning -----------------------------------

# Variable Description
# Y = Refugee Employment
# X = Labor_Force, Democracy, Refugee_Num, GDP_per_Capita, GDP_Growth, *potential (PEW_Survey)

# Analysis 1 Merging GDP Data
GDP_Merge <- merge(x=GDP_Capita_2016, y = GDP_Growth_2016, by = "Country Code")
GDP_Merge <- GDP_Merge[,-4]
names(GDP_Merge) <- c("Country Code", "Country Name", "2016 GDP Per Capita","2016 GDP Growth")

GDP_Refugee_Merge <- merge(x = GDP_Merge, y=Refugee_Employ_C, by = "Country Name")

GDP_Practice <- Reduce(function(x,y) merge(x = x, y = y, by = "Country Name"), 
                       list(GDP_Capita_2016, GDP_Growth_2016, Refugee_Employ_C))

# Analysis 1 Merging Analysis Data

Analysis_Data_1.1 <- GDP_Refugee_Merge

Analysis_Data_1.1 <- merge(x=Labor_Force_2016, y=GDP_Refugee_Merge, by = "Country Name")

names(Analysis_Data_1.1)[3]<-"2016 Labor Force Participation Rate"

Analysis_Data_1.2 <- Reduce(function(x,y) merge(x = x, y = y, by = "Country Name"), 
                            list(Analysis_Data_1.1, Labor_Force_2016, Refugee_Num_2016))

Analysis_Data_1.3 <- Analysis_Data_1.2[,-13:-17]
Analysis_Data_1.3 <- Analysis_Data_1.3[,-11]
Analysis_Data_1.3 <- Analysis_Data_1.3[,-10]
Analysis_Data_1.3 <- Analysis_Data_1.3[,-8]
Analysis_Data_1.3 <- Analysis_Data_1.3[,-4]
Analysis_Data_1.3 <- Analysis_Data_1.3[,-7]

names(Analysis_Data_1.3)[7]<-"Refugees under UNHCR's mandate"

Analysis_Data_1.3$`2016 Labor Force Participation Rate`<- 
  as.numeric(Analysis_Data_1.3$`2016 Labor Force Participation Rate`)

Analysis_Data_1.3$`2016 GDP Per Capita` <- 
  as.numeric(Analysis_Data_1.3$`2016 GDP Per Capita`)

Analysis_Data_1.3$`2016 GDP Growth` <-
  as.numeric(Analysis_Data_1.3$`2016 GDP Growth`)

Analysis_Data_1.3$`Refugee Employment Rate`<-
  as.numeric(Analysis_Data_1.3$`Refugee Employment Rate`)

Analysis_Data_1.3$`Refugees under UNHCR's mandate`<-
  as.numeric(Analysis_Data_1.3$`Refugees under UNHCR's mandate`)

Analysis_Data_1.3$`Total Displaced People`<-
  as.numeric(Analysis_Data_1.3$`Total Displaced People`)

Analysis_Data_1.4 <- merge(x=Analysis_Data_1.3, y=Refugee_Democracy_C, by = "Country Name")

Analysis_Data_1.4 <- Analysis_Data_1.4[,-2]

str(Analysis_Data_1.4)

Analysis_Data_1.PEW <- Reduce(function(x,y) merge(x=x,y=y, by = "Country Name"), 
                              list(Analysis_Data_1.4, PEW_17_C, PEW_43_C)) #Good


# Analysis 2 Data Cleaning -----------------------------------

#Variable Description
#Y = Immigrant Employment
#X = Labor_Force, Democracy, Refugee_Num, GDP_per_Capita, GDP_Growth, *potential (PEW_Survey)

names(Imigrant_Employ_2016)[1] <- "Country Code"
Analysis_Data_2.1 <- Reduce(function(x,y) merge(x=x, y=y, by = "Country Code"), 
                            list(Imigrant_Employ_2016, GDP_Capita_2016, GDP_Growth_2016))

Analysis_Data_2.2 <- Analysis_Data_2.1[,-6]
Analysis_Data_2.2 <- Analysis_Data_2.2[,-2]

names(Analysis_Data_2.2)[2:5] <- 
  c("2016 Imigrant Employment Rate","Country Name","2016 GDP per Capita","2016 GDP Growth")

Analysis_Data_2.3 <- merge(x=Analysis_Data_2.2, y=Labor_Force_2016, by = "Country Code")

names(Analysis_Data_2.3)[7] <- c("2016 Labor Force Participation")
names(Analysis_Data_2.3)[3] <- c("Country Name")
Analysis_Data_2.3 <- Analysis_Data_2.3[,-6]

Analysis_Data_2.4 <- merge(x=Analysis_Data_2.3, y=Refugee_Num_2016, by = "Country Name")
Analysis_Data_2.4 <- Analysis_Data_2.4[-10:-14]

Analysis_Data_2.4 <- Analysis_Data_2.4[-8]
Analysis_Data_2.4 <- Analysis_Data_2.4[-7]

names(Analysis_Data_2.4)[7] <- c("Refugees under UNHCR's mandate")

#Translating Data into numeric

Analysis_Data_2.4$`2016 Imigrant Employment Rate` <- as.numeric(Analysis_Data_2.4$`2016 Imigrant Employment Rate`)

Analysis_Data_2.4$`2016 GDP per Capita` <- as.numeric(Analysis_Data_2.4$`2016 GDP per Capita`)

Analysis_Data_2.4$`2016 GDP Growth`<- as.numeric(Analysis_Data_2.4$`2016 GDP Growth`)

Analysis_Data_2.4$`2016 Labor Force Participation`<- as.numeric(Analysis_Data_2.4$`2016 Labor Force Participation`)

Analysis_Data_2.4$`Refugees under UNHCR's mandate` <- as.numeric(Analysis_Data_2.4$`Refugees under UNHCR's mandate`)

Analysis_Data_2.4$`Total Displaced People` <- as.numeric(Analysis_Data_2.4$`Total Displaced People`)
str(Analysis_Data_2.4)

Analysis_Data_2.5 <- merge(x=Analysis_Data_2.4, y=Imigrant_Democracy_C, by = "Country Name")

Analysis_Data_2.PEW <- Reduce(function(x,y) merge(x=x,y=y, by = "Country Name"), 
                              list(Analysis_Data_2.5, PEW_17_C, PEW_43_C))

# Analysis 3 Data Cleaning -------------------------------------

#Variable Description
#Y = Labor Force Participation 
#X = Total_Displaced, Refugee_Num, GDP_per_Capita, GDP_Growth, *potential (PEW_Survey), (Democracy)

names(Imigrant_Employ_2016)[1] <- "Country Code"
Analysis_Data_3.1 <- Reduce(function(x,y) merge(x=x, y=y, by = "Country Code"), 
                            list(Labor_Force_2016, GDP_Capita_2016, GDP_Growth_2016))
Analysis_Data_3.2 <- merge(x=Analysis_Data_3.1, y=Refugee_Num_2016, by = "Country Name")
Analysis_Data_3.3 <- subset(Analysis_Data_3.2,select = -c(2,3,5,8,9))
Analysis_Data_3.3 <- subset(Analysis_Data_3.3,select = -c(6:10))

names(Analysis_Data_3.3)[2:5] <- 
  c("2016 Labor Force Participation","2016 GDP per Capita","2016 GDP Growth","Refugees")

Analysis_Data_3.3$`2016 Labor Force Participation`<- 
  as.numeric(Analysis_Data_3.3$`2016 Labor Force Participation`)

Analysis_Data_3.3$`2016 GDP per Capita`<- 
  as.numeric(Analysis_Data_3.3$`2016 GDP per Capita`)

Analysis_Data_3.3$`2016 GDP Growth`<- 
  as.numeric(Analysis_Data_3.3$`2016 GDP Growth`)

Analysis_Data_3.3$Refugees <-
  as.numeric(Analysis_Data_3.3$Refugees)

Analysis_Data_3.3$`Total Displaced People`<-
  as.numeric(Analysis_Data_3.3$`Total Displaced People`)
str(Analysis_Data_3.3)

Analysis_Data_3.4 <- na.omit(Analysis_Data_3.3)

Analysis_Data_3.5 <- merge(x=Analysis_Data_3.4, y=Democracy_Rate_C, by = "Country Name")

Analysis_Data_3.PEW <- Reduce(function(x,y) merge(x=x,y=y, by = "Country Name"), 
                              list(Analysis_Data_3.4, PEW_17_C, PEW_43_C))

#Analysis 1: Linear Regression ----------------------------------------

# Variables
# `2016 GDP per Capita` +`2016 GDP Growth`+ 
# `2016 Labor Force Participation`+`Refugees under UNHCR's mandate` + `Total Displaced People` 

str(Analysis_Data_1.4)
cor(Analysis_Data_1.4[,-1])

# Analysis 1: Full Model  ----------------------------------------
Refugee.full <- lm(`Refugee Employment Rate`~ `Democracy Rating`+`2016 GDP Per Capita`+`2016 GDP Growth`+ 
                     `2016 Labor Force Participation Rate`+`Refugees under UNHCR's mandate` + 
                     `Total Displaced People` , data = Analysis_Data_1.4 )

summary(Refugee.full)

# Analysis 1: Automated Stepwise Regression -----------------------
Refugee.null <- lm(`Refugee Employment Rate` ~ 1, data=Analysis_Data_1.4)
Refugee.full <- lm(`Refugee Employment Rate`~ `Democracy Rating`+`2016 GDP Per Capita`+`2016 GDP Growth`+ 
                     `2016 Labor Force Participation Rate`+`Refugees under UNHCR's mandate` + 
                     `Total Displaced People` , data = Analysis_Data_1.4 )

Refugee.stepfwd <- stepAIC(Refugee.null, scope=list(lower=Refugee.null, upper=Refugee.full), direction="forward")
# Result: `Refugee Employment Rate` ~ `Democracy Rating` + `2016 GDP Per Capita`

Refugee.stepback <- stepAIC(Refugee.full, scope=list(lower=Refugee.null, upper=Refugee.full), direction="back")
# Result: `Refugee Employment Rate` ~ `Democracy Rating` + `2016 GDP Per Capita`

Refugee.stepfb <- stepAIC(Refugee.null, scope=list(lower=Refugee.null, upper=Refugee.full), direction="both")
# Result: `Refugee Employment Rate` ~ `Democracy Rating` + `2016 GDP Per Capita`


#Analysis 2: Immigrant Employment Rate Linear Regression -----------------------

#Variables
#X = `2016 Immigrant Employment Rate`+
#Y = `2016 GDP per Capita` +`2016 GDP Growth`+ `2016 Labor Force Participation`+
#`Refugees under UNHCR's mandate`+`Total Displaced People`+`Democracy Rating`

str(Analysis_Data_2.5)
cor(Analysis_Data_2.5[,-1])

# Analysis 2: Automated Stepwise Regression -----------------------

Imigrant.null <- lm(`2016 Imigrant Employment Rate`~ 1, data=Analysis_Data_2.5)
Imigrant.full <- lm(`2016 Imigrant Employment Rate`~ `2016 GDP per Capita`
                    +`2016 GDP Growth`+ `2016 Labor Force Participation`+
                      `Refugees under UNHCR's mandate`+`Total Displaced People`+
                      `Democracy Rating`, data=Analysis_Data_2.5)
summary(Imigrant.full)

Imigrant.stepfwd <- stepAIC(Imigrant.null, scope=list(lower=Imigrant.null, upper=Imigrant.full), direction="forward")
#`2016 Imigrant Employment Rate` ~ `2016 Labor Force Participation` + `Refugees under UNHCR's mandate`

Imigrant.stepback <- stepAIC(Imigrant.full, scope=list(lower=Imigrant.null, upper=Imigrant.full), direction="back")
#`2016 Imigrant Employment Rate` ~ `2016 Labor Force Participation` + `Refugees under UNHCR's mandate`

Imigrant.stepfb <- stepAIC(Imigrant.null, scope=list(lower=Imigrant.null, upper=Imigrant.full), direction="both")
#`2016 Imigrant Employment Rate` ~ `2016 Labor Force Participation` + `Refugees under UNHCR's mandate`

# Analysis 2 with PEW Data: Automated Stepwise Regression

Imigrant.null.PEW <- lm(`2016 Imigrant Employment Rate` ~ 1, data=Analysis_Data_2.PEW)
Imigrant.full.PEW <- lm(`2016 Imigrant Employment Rate`~ `Democracy Rating`+`2016 GDP per Capita`+`2016 GDP Growth`+ 
                          `2016 Labor Force Participation`+`Refugees under UNHCR's mandate` + 
                          `Total Displaced People` , data = Analysis_Data_2.PEW )

Imigrant.stepfwd.PEW <- stepAIC(Imigrant.null.PEW, scope=list(lower=Imigrant.null.PEW, upper=Imigrant.full.PEW), direction="forward")

Imigrant.stepback.PEW <- stepAIC(Imigrant.full.PEW, scope=list(lower=Imigrant.null.PEW, upper=Imigrant.full.PEW), direction="back")

Imigrant.stepfb.PEW <- stepAIC(Imigrant.null.PEW, scope=list(lower=Imigrant.null.PEW, upper=Imigrant.full.PEW), direction="both")
#All Models = `2016 Imigrant Employment Rate` ~ `2016 Labor Force Participation` + `Refugees under UNHCR's mandate`

#Analysis 3: Labor Force Participation Rate Linear Regression --------------------

#Variables
#X = `2016 Labor Force Participation`+
#Y = `2016 GDP per Capita` +`2016 GDP Growth`+ `2016 Labor Force Participation`+
#`Refugees under UNHCR's mandate`+`Total Displaced People`+`Democracy Rating`


# Analysis 3: Automated stepwise regression

Labor.null <- lm(`2016 Labor Force Participation`~1, data=Analysis_Data_3.5)
Labor.full <- lm(`2016 Labor Force Participation`~ `2016 GDP per Capita` +`2016 GDP Growth`+ 
                   `Refugees`+`Total Displaced People`+`Democracy Score`, data = Analysis_Data_3.5)
summary(Labor.full)

Labor.stepfwd <- stepAIC(Labor.null, scope=list(lower=Labor.null, upper=Labor.full), direction="forward")
#`2016 Labor Force Participation` ~ `Democracy Score` + Refugees + `2016 GDP per Capita`

Labor.stepback <- stepAIC(Labor.full, scope=list(lower=Labor.null, upper=Labor.full), direction="back")
#`2016 Labor Force Participation` ~  `Democracy Score` + Refugees + `2016 GDP per Capita` 

Labor.stepfb <- stepAIC(Labor.null, scope=list(lower=Labor.null, upper=Labor.full), direction="both")
#`2016 Labor Force Participation` ~ `Democracy Score` + Refugees + `2016 GDP per Capita`

Labor.final <- lm(`2016 Labor Force Participation` ~  `Refugees`+`Democracy Score`, data = Analysis_Data_3.5)

# Analysis 1: Checking Conditions  -----------------------

summary(Refugee.stepfwd)
summary(Refugee.stepback) 
summary(Refugee.stepfb) 

#Analysis 1: Checking Tolerance of `Democracy Rating`

Analysis_1_Tol <- lm(`Democracy Rating`~ `2016 GDP Per Capita`, data = Analysis_Data_1.4) 
summary(Analysis_1_Tol)

#Tolerance of Democracy Rating = .2501
#Cause for concern Democracy Rating and 2016 GDP per Capita

Refugee.NoCap <- lm(`Refugee Employment Rate`~ `Democracy Rating`+`2016 GDP Growth`+ 
                      `2016 Labor Force Participation Rate`+`Refugees under UNHCR's mandate` + 
                      `Total Displaced People` , data = Analysis_Data_1.4 )

vif(Refugee.full)
vif(Refugee.stepfwd)
vif(Refugee.NoCap) #This is better, no high vif values

plot(Refugee.stepfwd,which=1:2)

summary(Refugee.stepfwd)
confint(Refugee.stepfwd,level=.9)
?confint
#R-Values
#R-squared:  0.7356,	Adjusted R-squared:  0.6695

#F-Test: p-value: 0.004885

#P-Values:
#`Democracy Rating`     = 0.00764 **
#`2016 GDP Per Capita`  = 0.18307

#Graphs: 
#Residual plot is very skewed by the value 11 which is at -.02 yet otherwise no clear trend in residuals
#Normal Q-Q plot has no clear deviations and looks fine given the small data set

#No Clear Co-Linearity
#Relatively Low Standard Error

# Confidence Intervals
#                             5 %         95 %
#  (Intercept)           -4.808455e-01    1.283604e-01
#`Democracy Rating`      6.074817e-02     1.953101e-01
#`2016 GDP Per Capita`   -1.450088e-05    1.757359e-06


#Analysis 2: Checking Conditions  -----------------------

summary(Imigrant.stepfwd)
summary(Imigrant.stepback) 
summary(Imigrant.stepfb) 

vif(Imigrant.full)
vif(Imigrant.stepfwd) # This is Good

plot(Imigrant.stepfwd)

shapiro.test(Imigrant.stepfwd$residuals)

confint(Imigrant.stepfwd)

#F-Test: p-value: 1.528e-06

#P-Values:
#`2016 Labor Force Participation` = 7.42e-06 ***
#`Refugees under UNHCR's mandate` = 0.0378 * 

#Graphs: 
#Residual plot does not seem to show any clear relationship
#Normal Q-Q plot seems to have a high number of values above the line 
#Shapiro-wilk: p-value = 0.8933

#Co-Linearity: 
#No Clear Co-Linearity
#Relatively Low Standard Error

#Confidence Intervals
#                2.5 %        97.5 %
#(Intercept)                      -2.054255e+01  2.617569e+01
#`2016 Labor Force Participation`  6.666347e-01  1.426450e+00
#`Refugees under UNHCR's mandate` -8.610397e-06 -2.718178e-07


#Analysis 3: Checking Conditions   -----------------------

summary(Labor.stepfwd)
summary(Labor.stepfb)
summary(Labor.stepback)

summary(Labor.final)


vif(Labor.full)
vif(Labor.final) # This is Good

plot(Labor.final)
shapiro.test(Labor.final$residuals)
confint(Labor.final)

with(Analysis_Data_3.5,plot(`Democracy Score`,`2016 Labor Force Participation`))
abline(lm(`2016 Labor Force Participation` ~  `Democracy Score`,data=Analysis_Data_3.5))

#Model Selection = Labor.stepfwd
#F-Test: p-value: 0.02397

#P-Values:
#                       2.5 %        97.5 %
# (Intercept)        6.400730e+01   7.372482e+01
#Refugees          -1.054471e-05    -4.637438e-09
#`Democracy Score` -1.691998e+00    -1.275325e-01

#Graph
#Residual: No clear patern, strong outlier at 50,0 ; Slight final trend upwards
#Normality Q-Q: Slight trend upwards at the end
#Shapiro-Wilk: p-value = 0.6627

#No Clear Co-Linearity
#Relatively Low Standard Error