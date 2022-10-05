#Packages used in developing the code
library(tidyverse)
library(corrplot)
library(broom)

#Reading from the first data set
dataSet1 <- read.csv("WHO-COVID-19-global-data.csv")
#names(dataSet1)  #for testing purposes only

#Reading from the second data set
dataSet2 <- read.csv("covid-vaccination-vs-death_ratio.csv")
#names(dataSet2) #for testing purposes only

#Extracting only the data for UNITED STATES
table1 <- filter(dataSet1, Country_code == 'US')
table2 <- filter(dataSet2, iso_code == 'USA')

#Merging the two tables into one single data frame
mergedDataSet <- merge(table1, table2, by.x='Date_reported', by.y='date')

#Adding a recovery rate column formed from the other two columns
mergedDataSet$Recovery_rate <- abs(mergedDataSet$Cumulative_cases - mergedDataSet$Cumulative_deaths)
#names(mergedDataSet)  #for testing purposes only

#Initializing the dependent and independent variables
x <- mergedDataSet[["Recovery_rate"]]
y <- mergedDataSet[["people_fully_vaccinated"]]
#Plotting the graph
plot(x,y, xlab ='Recovery_rate', ylab = 'people_fully_vaccinated')

#Fitting the line
mergedDataSetFit <- lm(formula= y ~ x)
abline(mergedDataSetFit, col=7)

#Summary of the graph
summary(mergedDataSetFit)

#Plotting the diagnostic plots
par(mfrow = c(2,2))
plot(mergedDataSetFit)

#Data cleaning
dropList <- c("Date_reported","Country_code", "Country","WHO_region","country","iso_code","X1012.611111","ratio","X","New_deaths.y","population")
finalDataSet <- mergedDataSet[,!(names(mergedDataSet) %in% dropList)]
finalDataSet <- finalDataSet %>% mutate_all(as.numeric)

#Forming the correlation plot
corrplot(cor(finalDataSet),method = "number", type = "upper")

#Re-plotting the graph with refined columns

x <- finalDataSet[['Recovery_rate']] #Recovery_rate
y <- finalDataSet[['people_fully_vaccinated']] #people_fully_vaccinated
plot(x,y, xlab ='Recovery_rate', ylab = 'people_fully_vaccinated')
finalDataSetFit <- lm(formula= y ~ x)
abline(finalDataSetFit, col=7)
summary(finalDataSetFit)

#Plotting the diagnostic plots
par(mfrow = c(2,2))
plot(finalDataSetFit)

#Plotting the Fitted and Residual Values
values <- augment(finalDataSetFit)
head(values)
ggplot(values,aes(x,y)) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  geom_segment(aes(xend = x, yend = .fitted), color = "yellow", size = 0.3)

#Check for Multi Linear Regression

mlrFitModel <- lm(Recovery_rate ~ people_vaccinated+people_fully_vaccinated, data=finalDataSet) #mlr was changed here
summary(mlrFitModel)
par(mfrow = c(2,2))
plot(mlrFitModel)

#Checking for Leverages
leverages <- hatvalues(mlrFitModel) > 3 * mean(hatvalues(mlrFitModel))
finalDataSet[leverages,]

#Checking for Outliers
outliers <- rstudent(mlrFitModel) > 3
finalDataSet[outliers, ]

#Checking for influential points using DFFITS
p <- length(mlrFitModel$coefficients)
n <- nrow(mlrFitModel$model)
critical_dffits = 2 * sqrt((p + 1) / (n - p - 1))
data_dffits = dffits(mlrFitModel)
finalDataSet[which(abs(data_dffits)>critical_dffits),]

#Checking for influential points using Cook's Distance
cooks_crit = 0.5
COOKS <- cooks.distance(mlrFitModel)
finalDataSet[which(abs(COOKS) > cooks_crit),]


#Removing the leverages
finalDataSetWoLev <- finalDataSet[!leverages,]
finalDataSetWoLevModel <- lm(Recovery_rate ~ people_fully_vaccinated+people_vaccinated, data=finalDataSetWoLev)

#Re-plotting the model after removing Leverages
summary(finalDataSetWoLevModel)
par(mfrow = c(2,2))
plot(finalDataSetWoLevModel)




