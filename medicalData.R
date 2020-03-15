#insData <- read.csv("insurance_0.csv")
#If you face any error with MySQL connection, Use the 1st line instead of the lines upto 14

library(RMySQL)
library(psych)
library(stats)
library(jtools)

conn<-dbConnect(MySQL(), dbname="insurance_db", username="root", password="")
dbListTables(conn)
dbListFields(conn, "ins_data")

insData <- dbGetQuery(conn, "SELECT * FROM ins_data;")

dim(insData)
str(insData)
summary(insData)

# Data preparation
insData$sex <- as.factor(insData$sex)
insData$smoker <- as.factor(insData$smoker)
insData$region <- as.factor(insData$region)
summary(insData)

# To check the data types
str(insData)

# To display first few rows
head(insData)

length(insData$region) ## 289,955 records in the file

summary(insData$charges)
hist(insData$charges)

table(insData$region)

cor(insData[c("age", "bmi", "children", "charges")])
pairs(insData[c("age", "bmi", "children", "charges")])
pairs.panels(insData[c("age", "bmi", "children", "charges")])

medModel<-lm(charges~age+children+bmi+sex+smoker+region, data=insData)
medModel
summary(medModel)

# Improving the performance
insData$age2 <- insData$age^2
insData$bmi30 <- ifelse(insData$bmi >= 30, 1, 0)
medModel2<-lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region, data=insData)
medModel2
summary(medModel2)

#Predict on the given combination of data using medModel
medTest <- data.frame(age=19, children=0, bmi=27.9, sex="female", smoker="yes", region="southwest")
predict(medModel, medTest)

#Predict on the given combination of data using medModel2
medTest2 <- data.frame(age=19, age2=19^2, bmi30=0, children=0, bmi=27.9, sex="female", smoker="yes", region="southwest")
predict(medModel2, medTest2)

effect_plot(medModel, pred = sex, interval = TRUE, plot.points = TRUE, rug = TRUE, point.color = "grey")
effect_plot(medModel2, pred = sex, interval = TRUE, plot.points = TRUE, rug = TRUE, point.color = "grey")

effect_plot(medModel, pred = age, interval = TRUE, plot.points = TRUE, rug = TRUE, point.color = "grey")
effect_plot(medModel2, pred = age, interval = TRUE, plot.points = TRUE, rug = TRUE, point.color = "grey")
