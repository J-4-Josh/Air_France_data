######################################################
##### HULT Data Science:R Team Project Assignment 
##### Date: 11.4.2021 
##### Version 1.0
##### Created by: Team 5
##### AirFrance
######################################################

airFrance <- read_excel("C:/Users/wpuan/Desktop/AirFrance.xls", sheet = "DoubleClick")

##### Remove space in column name #####
colnames(airFrance)[2] <- "PublisherName"
colnames(airFrance)[22] <- "TotalCost"
colnames(airFrance)[17] <- "EngineClickThru"
colnames(airFrance)[7] <- "KeywordGroup"

##### Add Revenue column by Amount-TotalCost #####
airFrance$Revenue <- (airFrance$Amount - airFrance$TotalCost)

##### Define Binary column for logistic regression #####
airFrance$Binary <- ifelse(airFrance$Revenue >0, 1, 0)

##### Normalizing variables #####
airFrance$Clicks_Norm <- (airFrance$Clicks - min(airFrance$Clicks))/(max(airFrance$Clicks)-min(airFrance$Clicks))
airFrance$Impressions_Norm <- (airFrance$Impressions - min(airFrance$Impressions))/(max(airFrance$Impressions)-min(airFrance$Impressions))
airFrance$EngineClickThru_Norm <- (airFrance$EngineClickThru - min(airFrance$EngineClickThru))/(max(airFrance$EngineClickThru)-min(airFrance$EngineClickThru))airFrance$EngineClickThru_Norm <- (airFrance$EngineClickThru - min(airFrance$EngineClickThru))/(max(airFrance$EngineClickThru)-min(airFrance$EngineClickThru))

##### Random sampling data #####
index <- sample(1:nrow(airFrance), size = 0.8*nrow(airFrance))
airFrance_train <- airFrance[index,]
airFrance_test <- airFrance[-index,]

##### Logistic regression #####
my_logit <- glm(Binary ~ Clicks_Norm+Impressions_Norm+EngineClickThru, data=airFrance_train, family="binomial")
summary(my_logit)

##### Plotting ROC AUC graph #####
library(caret)
my_prediction_testing <- predict(my_logit, airFrance_test, type="response")

confusionMatrix(data=as.factor(as.numeric(my_prediction_testing > 0.5)), 
                reference=as.factor(as.numeric(airFrance_test$Binary)))

library(ROCR)
pred_val_logit <- prediction(my_prediction_testing, airFrance_test$Binary)
pref_logit <- performance(pred_val_logit, "tpr", "fpr")
plot(pref_logit)


### Finding Top 5 Keyword groups that generate most clicks for each platform###

yahooUS <- subset(airFrance, PublisherName == "Yahoo - US")
googleUS <- subset(airFrance, PublisherName == "Google - US")
msnUS <- subset(airFrance, PublisherName == "MSN - US")
overtureUS <- subset(airFrance, PublisherName == "Overture - US")

yahooUS_aggregated <- aggregate(yahooUS$Clicks, by=list(KeywordGroup=yahooUS$KeywordGroup), FUN=sum)
yahooUS_sorted <- yahooUS_aggregated[order(yahooUS_aggregated$x, decreasing = TRUE), ]
yahooUS_Top5 <- head(yahooUS_sorted, 5)

googleUS_aggregated <- aggregate(googleUS$Clicks, by=list(KeywordGroup=googleUS$KeywordGroup), FUN=sum)
googleUS_sorted <- googleUS_aggregated[order(googleUS_aggregated$x, decreasing = TRUE), ]
googleUS_Top5 <- head(googleUS_sorted, 5)

msnUS_aggregated <- aggregate(msnUS$Clicks, by=list(KeywordGroup=msnUS$KeywordGroup), FUN=sum)
msnUS_sorted <- msnUS_aggregated[order(msnUS_aggregated$x, decreasing = TRUE), ]
msnUS_Top5 <- head(msnUS_sorted, 5)

overtureUS_aggregated <- aggregate(overtureUS$Clicks, by=list(KeywordGroup=overtureUS$KeywordGroup), FUN=sum)
overtureUS_sorted <- overtureUS_aggregated[order(overtureUS_aggregated$x, decreasing = TRUE), ]
overtureUS_Top5 <- head(overtureUS_sorted, 5)

airFrance_aggregated <- aggregate(airFrance$Clicks, by=list(KeywordGroup=airFrance$KeywordGroup), FUN=sum)
airFrance_sorted <- airFrance_aggregated[order(airFrance_aggregated$x, decreasing = TRUE), ]
airFrance_Top10 <- head(airFrance_sorted[-1,], 10)

##### Plotting bar chart for top keyword group #####

mx_yahooUS <- matrix(c(yahooUS_Top5$x), nrow = 1, byrow = TRUE, dimnames = list("Clicks", yahooUS_Top5$KeywordGroup))
mx_googleUS <- matrix(c(googleUS_Top5$x), nrow = 1, byrow = TRUE, dimnames = list("Clicks", googleUS_Top5$KeywordGroup))
mx_msnUS <- matrix(c(msnUS_Top5$x), nrow = 1, byrow = TRUE, dimnames = list("Clicks", msnUS_Top5$KeywordGroup))
mx_overtureUS <- matrix(c(overtureUS_Top5$x), nrow = 1, byrow = TRUE, dimnames = list("Clicks", overtureUS_Top5$KeywordGroup))
mx_airFrance <- matrix(c(airFrance_Top10$x), nrow = 1, byrow = TRUE, dimnames = list("Clicks", airFrance_Top10$KeywordGroup))

barplot(mx_yahooUS, main="Yahoo Top 5 Keyword Groups", ylim=c(0,35000), xlab="Keyword Group", ylab="Clicks")
barplot(mx_googleUS, main="Google Top 5 Keyword Groups", ylim=c(0,80000), xlab="Keyword Group", ylab="Clicks")
barplot(mx_msnUS, main="MSN Top 5 Keyword Groups", ylim=c(0,8000), xlab="Keyword Group", ylab="Clicks")
barplot(mx_overtureUS, main="Overture Top 5 Keyword Groups", ylim=c(0,130000), xlab="Keyword Group", ylab="Clicks")
barplot(mx, main="Air France Top 10 Keyword Groups", ylim=c(0,120000), xlab="Keyword Group", ylab="Clicks")


#Summary per publisher
View(airFrance)
table(airFrance$`Publisher Name`)
Google_global <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="Google - Global"))
Google_US <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="Google - US"))
MSN_global <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="MSN - Global"))
MSN_US <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="MSN - US"))
Ove_global <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="Overture - Global"))
Ove_US <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="Overture - US"))
Yahoo_US <- data.frame(subset(airFrance, airFrance$`Publisher Name`=="Yahoo - US"))



Google_global_data <- c(Google_global$Publisher.Name[1], sum(Google_global$Amount), sum(Google_global$Clicks), mean(Google_global$Avg..Cost.per.Click), mean(Google_global$Total.Cost..Trans.))
Google_US_data <- c(Google_US$Publisher.Name[1], sum(Google_US$Amount), sum(Google_US$Clicks), mean(Google_US$Avg..Cost.per.Click), mean(Google_US$Total.Cost..Trans.))
MSN_G_data <- c(MSN_global$Publisher.Name[1], sum(MSN_global$Amount), sum(MSN_global$Clicks), mean(MSN_global$Avg..Cost.per.Click), mean(MSN_global$Total.Cost..Trans.))
MSN_U_data <- c(MSN_US$Publisher.Name[1], sum(MSN_US$Amount), sum(MSN_US$Clicks), mean(MSN_US$Avg..Cost.per.Click), mean(MSN_US$Total.Cost..Trans.))
Ove_G_data <- c(Ove_global$Publisher.Name[1], sum(Ove_global$Amount), sum(Ove_global$Clicks), mean(Ove_global$Avg..Cost.per.Click), mean(Ove_global$Total.Cost..Trans.))
Ove_U_data <- c(Ove_US$Publisher.Name[1], sum(Ove_US$Amount), sum(Ove_US$Clicks), mean(Ove_US$Avg..Cost.per.Click), mean(Ove_US$Total.Cost..Trans.))
Yahoo_U_data <- c(Yahoo_US$Publisher.Name[1], sum(Yahoo_US$Amount), sum(Yahoo_US$Clicks), mean(Yahoo_US$Avg..Cost.per.Click), mean(Yahoo_US$Total.Cost..Trans.))


Publisher_data <- data.frame(Google_global_data, Google_US_data, MSN_G_data, MSN_U_data, Ove_G_data, Ove_U_data, Yahoo_U_data)
row.names(Publisher_data) <- c("Publisher", "Amount", "Clicks", "Av. Cost(clicks)", "Av. trans. conv")



