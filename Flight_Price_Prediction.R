
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
train <- read_excel(file.choose())
test <- read_excel(file.choose())

test$Price <- 0
train <- data.frame(train)
test <- data.frame(test)

test$Ind <- FALSE
train$Ind <- TRUE
str(train)
str(test)

mydata <- rbind(train, test)

head(mydata)

table(is.na(mydata))

table(is.na(mydata))
# Since there are only two na value, so can drop it
train.comp <- na.omit(mydata)
#train.comp <- complete.cases(train)
table(is.na(train.comp))
str(train.comp)

pairs.panels(train)

hist(train$Price, density = NULL,  breaks=150, col = " red",
                    xlab = "Flight Price", main = "Distribution of Price")

train.comp$Date_of_Journey <- dmy(train.comp$Date_of_Journey)

train.comp <-separate(train.comp, "Date_of_Journey", 
                      c('Journey_Year', 'Journey_Month','Journey_day'), sep = '-')

train.comp <- separate(train.comp, "Dep_Time" , 
                       c('Dep_Hour', 'Dep_minutes'), sep = ':')

train.comp <- separate(train.comp, "Arrival_Time" , 
                       c('Arr_Hour', 'Arr_minutes'), sep = ':')
str(train.comp)
train.comp <- separate(train.comp, "Arr_minutes" , 
                       c('Arr_minutes','Arr_month'),remove = T, sep = ' ')

train.comp <- separate(train.comp, "Duration", 
                       c('Dur_hour', 'Dur_minute'), sep =' ' )

#train.comp <- separate(train.comp, "Dur_hour", 
#                       into = c('Dur_hour'), sep =' ')

library(tidyverse)
train.comp$Dur_hour <-parse_number(train.comp$Dur_hour, na = c("h"))
train.comp$Dur_minute <-parse_number(train.comp$Dur_minute, na = c("m"))
str(train.comp)

#here replace all NA value with 0
train.comp$Dur_minute[is.na(train.comp$Dur_minute)] <- 0

#NOw we will handle Categorical data
# Handling AIRline col
table(train.comp$Airline)

#try to make histagram to check total no of airline
ggplot(train.comp, aes(x = Airline, y = Price)) + 
                     geom_bin_2d(aes(inherit.aes = TRUE)) 

ggplot(train.comp, aes(x = Airline, y = Price,  fill = Airline)) + 
                     geom_boxplot() +theme(axis.text.x = element_blank())


       #One hotencoding 
       # dummify the data
       
       
       Airline = factor(train.comp$Airline) 
       dumm = as.data.frame(model.matrix(~Airline )[,-1])
       train.comp <- cbind(train.comp, dumm)
       train.comp<- train.comp[, c(-2,-16)]
       
       Mod_data <- train.comp
       
       #One hotencoding 
       # # dummify the data for source varible
       # #model.matrix creates a design (or model) matrix, 
       # e.g., by expanding factors to a set of dummy variables 
       # (depending on the contrasts) and expanding \
       # interactions similarly.
       Source <- as.factor(Mod_data$Source)
       dumm2<- as.data.frame(model.matrix(~Source))[,-1]
       Mod_data <- cbind(Mod_data, dumm2)
       ggplot(train.comp, aes(x = Source, y = Price,  fill = Source)) + 
         geom_boxplot() +theme(axis.text.x = element_blank())
       
       str(Mod_data)
       #One hotencoding 
       # # dummify the data for Destination varible
       
       Destination <- factor(Mod_data$Destination)
       dumm3 <- as.data.frame(model.matrix(~Destination))[,-1]
       
       Mod_data <- cbind(Mod_data, dumm3)
       
       Mod_data <- subset(Mod_data, select = -c(Route))
       
       #Labelencoding in Total_Stops
       library(superml)
       lbl <- LabelEncoder$new()
       c <-lbl$fit(Mod_data$Total_Stops)
       Mod_data$Total_Stops <- lbl$fit_transform(Mod_data$Total_Stops)
       
       Mod_data <- subset(Mod_data, select= -c(Destination, Source))
       Mod_data <- subset(Mod_data, select= -c(Arr_month))
       str(Mod_data)
       
       #Check for na vlaue 
       table(is.na(Mod_data))
       
       final_data <- Mod_data
       
       
       final_data$Dep_Hour <- as.numeric(final_data$Dep_Hour)
       final_data$Dep_minutes <- as.numeric(final_data$Dep_minutes)
       final_data$Arr_Hour <- as.numeric(final_data$Arr_Hour )
       final_data$Arr_minutes <- as.numeric(final_data$Arr_minutes)
       
       final_data$Journey_Month <- as.numeric(final_data$Journey_Month)
       final_data$Journey_day <- as.numeric(final_data$Journey_day)
      
       
       str(final_data)
       
       names(final_data)<-make.names(names(final_data))
       
       train.clean <- final_data[final_data$Ind == TRUE, ]
       test.clean <- final_data[final_data$Ind== FALSE, ]
       
       test.clean <- subset(test.clean, select = -c(Price))
       
       pairs.panels(final_data[6:10])
       library(corrplot)
       M2<-cor(final_data[-12, -1])
       
       heatmap(M2, hclustfun = hclust,  margins = c(5, 5))
       library(ggplot2)
       library(ggheatmap)
       ggheatmap(M[1:15,1:10], color=colorRampPalette(c( "red","green","blue"))(100),
                 dist_method="euclidean")
 
             
                
       
   
       # Imp Feature selection using Boruta algo
       library(Boruta)
       library(caret)
        boruta <- Boruta(Price~.,data = train.clean, doTrace = T, maxRun = 200)
       
       library(randomForest)
       
       #Random Forest
       
        random <- randomForest(Price~Dur_hour+Journey_Month+AirlineJet.Airways+
                                   Journey_day+AirlineJet.Airways.Business+Total_Stops+
                                 AirlineIndiGo+DestinationNew.Delhi+Dep_Hour+Arr_Hour+
                                 AirlineMultiple.carriers+Dep_minutes+Dur_hour+SourceKolkata+
                                 AirlineAir.India+AirlineSpiceJet+SourceDelhi + AirlineGoAir,
                               data = train.clean, 
                               importance = T)
    
        
        varImpPlot(random)
         imp <- data.frame(varImp(random))
       
        pred1 <- predict(random, train.clean)
        
        plot(train.clean, pred1)
        plot(test.clean, pred1)
        
        Price_error <- (train.clean$Price - pred1)/train.clean$Price
        hist(Price_error, breaks = 500, xlim = c(-1,1),
             main = "Accuracy Before hyperparameter tuning")
 #########################################################################      
        library(xgboost)
       
       set.seed(1234)
       cvcontrol <- trainControl(method = "repeatedcv",
                                 number = 7,
                                 repeats = 2,
                                 allowParallel = T)
       
       boost <- train(Price~Dur_hour+Journey_Month+AirlineJet.Airways+
                          Journey_day+AirlineJet.Airways.Business+Total_Stops+
                          AirlineIndiGo+DestinationNew.Delhi+Dep_Hour+Arr_Hour+
                          AirlineMultiple.carriers+Dep_minutes+Dur_hour+SourceKolkata+
                          AirlineAir.India+AirlineSpiceJet+SourceDelhi + AirlineGoAir,
                    data = train.clean,
                    method = "xgbTree",
                    trControl = cvcontrol,
                    importance =T,tuneGrid =expand.grid(nrounds = 500,
                                                        max_depth =5,
                                                        eta = 0.2,
                                                        gamma= 2.1,
                                                        colsample_bytree =1,
                                                        min_child_weight =1,
                                                        subsample =1))
         
       plot(varImp(boost))
       pred1 <- predict(boost, train.clean)
       pred2 <- predict(boost, test.clean)
       plot(train.clean, pred1)
       plot(test.clean, pred2)
       
       hist(pred1, breaks = 2000)
       hist(pred2, breaks = 50)
       
       Price_error <- (train.clean$Price - pred1)/train.clean$Price
       hist(Price_error, breaks = 500, xlim = c(-1,1),
            main = "Accuracy After hyperparameter tuning")
       
       
######################################################################
     
       boost1 <- train(Price~.,
                    data = test.clean,
                    method = "xgbTree",
                    trControl = cvcontrol,
                    importance =T,  tuneGrid =expand.grid(nrounds = 400,
                                                        max_depth =3,
                                                        eta = 0.2,
                                                        gamma= 2.1,
                                                        colsample_bytree =1,
                                                        min_child_weight =1,
                                                        subsample =1))
       
       
###########################################################################################
 
       set.seed(1234)
       Forest <- train(Price~Dur_hour+Month+AirlineJet.Airways+
                         day+AirlineJet.Airways.Business+Total_Stops+
                         AirlineIndiGo+DestinationNew.Delhi+Hour+Arr_Hour+
                         AirlineMultiple.carriers+minutes+Dur_hour+SourceKolkata+
                         AirlineAir.India+AirlineSpiceJet+SourceDelhi + AirlineGoAir,
                       data = final_data,
                       method = "rf",
                       trControl = cvcontrol,
                       importance =T)
       
       plot(varImp(Forest))
       plot(Forest)
       
       
       
       