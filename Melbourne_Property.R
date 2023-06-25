# Data Preparation
# Install and Load the required libraries.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart",repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales",repos = "http://cran.us.r-project.org")
if(!require(BBmisc)) install.packages("BBmisc",repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest",repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(stringr)
library(gridExtra)
library(rpart)
library(scales)
library(BBmisc)
library(randomForest)

# Downloand and save the file in your "working directory"
"https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market?select=MELBOURNE_HOUSE_PRICES_LESS.csv"

# Read the file and set digits to 4
file <- "C:\\Users\\shane\\Documents\\R\\MELBOURNE_HOUSE_PRICES_LESS.csv"
property <- read.csv(file)
digits = 4

# Dataset Inspection 
# Inspect the data-set
head(property)
str(property)
colnames(property)

# Check for missing data.
sum(is.na(property))

# Dataset Modification
# Investigating missing data
property %>% summarise_all(~ sum(is.na(.)))

property %>% filter (is.na(Price)) %>%
        group_by(Method) %>% 
        summarise(Count = n()) %>%
        arrange(desc(Count))

#Remove rows with no prices
property <- property %>% filter(!(is.na(Price))) 
nrow(property)

# Inspect the "Type" variable
property %>% 
        group_by(Type) %>%
        summarise(Count = n())

# Updating the "Type" variable.
property$Type <- str_replace(property$Type,"^h", "House")
property$Type <- str_replace(property$Type,"^u", "Unit")
property$Type <- str_replace(property$Type,"^t", "Townhouse")

# Inspect the "Method" variable
property %>% 
        group_by(Method) %>%
        summarise(Count = n())

# Updating the "Method" variable.
property$Method <- str_replace(property$Method,"^S$", "Sold")
property$Method <- str_replace(property$Method,"^PI$", "Passed In")
property$Method <- str_replace(property$Method,"^SA$", "Sold After")
property$Method <- str_replace(property$Method,"^SP$", "Sold Prior")
property$Method <- str_replace(property$Method,"^VB$", "Vendor Bid")

# Trim the "CouncilArea" data 
property <- property %>% mutate(CityCouncil = word(CouncilArea, 1,-3)) %>% select(-CouncilArea)

# Remove the data columns we don't require
property <- property %>% select(-Address,-Postcode)

# Data Analysis

## Pricing
# Summary of the Price variable
summary(property$Price)

# Top and Bottom 5 properties by price
property %>% arrange(desc(Price)) %>% head(5)
property %>% arrange(desc(Price)) %>% tail(5)

# Histogram of the prices
property %>% ggplot(aes(Price)) +
        geom_histogram(color = "azure3") +
        scale_x_continuous(limits = c(0, 12000000)) +
        geom_vline(xintercept = mean(property$Price)
                   , lty = 2, color = "red") +
        xlab("Price") +
        ylab("Count") +
        ggtitle("Pricing Histogram") 

# Removing outlier values greater than 3 million dollar
tibble(All = nrow(property), Upto_3M = nrow(property %>% filter( Price <= 3000000)))
property <- property %>% filter( Price <= 3000000) 

# Histogram of prices under 3 million
property %>% ggplot(aes(Price)) +
        geom_histogram(color = "azure3") +
        scale_x_continuous(limits = c(0, 3000000), 
                           breaks = seq(0, 3000000, 500000)) +
        geom_vline(xintercept = mean(property$Price), 
                   lty = 2, color = "red") +
        xlab("Price") +
        ylab("Count") +
        ggtitle("Pricing Histogram") 

# The "Suburb" feature
# The different suburbs,regions and council areas 
property %>% summarise(uniq_Suburbs = n_distinct(Suburb),
                       uniq_Regions = n_distinct(Regionname),
                       uniq_Councils = n_distinct(CityCouncil))

# Average number of properties sold in a suburb
avg_properties_suburb <- property %>% 
        group_by(Suburb) %>%
        summarise(Count = n()) %>%
        summarise(Avg = mean(Count)) %>%
        pull(.)
avg_properties_suburb

# Top and Bottom 5 suburbs by properties sold
property %>% group_by(Suburb) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        head(5)

property %>% group_by(Suburb) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        tail(5)

# Top 25 suburbs by sales numbers
property %>% 
        group_by(Suburb) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        head(25) %>%
        ggplot(aes(Count, reorder(Suburb,Count))) +
        geom_col(color = "azure3") +
        geom_vline(xintercept = avg_properties_suburb,lty = 2, color = "red") +
        theme(axis.text.y = element_text(size = 5)) + 
        xlab("Count") +
        ylab("Suburb") +
        ggtitle("Top 25 Suburbs by Sales Numbers") 

# Top 25 suburbs by average sales price
property %>% 
        group_by(Suburb) %>%
        summarise(Count = n(), Avg_Price = mean(Price)) %>%
        arrange(desc(Avg_Price)) %>%
        head(25) %>% 
        ggplot(aes(x = Avg_Price,y = reorder(Suburb,Avg_Price))) +
        geom_col(color = "azure3") + 
        geom_vline(xintercept = mean(property$Price), lty = 2, color = "red") +
        theme(axis.text.y = element_text(size = 5)) +
        xlab("Avg Sales Price") +
        ylab("Suburb") +
        ggtitle("Top 25 Suburbs by Average Sales Price") 

# Bottom 25 suburbs by sales numbers
property %>% 
        group_by(Suburb) %>%
        summarise(Count = n()) %>%
        filter(Count > 5) %>% 
        arrange(desc(Count)) %>%
        tail(25) %>%
        ggplot(aes(Count, reorder(Suburb,Count))) +
        geom_col(color = "azure3") +
        geom_vline(xintercept = avg_properties_suburb,lty = 2, color = "red") +
        theme(axis.text.y = element_text(size = 5)) + 
        xlab("Count") +
        ylab("Suburb") +
        ggtitle("Bottom 25 Suburbs by Sales Numbers") 

# Bottom 25 suburbs by average sales price
property %>% 
        group_by(Suburb) %>%
        summarise(Count = n(), Avg_Price = mean(Price)) %>%
        arrange(desc(Avg_Price)) %>%
        tail(25) %>% 
        ggplot(aes(x = Avg_Price,y = reorder(Suburb,Avg_Price))) +
        geom_col(color = "azure3") + 
        geom_vline(xintercept = mean(property$Price), lty = 2, color = "red") +
        theme(axis.text.y = element_text(size = 5)) +
        xlab("Avg Sales Price") +
        ylab("Suburb") +
        ggtitle("Bottom 25 Suburbs by Average Sales Price") 

# The "Rooms" feature
# Summarise the number of rooms by sold properties
property %>% group_by(Rooms) %>% 
        summarise(Count = n()) %>% 
        arrange(desc(Count))

# Count of Properties by the Number of Rooms
property %>% group_by(Rooms) %>%
        summarise(Count = n(), Avg_Price_Room = mean(Price)) %>%
        ggplot(aes(Rooms,Count)) +
        geom_col(color = "azure3") +
        scale_x_continuous(breaks = seq(0,32,5)) +
        ggtitle("Properties by Rooms") 

# Keep properties with 5 or less bedrooms.
tibble(All_Rooms = nrow(property), Upto_5rooms = nrow(property %>% filter(Rooms <= 5)))
property <- property %>% filter(Rooms <= 5)

# Property count with 5 or less bedrooms along with average price per type
property %>% group_by(Rooms) %>%
        summarise(Count = n(), Avg_Price_Room = mean(Price)) %>%
        ggplot(aes(Rooms,Count, label = dollar(round(Avg_Price_Room)))) +
        geom_col(color = "azure3") +
        geom_text(position = position_dodge(width = .9),
                  vjust = -0.5, size = 3) +
        scale_x_continuous(breaks = seq(0,5,1)) +
        ggtitle("Properties by Rooms") 

# Property "Type" feature
# Count and Average Price by each property type
property %>% group_by(Type) %>% summarise(Count = n(),Avg_Price = mean(Price)) %>% arrange(desc(Count))

# Histogram of counts and average prices by property "Types"
property %>% group_by(Type) %>%
      summarise(Count = n(), Avg_Price_Type = mean(Price)) %>%
      ggplot(aes(Type,Count,label = dollar(round(Avg_Price_Type)))) +
      geom_col(color = "azure3") +
      geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 3) +
      ggtitle("Properties by Type")

# The sales "Method" feature
# Statistics of Sales using different sales methods
property %>% 
        group_by(Method) %>%
        summarise(Count = n(), Avg_Price = mean(Price),Min = min(Price), Max = max(Price))

# Histogram of counts vs average, higest and lowest prices by sales "Methods"
method_avg_price <- property %>% 
                        group_by(Method) %>%
                        summarise(Count = n(), Avg_Price = mean(Price)) %>%
                        ggplot(aes(Count,Avg_Price)) +
                        geom_point(size = 3) +
                        ylim(0, 1500000) +
                        geom_smooth() +                
                        ggtitle("Average Sales Price by Sales Method")

method_max_price <- property %>% 
                        group_by(Method) %>%
                        summarise(Count = n(), Max = max(Price)) %>%
                        ggplot(aes(Count,Max)) +
                        geom_point(size = 3) +
                        ylim(2500000, 3500000) +
                        geom_smooth() +                
                        ggtitle("Higest Sales Price by Sales Method")

method_min_price <- property %>% 
                        group_by(Method) %>%
                        summarise(Count = n(), Min = min(Price)) %>%
                        ggplot(aes(Count,Min)) +
                        geom_point(size = 3) +
                        ylim(0,250000) +
                        geom_smooth() +                
                        ggtitle("Lowest Sales Price by Sales Method")

grid.arrange(method_avg_price,method_max_price,method_min_price,nrow = 3)

# The selling agency feature, "SellerG"
# View number of unique properties & agents along with the average sales price
property %>% 
        summarise(Properties_Sold = nrow(property),
                  Number_of_Agents = n_distinct(SellerG),
                  Avg_Sales_Price = round(mean(property$Price), digits = 0))

# Histogram of Average sales prices by number of properties sold
property %>% 
        group_by(SellerG) %>% 
        summarise(Count = n(), Avg_Price = mean(Price)) %>%
        filter(Count >=10) %>% 
        ggplot(aes(Count, Avg_Price)) +
        geom_point() +
        geom_hline(yintercept = mean(property$Price),
                   lty = 2, color = "red") +
        ggtitle("Avg Price VS Property Count Sold by Agents")

# Histogram of Average Sales by Top and Bottom 50 Agents by number of properties sold
top_agents <- property %>% 
                group_by(SellerG) %>%
                summarise(Count = n(),Avg_Price = mean(Price)) %>%
                arrange(desc(Count)) %>%
                head(50) %>%
                ggplot(aes(Count, Avg_Price)) +
                geom_point() +  
                geom_hline(yintercept = mean(property$Price),
                           lty = 2, color = "red") +
                scale_y_continuous(breaks = seq(400000,1800000,400000)) +
                geom_smooth() +
                ggtitle("Avg Price VS Property Count by Top Agents")

# Filtering only those agents who sold 20 or more properties to give us a better insight 
bottom_agents <- property %>% 
                group_by(SellerG) %>%
                summarise(Count = n(), Avg_Price = mean(Price)) %>%
                filter(Count >= 20) %>%
                arrange(Count) %>%
                head(50) %>%
                ggplot(aes(Count, Avg_Price)) +
                geom_point() +
                geom_hline(yintercept = mean(property$Price),
                           lty = 2, color = "red") +
                scale_y_continuous(breaks = seq(400000,1800000,400000)) +
                geom_smooth() +
                ggtitle("Avg Price VS Property Count by Bottom Agents") 

grid.arrange(top_agents,bottom_agents)

# Remove the selling agents information.
property <- property %>% select(-SellerG)

# The Region Name and Distance variable
# Properties sold per Region and relative distace to the city centre
property %>% 
        group_by(Regionname) %>%
        summarise(Count = n(), Avg_Price = mean(Price),Proximity = min(Distance)) %>%
        arrange(desc(Avg_Price))

# Inspecting the contents of the Northern Metropolitan region.
property %>% 
        filter(Regionname == "Northern Metropolitan") %>%
        group_by(CityCouncil) %>%
        summarise(Proximity = min(Distance)) %>%
        arrange(desc(Proximity))

# Removing the RegionName column from the data-set
property <- property %>% select(-Regionname)

# The City Council and Distance variable
# Viewing the average, minimum and maximum selling prices by Council Area 
property %>% 
        group_by(CityCouncil) %>%
        summarise(Count = n(),
                  Avg_Price = mean(Price),
                  Proximity = min(Distance), 
                  Min = min(Price),
                  Max = max(Price)) %>%
        arrange(desc(Count)) %>%
        head(10)

# Average Prices by Council Area's vs Relative distance to the city center
property %>%
        group_by(CityCouncil) %>%
        summarise(Count = n(),
                  Avg_Price = mean(Price),
                  Proximity = min(Distance)) %>%
        ggplot(aes(Avg_Price, reorder(CityCouncil,-Proximity),label = Proximity)) +
        geom_col(width = 0.75) +
        geom_text(position = position_dodge(width = .9),size = 3,hjust = -0.5) +
        geom_vline(xintercept = mean(property$Price),
                   lty = 2, color= "red") +
        xlab("Average Sales Price") +
        ylab("Council Areas") +
        ggtitle("Average Price VS Proximity to City Centre") 

# Calculate the average properties sold per council area
avg_properties_council <- property %>% 
                                group_by(CityCouncil) %>%
                                summarise(Count = n()) %>%
                                summarise(Avg = mean(Count)) %>%
                                pull(.)
avg_properties_council

# Number of properties sold by council area vs Proximity to the city center
property %>%
        group_by(CityCouncil) %>%
        summarise(Count = n(),
                  Avg_Price = mean(Price),
                  Proximity = min(Distance)) %>%
        ggplot(aes(Count, reorder(CityCouncil,-Proximity),label = Proximity)) +
        geom_col(width = 0.75) +
        geom_text(position = position_dodge(width = .9),
                  size = 3,hjust = -0.5) +
        geom_vline(xintercept = avg_properties_council,
                   lty = 2, color= "red") +
        xlab("Number of Properties Sold") +
        ylab("Council Areas") +
        ggtitle("Properties sold VS Proximity to City Centre") 

# Model Building
# Normalize the data in the data-set
property <- normalize(property,method = "range", range = c(0,1))  

# Creating the Test, Training and smaller Training subset
set.seed(10, sample.kind="Rounding")
test_index <- createDataPartition(y = property$Price, times = 1, p = 0.1, list = FALSE)
train <- property[-test_index,]
test <- property[test_index,]
train_subset <- train %>% sample_n(10000)

# Average model
# Prediction based on Mean Prices
mu <- mean(train$Price)
mu

# Calculating RMSE using the mean
mu_rmse <- RMSE(test$Price, mu)
mu_rmse

# Initializing a RMSE table to save the results
rmse_results <- tibble(Method = "Average Price Model", RMSE = mu_rmse)
rmse_results %>% knitr::kable()

# Suburb's Model
# Prediction based on the Suburbs 
suburb_avgs <- train %>%
                group_by(Suburb) %>%
                summarise(sub = mean(Price - mu))

predicted_pricing <- mu + test %>%
                        left_join(suburb_avgs, by='Suburb') %>%
                        pull(sub)

# Calculating RMSE using the suburbs bias 
rmse_model_suburb <- RMSE(predicted_pricing, test$Price)
rmse_model_suburb

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Suburbs Model",
                                RMSE = rmse_model_suburb))
rmse_results %>% knitr::kable()

# Property Type Model
# Prediction based on the Suburbs and Property Types
types_avgs <- train %>%
                left_join(suburb_avgs, by="Suburb") %>%
                group_by(Type) %>%
                summarise(type = mean(Price - mu - sub))

predicted_ratings <- test %>%
                        left_join(suburb_avgs, by="Suburb") %>%
                        left_join(types_avgs, by="Type") %>%
                        mutate(pred = mu + sub + type) %>%
                        pull(pred)

# Calculating RMSE using the suburbs and property types effects 
rmse_model_types <- RMSE(predicted_ratings, test$Price)
rmse_model_types

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method="Suburbs and Property Types Model",
                                 RMSE = rmse_model_types))
rmse_results %>% knitr::kable()

# Regularisation
# Calculating RMSE using multiple values of lambda.
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
                mu <- mean(train$Price)
                suburb_avgs <- train %>%
                group_by(Suburb) %>%
                summarise(sub = sum(Price - mu)/(n()+l))
                types_avgs <- train %>%
                left_join(suburb_avgs, by= "Suburb") %>%
                group_by(Type) %>%
                summarise(type = sum(Price - sub - mu)/(n()+l))
                predicted_ratings <- test %>%
                left_join(suburb_avgs, by = "Suburb") %>%
                left_join(types_avgs, by = "Type") %>%
                mutate(pred = mu + sub + type) %>%
                pull(pred)
        
        return(RMSE(predicted_ratings, test$Price))
})

rmse_regularisation <- min(rmses)
rmse_regularisation

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method="Regularised Suburbs and Property Types Model",
                                         RMSE = rmse_regularisation))
rmse_results %>% knitr::kable()

# Linear Regression Model
# Assigning the cross validating number
control <- trainControl(method = "cv", number = 10)

# Fitting and Predicting using Linear Regression
fit_lm <- train(Price ~ ., method = "lm", data = train, trControl = control)
predictions_lm <- predict(fit_lm, test)
lm_rmse <- RMSE(test$Price, predictions_lm)
lm_rmse

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method="Linear Regression Model",
                                     RMSE = lm_rmse))
rmse_results %>% knitr::kable()

# Regression Tree model
# Plotting different values of the cp parameter and their corresponding rmse values.
tune <- expand.grid(cp = seq(0, 0.0002, len = 10))

train_rt <- train(Price ~ ., method = "rpart", data = train_subset,
                  trControl = control, tuneGrid = tune)

# Identify and plot the best tune
train_rt$bestTune
ggplot(train_rt)

fit_rt <- rpart(Price ~ ., data = train, 
                control = rpart.control(cp = train_rt$bestTune))
predictions_rt <- predict(fit_rt, test)

rt_rmse <- RMSE(test$Price, predictions_rt)
rt_rmse

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method="Regresion Tree Model",
                                     RMSE = rt_rmse))
rmse_results %>% knitr::kable()

# Random Forest Model
# Using the train_subset to find the best tune for the model 
mtry <- round(ncol(train) / 3)
tune <- expand.grid(mtry = seq(mtry - 2, mtry + 2, len = 5))
train_rf <- train(Price ~ ., method = "rf", data = train_subset, 
                  ntree = 100, trControl = control, tuneGrid = tune)

ggplot(train_rf)

# Fitting the best tune to the training set
fit_rf <- randomForest(Price ~ ., data = train, 
                       minNode = train_rf$bestTune$mtry, ntree = 100)
predictions_rf <- predict(fit_rf, test)

rf_rmse <- RMSE(test$Price, predictions_rf)
rf_rmse

# Adding RMSE results to the Table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Method="Random Forest Model",
                                     RMSE = rf_rmse))
rmse_results %>% knitr::kable()
