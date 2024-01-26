## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----cars-------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
library(tree)
library(class)
library(gbm)
set.seed(1)


## ----pressure, echo=FALSE---------------------------------------------------------------------------------------------------------------------
# reading the training independent variables
train_x <- read_csv("airbnb_train_x_2023.csv")
# reading training dependent variable
train_y <- read_csv("airbnb_train_y_2023.csv")
# combining both dependent and independent variables in to one dataframe
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score)) 
# reading external data
housing_data <- read_csv("Housing.csv")

## ---------------------------------------------------------------------------------------------------------------------------------------------
check <- table(train$perfect_rating_score, train$bedroom)
prop.table(check, 2)
barplot(check, legend.text = rownames(check),
        main = "Perfect Rating Score by Accommodates (Before cleaning)",
        xlab = "Accommodates",
        ylab = "Frequency")


## ---------------------------------------------------------------------------------------------------------------------------------------------
# defining a function for cleaning the data
data_cleaning_func <- function(data, external_data){
  # new feature, counting the number of amenities
  data <- data %>%
  mutate(
    amenities_count = str_count(data$amenities, ",")
  )
  
  # converting amenities to a dummy variable
  data$amenities = gsub("\\{", "", data$amenities)
  data$amenities = gsub("\\}", "", data$amenities)
  # splitting based on ","
  amenities.split <- strsplit(data$amenities, ",")
  lev <- unique(unlist(amenities.split))
  amenities.dummy <- (lapply(amenities.split, function(x) table(factor(x, levels=lev))))
  data_new <- with(data, data.frame(access, do.call(rbind, amenities.dummy), accommodates))
  data_new <- subset(data_new, select = -c(access,accommodates))
  
  data <- cbind(data, data_new)
  
  # converting price related columns into integer
  data$cleaning_fee = gsub("\\$", "", data$cleaning_fee)
  data$price = gsub("\\$", "", data$price)
  data$cleaning_fee <- parse_number(data$cleaning_fee)
  data$price <- parse_number(data$price)
  data$extra_people = gsub("\\$", "", data$extra_people)
  data$extra_people <- parse_number(data$extra_people)
  data$security_deposit = gsub("\\$", "", data$security_deposit)
  data$security_deposit <- parse_number(data$security_deposit)
  data$weekly_price = gsub("\\$", "", data$weekly_price)
  data$weekly_price <- parse_number(data$weekly_price)
  data$monthly_price = gsub("\\$", "", data$monthly_price)
  data$monthly_price <- parse_number(data$monthly_price)

  data <- data %>%
      mutate( 
        # grouping categories to even spread the data
        cancellation_policy = as.factor(ifelse(cancellation_policy == "super_strict_30", "strict", (ifelse(cancellation_policy == "super_strict_60", "strict", (ifelse(cancellation_policy == "no_refunds", "strict", cancellation_policy)))))),
        cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
        price = ifelse(is.na(price), mean(price, na.rm=TRUE), price),
        host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm=TRUE), host_listings_count),
        security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm=TRUE), security_deposit),
        accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm=TRUE), accommodates),
        bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms),
        # new feature, check the availability for next 30, 60, 90 and 365 days
        is_available = as.factor(ifelse(availability_30 == 0, "NO", "YES")),
        is_available_60 = as.factor(ifelse(availability_60 == 0, "NO", "YES")),
        is_available_90 = as.factor(ifelse(availability_90 == 0, "NO", "YES")),
        is_available_365 = as.factor(ifelse(availability_365 == 0, "NO", "YES")),
        # new feature, a flag which is true when there are no specific rules from the host
        no_rules = as.factor(ifelse(is.na(house_rules), "NO", "YES")),
        beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds),
        host_is_superhost = ifelse(is.na(host_is_superhost), FALSE, host_is_superhost),
        host_identity_verified = ifelse(is.na(host_identity_verified), FALSE, host_identity_verified),
        zipcode = as.factor(zipcode),
        smart_location = as.factor(smart_location),
        state = as.factor(state),
        # new feature, price per day when the listing is booked for a week
        per_day_price_weekly = (weekly_price / 7),
        # new feature, price per day when the listing is booked for a month
        per_day_price_monthly = (monthly_price / 30),
        host_total_listings_count = ifelse(is.na(host_total_listings_count), median(host_total_listings_count), host_total_listings_count),
        # new feature, a flag which is true when host information is available
        host_info_available = as.factor(ifelse(is.na(host_about), "NO", "YES")),
        # limiting number of guests to 5 to avoid outliers
        guests_included = ifelse(guests_included > 4, 5, guests_included)
      )
  
  data <- data %>%
    mutate(
      # new feature, price per person
      price_per_person = price/accommodates,
      # new feature, flag which is true when the cleaning fee is present
      has_cleaning_fee = as.factor(ifelse(cleaning_fee == 0, "NO", "YES")),
      # new feature, flag which turns true when the deposit is present
      has_deposit = as.factor(ifelse(security_deposit == 0, "NO", "YES")),
      # new feature, flag which turns true when host charges extra fee for extra accommodates
      has_extra_fee = as.factor(ifelse(extra_people == 0, "NO", "YES")),
      bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
      property_category = as.factor(ifelse(property_type == "Apartment" | property_type == "Serviced apartment" | property_type == "Loft" | is.na(property_type) , "apartment", ifelse(
                     property_type == "Bed & Breakfast" | property_type == "Boutique hotel" | property_type == "Hostel", "hotel", ifelse(
                       property_type == "Townhouse" | property_type == "Condominium", "condo", ifelse(property_type == "Bungalow" | property_type == "House", "house", "apartment")))))
    )
  
  data <- data %>%
    # new feature, an index value based on the property_type
    group_by(property_category) %>%
    mutate(median_value = median(price_per_person, na.rm = TRUE)) %>%
    ungroup()  %>%
    mutate(ppp_ind = as.factor(ifelse(price_per_person >  median_value, 1, 0)))
  
  data <- data %>%
    mutate(
    room_type = as.factor(room_type),
    bed_type = as.factor(bed_type),
    market = ifelse(is.na(market), "MISSING", market)
  )
  
#  data <- data %>%
#    group_by(as.factor(market)) %>%
#    mutate(market_count = n()) %>%
#    ungroup() %>%
#    mutate(market = as.factor(ifelse(market_count < 300, "OTHER", market)))
  data <- mutate(data, 
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
    host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate), "MISSING", ifelse(host_acceptance_rate == "100%", "ALL", "SOME"))),
    host_response = as.factor(ifelse(is.na(host_response_rate), "MISSING", 
    ifelse(host_response_rate == "100%", "ALL", "SOME"))),
    host_response_time = as.factor(ifelse(is.na(host_response_time), "MISSING", host_response_time)),
    # new feature, a categorical variable which depends on the minimum nights policy of the listing
    has_min_nights = as.factor(ifelse(minimum_nights < 2, "MIN", ifelse(minimum_nights < 4, "MOD", "HIGH"))),
    # new feature, a categorical variable which depends on the maximum nights policy of the listing
    has_max_nights = as.factor(ifelse(maximum_nights > 1, "YES", "NO")),
    # new feature, a flag which is true when the square feet data is available
    square_feet_data = as.factor(ifelse(is.na(square_feet), "YES", "NO"))
    )
  
  data <- data %>%
    mutate(
    # new feature, a categorical variables to categorize the prices of the listing 
    price_cat = as.factor(ifelse(price < 74, "LOW", ifelse(price < 161, "MOD", ifelse(price > 160, "HIGH", "LOW" )))),
    # new feature, a categorical variable to categorize the number of bathrooms
    bathrooms_cat = as.factor(ifelse(bathrooms < 1, "FEW","MANY")),
    # new feature, a flag which is "YES" when there is transit information available
    transit_available = as.factor(ifelse(is.na(transit), "YES", "NO")),
    # new feature, a flag which is "YES" when the listing is available for entire week
    weekly_available = as.factor(ifelse(is.na(weekly_price), "YES", "NO")),
    # new feature, a flag which is "YES" when the listing is available for entire month
    monthly_available = as.factor(ifelse(is.na(monthly_price), "YES", "NO")),
    # limiting number of market levels to have consistent levels with the testing dataset
    market = as.factor(ifelse(market == "New York", market, ifelse(market == "D.C.", market, ifelse(market == "Austin", market, ifelse(market == "Los Angeles", market, ifelse(market == "New Orleans", market, ifelse(market == "Boston", market, ifelse(market == "Chicago", market, ifelse(market == "Denver", market, ifelse(market == "San Diego", market, ifelse(market == "San Francisco", market,ifelse(market == "Nashville", market, ifelse(market == "Portland", market, ifelse(market == "Seattle", market,"Other"))))))))))))))
    )
  data <- data %>%
    mutate(
      accommodates = ifelse(accommodates > 9, 9, accommodates),
      bedrooms = ifelse(bedrooms > 6, 6, bedrooms),
      bedrooms = ifelse(bedrooms == 0, 1, bedrooms),
      bedrooms = ifelse(bedrooms >1 & bedrooms < 2, 2, bedrooms)
  )
  for(i in 1:nrow(data)) {
  if(is.na(data$square_feet[i]) | data$square_feet[i] == 0) {
    data$square_feet[i] <- sample(external_data$area[external_data$bedrooms == data$bedrooms[i]], size = 1)
  }
  }
  return(data)
}
 



## ---------------------------------------------------------------------------------------------------------------------------------------------
# cleaning the training data
train_cleaned_data <- data_cleaning_func(train, housing_data)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# visualizing few features
check <- table(train_cleaned_data$perfect_rating_score, train_cleaned_data$amenities_count)
prop.table(check, 2)
barplot(check, legend.text = rownames(check),
        main = "Perfect Rating Score by Accommodates (After cleaning)",
        xlab = "Accommodates",
        ylab = "Frequency")


## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
train_instn = sample(nrow(train_cleaned_data), 0.7*nrow(train_cleaned_data))


## ---------------------------------------------------------------------------------------------------------------------------------------------
training_data <- train_cleaned_data[train_instn,]
validation_data <- train_cleaned_data[-train_instn,]

# predicting "NO" for each listing
preds_baseline <- rep("NO", nrow(validation_data))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# testing the baseline model
# changing first prediction to "YES" to match the level of the validation data's perfect rating score
preds_baseline[1] = "YES" 

basline_CM = confusionMatrix(data = as.factor(preds_baseline),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Accuracy for the Basline model: ", basline_CM$overall["Accuracy"])
TPR <- as.numeric(basline_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(basline_CM$byClass["Specificity"])

print(paste("TPR: ", TPR))
print(paste("FPR: ", FPR))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# selecting features for the logistic model
data_features <- train_cleaned_data %>%
  select(accommodates, availability_365, bed_type, availability_30, availability_90, bathrooms, bedrooms, bedrooms, beds, cancellation_policy, cleaning_fee, extra_people, guests_included, host_listings_count, instant_bookable, ppp_ind, has_min_nights, host_is_superhost, host_identity_verified, has_extra_fee, host_response, room_type, is_location_exact, instant_bookable, is_available, no_rules, market, maximum_nights, minimum_nights, monthly_available, price, require_guest_phone_verification, require_guest_profile_picture, requires_license, weekly_available, TV, X.Cable.TV., Internet, X.Wireless.Internet., X.Air.conditioning., Kitchen, X.Free.parking.on.premises., Heating, X.Family.kid.friendly., X.Family.Kid.Friendly., Washer, X.Carbon.Monoxide.Detector., Essentials, Shampoo, Hangers, X.Hair.dryer., Iron, X.Laptop.friendly.workspace., X.Self.Check.In., Keypad, X.Safety.card., X.Hot.water., X.Cooking.basics., X.Lock.on.bedroom.door., Gym, X.Pets.allowed.,transit_available, square_feet, perfect_rating_score)

# creating training and validation data
training_data <- data_features[train_instn,]
validation_data <- data_features[-train_instn,]


## ---------------------------------------------------------------------------------------------------------------------------------------------
# training the logistic model
log_model <- glm(perfect_rating_score ~. , data = training_data, family = "binomial")
summary(log_model)            


## ---------------------------------------------------------------------------------------------------------------------------------------------
##Testing the logistic model:

#training performance: 
prob_log <- predict(log_model, newdata = training_data, type = "response")
preds_log <- ifelse(prob_log > 0.49, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(preds_log),
                     reference = as.factor(training_data$perfect_rating_score),
                     positive="YES")

cat("Training Accuracy for the logistic model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Training TPR: ", TPR))
print(paste("Training FPR: ", FPR))



# generalization performance
prob_log <- predict(log_model, newdata = validation_data, type = "response")
preds_log <- ifelse(prob_log > 0.49, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(preds_log),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Validation Accuracy for the logistic model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Validation TPR: ", TPR))
print(paste("Validation FPR: ", FPR))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# selecting features for the models
data_features <- train_cleaned_data %>%
  select(accommodates, availability_365, bed_type, availability_30, availability_90, bathrooms, bedrooms, bedrooms, beds, cancellation_policy, cleaning_fee, extra_people, guests_included, host_listings_count, instant_bookable, ppp_ind, has_min_nights, host_is_superhost, host_identity_verified, has_extra_fee, host_response, room_type, is_location_exact, instant_bookable, is_available, no_rules, market, maximum_nights, minimum_nights, monthly_available, price, require_guest_phone_verification, require_guest_profile_picture, requires_license, weekly_available, TV, X.Cable.TV., Internet, X.Wireless.Internet., X.Air.conditioning., Kitchen, X.Free.parking.on.premises., Heating, X.Family.kid.friendly., X.Family.Kid.Friendly., Washer, X.Carbon.Monoxide.Detector., Essentials, Shampoo, Hangers, X.Hair.dryer., Iron, X.Laptop.friendly.workspace., X.Self.Check.In., Keypad, X.Safety.card., X.Hot.water., X.Cooking.basics., X.Lock.on.bedroom.door., Gym, X.Pets.allowed.,transit_available, square_feet, perfect_rating_score)

# creating training and validation data
training_data <- data_features[train_instn,]
validation_data <- data_features[-train_instn,]


## ---------------------------------------------------------------------------------------------------------------------------------------------
# creating dummy variables for the ridge and lasso models
dummy <- dummyVars( ~ . , data=training_data, fullRank = TRUE)
training_dummy <- predict(dummy, newdata =training_data)
# remove the target variable from the matrix of features
train_x <- training_dummy[, !(colnames(training_dummy) %in% c("perfect_rating_score.YES"))]
# storing dependent variable (factor)
train_y <- training_data$perfect_rating_score

# repeating the same steps for validation data
val_dummy <- predict(dummy, newdata =validation_data)
# remove the target variable from the matrix of features
valid_x <- val_dummy[, !(colnames(val_dummy) %in% c("perfect_rating_score.YES"))]
# storing dependent variable (factor)
valid_y <- validation_data$perfect_rating_score


## ---------------------------------------------------------------------------------------------------------------------------------------------

glm.out.ridge <- glmnet(train_x, train_y, alpha = 0, family="binomial")
glm.out.lasso <- glmnet(train_x, train_y, alpha = 1, family="binomial")



## ---------------------------------------------------------------------------------------------------------------------------------------------
# defining the accuracy function
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}
grid <- 10^seq(-1,-4,length=100)

# setting alpha to 0 for ridge and 1 for lasso
my_alpha = 0
#storage vector
accs <- rep(0, length(grid))

for(i in c(1:length(grid))){
  lam = grid[i] #current value of lambda

  #train a ridge model with lambda = lam
  glmout <- glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = lam)
  
  #make predictions as usual
  preds <- predict(glmout, newx = valid_x, type = "response")
  
  #classify and compute accuracy
  classifications <- ifelse(preds > .47, "YES", "NO")
  inner_acc <- accuracy(classifications, valid_y)
  accs[i] <- inner_acc
}

#plot fitting curve - easier to read if we plot logs
plot(log10(grid), accs)

# get best-performing lambda
best_validation_index <- which.max(accs)
best_lambda <- grid[best_validation_index]

best_lambda
#print coefficients for best lambda
#coef(glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = best_lambda))



## ---------------------------------------------------------------------------------------------------------------------------------------------
# using the best lambda above, training the ridge model
glmout <- glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = best_lambda)
  
# testing the ridge model:

#Training performance:
preds <- predict(glmout, newx = train_x, type = "response")
log_classifications <- ifelse(preds > 0.495, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(log_classifications),
                     reference = as.factor(training_data$perfect_rating_score),
                     positive="YES")

cat("Training Accuracy for the Ridge model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Training TPR for ridge model: ", TPR))
print(paste("Training FPR for ridge model: ", FPR))



#Generalization performance:
preds <- predict(glmout, newx = valid_x, type = "response")
log_classifications <- ifelse(preds > 0.495, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(log_classifications),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Generalization Accuracy for the Ridge model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Generalization TPR for Ridge model: ", TPR))
print(paste("Generalization FPR for Ridge model: ", FPR))
best_lambda



## ---------------------------------------------------------------------------------------------------------------------------------------------
#print coefficients for best lambda for Ridge model
coef(glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = best_lambda))



## ---------------------------------------------------------------------------------------------------------------------------------------------
# defining the accuracy function
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}
grid <- 10^seq(-1,-4,length=100)

# setting alpha to 0 for ridge and 1 for lasso
my_alpha = 1
#storage vector
accs <- rep(0, length(grid))

for(i in c(1:length(grid))){
  lam = grid[i] #current value of lambda

  #train a ridge model with lambda = lam
  glmout <- glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = lam)
  
  #make predictions as usual
  preds <- predict(glmout, newx = valid_x, type = "response")
  
  #classify and compute accuracy
  classifications <- ifelse(preds > .47, "YES", "NO")
  inner_acc <- accuracy(classifications, valid_y)
  accs[i] <- inner_acc
}

#plot fitting curve - easier to read if we plot logs
plot(log10(grid), accs)

# get best-performing lambda
best_validation_index <- which.max(accs)
best_lambda <- grid[best_validation_index]

best_lambda


## ---------------------------------------------------------------------------------------------------------------------------------------------
#print coefficients for best lambda for Lasso model
coef(glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = best_lambda))



## ---------------------------------------------------------------------------------------------------------------------------------------------
# using the best lambda above, training the lasso model
glmout <- glmnet(train_x, train_y, family = "binomial", alpha = my_alpha, lambda = best_lambda)
  
# testing the lasso model:
#Training performance:
preds <- predict(glmout, newx = train_x, type = "response")
log_classifications <- ifelse(preds > 0.495, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(log_classifications),
                     reference = as.factor(training_data$perfect_rating_score),
                     positive="YES")

cat("Training Accuracy for the Lasso model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Training TPR for Lasso model: ", TPR))
print(paste("Training FPR for Lasso model: ", FPR))



#Generalization performance:
preds <- predict(glmout, newx = valid_x, type = "response")
log_classifications <- ifelse(preds > 0.495, "YES" , "NO")
log_CM = confusionMatrix(data = as.factor(log_classifications),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Generalization Accuracy for the Lasso model: ", log_CM$overall["Accuracy"])
TPR <- as.numeric(log_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(log_CM$byClass["Specificity"])

print(paste("Generalization TPR for Lasso model: ", TPR))
print(paste("Generalization FPR for the Lasso model: ", FPR))


## ---------------------------------------------------------------------------------------------------------------------------------------------
training_data_x <- training_data[, !(names(training_data) %in% c("perfect_rating_score"))]
validation_data_x <- validation_data[, !(names(validation_data) %in% c("perfect_rating_score"))]


## ---------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
rf.mod <- randomForest(training_data$perfect_rating_score~.,
                       data=training_data_x,
                       mtry=60, ntree=500,
                       importance=TRUE) 



## ---------------------------------------------------------------------------------------------------------------------------------------------

#testing the model:

#Training performance:
prob_random_forest <- predict(rf.mod, newdata=training_data_x, "prob")
preds_random_forest <- ifelse(prob_random_forest[,2] > 0.5, "YES" , "NO")

rf_CM = confusionMatrix(data = as.factor(preds_random_forest),
                     reference = as.factor(training_data$perfect_rating_score),
                     positive="YES")

cat("Training accuracy for the Random Forest model: ", rf_CM$overall["Accuracy"])
TPR <- as.numeric(rf_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(rf_CM$byClass["Specificity"])

print(paste("Training TPR for the random forest model: ", TPR))
print(paste("Training FPR for the random forest model: ", FPR))



#Generalization performance:
prob_random_forest <- predict(rf.mod, newdata=validation_data_x, "prob")
preds_random_forest <- ifelse(prob_random_forest[,2] > 0.5, "YES" , "NO")

rf_CM = confusionMatrix(data = as.factor(preds_random_forest),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Validation Accuracy for the Random Forest model: ", rf_CM$overall["Accuracy"])
TPR <- as.numeric(rf_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(rf_CM$byClass["Specificity"])

print(paste("Validation TPR for the Random forest model : ", TPR))
print(paste("Validation FPR for the Random forest model: ", FPR))


## ---------------------------------------------------------------------------------------------------------------------------------------------
#Finding important variables in RandomForest:

importance(rf.mod)
varImpPlot(rf.mod)



## ---------------------------------------------------------------------------------------------------------------------------------------------
rf.mod <- randomForest(training_data$perfect_rating_score~.,
                       data=training_data_x,
                       mtry=10, ntree=1000,
                       importance=TRUE) 



## ---------------------------------------------------------------------------------------------------------------------------------------------
prob_random_forest <- predict(rf.mod, newdata=validation_data_x, "prob")
# change this cutoff as well
preds_random_forest <- ifelse(prob_random_forest[,2] > 0.85, "YES" , "NO")

rf_CM = confusionMatrix(data = as.factor(preds_random_forest),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Accuracy for the Random Forest model: ", rf_CM$overall["Accuracy"])
TPR <- as.numeric(rf_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(rf_CM$byClass["Specificity"])

print(paste("TPR: ", TPR))
print(paste("FPR: ", FPR))



## ---------------------------------------------------------------------------------------------------------------------------------------------
rf.mod <- randomForest(training_data$perfect_rating_score~.,
                       data=training_data_x,
                       mtry=15, ntree=1000,
                       importance=TRUE) 



## ---------------------------------------------------------------------------------------------------------------------------------------------
prob_random_forest <- predict(rf.mod, newdata=validation_data_x, "prob")
# change this cutoff as well
preds_random_forest <- ifelse(prob_random_forest[,2] > 0.85, "YES" , "NO")

rf_CM = confusionMatrix(data = as.factor(preds_random_forest),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Accuracy for the Random Forest model: ", rf_CM$overall["Accuracy"])
TPR <- as.numeric(rf_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(rf_CM$byClass["Specificity"])

print(paste("TPR: ", TPR))
print(paste("FPR: ", FPR))



## ---------------------------------------------------------------------------------------------------------------------------------------------
mycontrol = tree.control(nrow(data.frame(train_x)),mincut = 5,  minsize = 10,mindev = 0.0005)
full_tree = tree(training_data$perfect_rating_score ~., control = mycontrol, data.frame(train_x))
summary(full_tree)


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Function to calculate the accuracy
tree_predict_classify <- function(predicting_dataset, tree_name, cutoff, dependant_variable){
  #make predictions in predicting_dataset (could be train, valid, etc.)
  predictions <- predict(tree_name, newdata = predicting_dataset)
  #extract P(Y = 1) (second column)
  probabilities <- predictions[,2]
  #classify using a cutoff
  classifications <- ifelse(probabilities > cutoff, "YES", "NO")
  #calculate and return accuracy
  acc <- accuracy(dependant_variable, classifications)
  return(acc)
}

tree_size <- c(2, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40)
va_acc <- rep(0, length(tree_size))
tr_acc <- rep(0, length(tree_size))

for(i in 1:length(tree_size)){
  pruned_tree_i = prune.tree(full_tree, best = tree_size[i]) 
  tr_acc[i] <- tree_predict_classify(data.frame(train_x), pruned_tree_i, 0.45, training_data$perfect_rating_score)
  va_acc[i] <- tree_predict_classify(data.frame(valid_x), pruned_tree_i, 0.45, validation_data$perfect_rating_score)
}

plot(tree_size, tr_acc, col = "blue", type = 'l')
#, ylim = c(0.7, 0.8)
lines(tree_size, va_acc, col = "red")
legend(5, 0.75, legend=c("Validation Accuracy", "Training Accuracy"), col=c("red","blue"), lty=1:1, cex=0.8)




## ---------------------------------------------------------------------------------------------------------------------------------------------
# testing the decision tree

#Training performance: 

best_tree = tree_size[which.max(tr_acc)]
pruned_tree_best = prune.tree(full_tree, best = best_tree) 
predictions <- predict(pruned_tree_best, newdata = data.frame(train_x))
prob_tree <- predictions[,2]
preds_tree <- ifelse(prob_tree > 0.5, "YES" , "NO")

tree_CM = confusionMatrix(data = as.factor(preds_tree),
                     reference = as.factor(training_data$perfect_rating_score),
                     positive="YES")

cat("Training Accuracy for the Decision Tree model: ", tree_CM$overall["Accuracy"])
TPR <- as.numeric(tree_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(tree_CM$byClass["Specificity"])

print(paste("Training TPR: ", TPR))
print(paste("Training FPR: ", FPR))



#Generalization performance
best_tree = tree_size[which.max(va_acc)]
pruned_tree_best = prune.tree(full_tree, best = best_tree) 
predictions <- predict(pruned_tree_best, newdata = data.frame(valid_x))
prob_tree <- predictions[,2]
preds_tree <- ifelse(prob_tree > 0.5, "YES" , "NO")

tree_CM = confusionMatrix(data = as.factor(preds_tree),
                     reference = as.factor(validation_data$perfect_rating_score),
                     positive="YES")

cat("Validation Accuracy for the Decision Tree model: ", tree_CM$overall["Accuracy"])
TPR <- as.numeric(tree_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(tree_CM$byClass["Specificity"])

print(paste("Validation TPR: ", TPR))
print(paste("Validation FPR: ", FPR))



## ---------------------------------------------------------------------------------------------------------------------------------------------
# Boosting

#needs a numerical target variable
train_y <- ifelse(training_data$perfect_rating_score=="YES",1,0)
valid_y <- ifelse(validation_data$perfect_rating_score=="YES",1,0)
#boost_train <- boost_data[train_inst,]
#boost_valid <- boost_data[-train_inst,]


boost.mod <- gbm(train_y~., data = data.frame(train_x),
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=4)

boost_preds <- predict(boost.mod,newdata=data.frame(valid_x),type='response',n.trees=1000)

#classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.4,1,0)
boost_acc <- mean(ifelse(boost_class==valid_y,1,0))
boost_acc

## ---------------------------------------------------------------------------------------------------------------------------------------------
#Important features in the boosting model:
summary.gbm(boost.mod)



## ---------------------------------------------------------------------------------------------------------------------------------------------
# testing the boosting model

# Training performance
boost_preds <- predict(boost.mod,newdata=data.frame(train_x),type='response',n.trees=1000)
boost_class <- ifelse(boost_preds>.49,1,0)
boost_CM = confusionMatrix(data = as.factor(boost_class),
                     reference = as.factor(train_y),
                     positive="1")
cat("Training Accuracy for the Boosting model: ", boost_CM$overall["Accuracy"])
TPR <- as.numeric(boost_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(boost_CM$byClass["Specificity"])

print(paste("Training TPR for Boosting model: ", TPR))
print(paste("Training FPR for Boosting model: ", FPR))


# Generalization performance
boost_class <- ifelse(boost_preds>.49,1,0)
boost_CM = confusionMatrix(data = as.factor(boost_class),
                     reference = as.factor(valid_y),
                     positive="1")
cat("Validation Accuracy for the Boosting Model: ", boost_CM$overall["Accuracy"])
TPR <- as.numeric(boost_CM$byClass["Sensitivity"])
FPR <- 1 - as.numeric(boost_CM$byClass["Specificity"])

print(paste("Validation TPR for the Boosting Model: ", TPR))
print(paste("Validation FPR for the Boosting Model: ", FPR))


## ---------------------------------------------------------------------------------------------------------------------------------------------
best_model = boost.mod


## ---------------------------------------------------------------------------------------------------------------------------------------------
# prediction on the testing data

test_x <- read_csv("airbnb_test_x_2023.csv")
testing_data <- data_cleaning_func(test_x)

# selecting the testing features
testing_data_features <- testing_data %>%
  select(accommodates, availability_365, bed_type, availability_30, availability_90, bathrooms, bedrooms, bedrooms, beds, cancellation_policy, cleaning_fee, extra_people, guests_included, host_listings_count, instant_bookable, ppp_ind, has_min_nights, host_is_superhost, host_identity_verified, has_extra_fee, host_response, room_type, is_location_exact, instant_bookable, is_available, no_rules, market, maximum_nights, minimum_nights, monthly_available, price, require_guest_phone_verification, require_guest_profile_picture, requires_license, weekly_available, TV, X.Cable.TV., Internet, X.Wireless.Internet., X.Air.conditioning., Kitchen, X.Free.parking.on.premises., Heating, X.Family.kid.friendly., X.Family.Kid.Friendly., Washer, X.Carbon.Monoxide.Detector., Essentials, Shampoo, Hangers, X.Hair.dryer., Iron, X.Laptop.friendly.workspace., X.Self.Check.In., Keypad, X.Safety.card., X.Hot.water., X.Cooking.basics., X.Lock.on.bedroom.door., Gym, X.Pets.allowed.,transit_available, square_feet)


final_prob <- predict(best_model, newdata = testing_data_features, type = "response")
final_classifications <- ifelse(final_prob > 0.52, "YES" , "NO")



## ---------------------------------------------------------------------------------------------------------------------------------------------
#saving the file
write.table(final_classifications, "perfect_rating_score_group22.csv", row.names = FALSE)



## ---------------------------------------------------------------------------------------------------------------------------------------------
# Further work to be done on KNN:
#KNN


# Function to calculate the knn accuracy
knn_accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}

kvec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 50, 100, 200)


#initialize storage
knn_va_acc <- rep(0, length(kvec))
knn_tr_acc <- rep(0, length(kvec))

#for loop
for(i in 1:length(kvec)){
  inner_tr_preds <- knn(data.frame(train_x), data.frame(train_x), training_data$perfect_rating_score, k=kvec[i], prob = TRUE) 
  knn_tr_acc[i] <- accuracy(inner_tr_preds, training_data$perfect_rating_score)
  inner_va_preds <- knn(data.frame(train_x), data.frame(valid_x), training_data$perfect_rating_score, k=kvec[i], prob = TRUE)
  knn_va_acc[i] <- accuracy(inner_va_preds, validation_data$perfect_rating_score)
}
plot(log(kvec), knn_tr_acc, col = "blue", type = 'l')
#ylim = c(0.6, 1))
lines(log(kvec), knn_va_acc, col = "red")
legend(3.5, 0.9, legend=c("Validation Accuracy", "Training Accuracy"), col=c("red","blue"), lty=1:1, cex=0.8)

