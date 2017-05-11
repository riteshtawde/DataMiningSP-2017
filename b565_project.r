#Reference : https://www.kaggle.com/vicky83/d/harlfoxem/housesalesprediction/price-prediction-comparison/code#L20
# https://www.kaggle.com/auygur/d/harlfoxem/housesalesprediction/step-by-step-house-price-prediction-r-2-0-77
# https://www.kaggle.com/harlfoxem/d/harlfoxem/housesalesprediction/house-price-prediction-part-1
# https://www.kaggle.com/harlfoxem/d/harlfoxem/housesalesprediction/house-price-prediction-part-2
# using R-version: 3.4.0		 
# RStudio version: 1.0.143
# install.packages("GGally")
# install.packages("ltm")
# install.packages("ggplot2")
# install.packages("hydroGOF")
# install.packages("mvtnorm")
# install.packages("DMwR")
# install.packages("ppcor")
# install.packages("tidyr")
# install.packages("corrplot")
# install.packages("randomForest")

library(GGally)
library(ltm)
library(ggplot2)
library(hydroGOF)
library(mvtnorm)
library(DMwR)
library(ppcor)
library(tidyr)
library(corrplot)
library(randomForest)

House <- read.csv("kc_house_data.csv")

# checking for dirty data. If it contains NA at all
any(is.na(House)) # FALSE as data doesn't contain any NaN/NULL values
# print each feature with data type and certain values
str(House)
# we can remove the variable date as it ranges from May,2014 to May, 2015 and
# prices of houses are not affected through this time.
plot(House$date, House$price, xlab = "Date", ylab = "Price", main = "Price vs Date")
#House = subset(House, select = -c(2))
House = House[,-2]

#detecting and removing outliers
#using Local Outelier Factor (lof) algorithm
# data_for_outlier = House[,3:20]
# outlier_score = lofactor(data_for_outlier,k=100)
# plot(density(outlier_score))

#top 500 outliers
#outliers = order(outlier_score, decreasing=T)[1:500]
#House = House[-outliers,]

# ---------------------------------------------------------------------------- #
# CONTINUOUS VARIABLES
# identify continuous variables and
# Calculate the correlation between continuous variables and price
continuous_variables = c("sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "yr_built", "yr_renovated", "zipcode","lat","long","sqft_living15","sqft_lot15")
for(i in 1:length(continuous_variables)) {
  cat("\nCorrelation between price and", continuous_variables[i], "=", cor(House["price"], House[continuous_variables[i]], method="pearson"), append = TRUE)
}

# Correlation between price and sqft_living = 0.7019173
# Correlation between price and sqft_lot = 0.08987622
# Correlation between price and sqft_above = 0.6053679
# Correlation between price and sqft_basement = 0.3237989
# Correlation between price and yr_built = 0.05395333
# Correlation between price and yr_renovated = 0.1264236
# Correlation between price and zipcode = -0.05340243
# Correlation between price and lat = 0.3066923
# Correlation between price and long = 0.02203632
# Correlation between price and sqft_living15 = 0.5852412
# Correlation between price and sqft_lot15 = 0.08284493

# let us look at each plot for better visualization of relations
plot(House$sqft_living, House$price, main = "Price v/s SqFt Living", xlab = "Sqft Living", ylab="Price")
plot(House$sqft_lot, House$price, main = "Price v/s sqft_lot", xlab = "sqft_lot", ylab="Price")
plot(House$sqft_above, House$price, main = "Price v/s sqft_above", xlab = "sqft_above", ylab="Price")
plot(House$sqft_basement, House$price, main = "Price v/s sqft_basement", xlab = "sqft_basement", ylab="Price")
plot(House$yr_built, House$price, main = "Price v/s yr_built", xlab = "yr_built", ylab="Price")
plot(House$yr_renovated, House$price, main = "Price v/s yr_renovated", xlab = "yr_renovated", ylab="Price")
plot(House$zipcode, House$price, main = "Price v/s zipcode", xlab = "zipcode", ylab="Price")
plot(House$lat, House$price, main = "Price v/s latitude", xlab = "latitude", ylab="Price")
plot(House$long, House$price, main = "Price v/s long", xlab = "longitude", ylab="Price")
plot(House$sqft_living15, House$price, main = "Price v/s sqft_living15", xlab = "sqft_living15", ylab="Price")
plot(House$sqft_lot15, House$price, main = "Price v/s sqft_lot15", xlab = "sqft_lot15", ylab="Price")
# sqft_lot, sqft_lot15 and yr_built look to be poorly related to price

# yr_renovated and sqft_basement and moderately correlated to price
# sqft_basement and yr_renovated consists of a lot of zeros
# thus it must be interesting to analyse these variables as dichotomous variables
has_basement <- rep(0, nrow(House))
is_renovated <- rep(0, nrow(House))

for(i in 1:nrow(House)) {
  if(House$sqft_basement[i] == 0) {
    has_basement[i] = 0;
  } else {
    has_basement[i] = 1;
  }
  if(House$yr_renovated[i] == 0) {
    is_renovated[i] = 0;
  } else {
    is_renovated[i] = 1;
  }
}

House["has_basement"] <- has_basement
House["is_renovated"] <- is_renovated

# sqft_living15 is highly correlated to price and sqft_living15 and sqft_living are also correlated
cat("Correlation btw sqft_living and sqft_living15=", cor(House$sqft_living, House$sqft_living15), append = TRUE)
# Correlation btw sqft_living and sqft_living15= 0.75
# Let us compute Pearson's partial correlation test to assess the association between
# sqft_living15 and price while controlling sqft_living
part_corr <- pcor(cbind(House$price, House$sqft_living, House$sqft_living15))
cat("Pearson's Partial correlation test between price and sqft_living15=", part_corr$estimate[1,3], append = TRUE)
# thus average house size of the surrounding 15 houses has no effect on price of the house

# ---------------------------------------------------------------------------- #
# CATEGORICAL VARIABLES
# We need to identify types of variables i.e. whether categorical / numeric
# identify and print the columns for categorical values

cat("bedrooms = [", unique(sort(House$bedrooms)), "]", append = TRUE)
cat("bathrooms = [", unique(sort(House$bathrooms)), "]", append = TRUE)
cat("floors = [", unique(sort(House$floors)), "]", append = TRUE)
cat("waterfront = [", unique(sort(House$waterfront)), "]", append = TRUE)
cat("view = [", unique(sort(House$view)), "]", append = TRUE)
cat("condition = [", unique(sort(House$condition)), "]", append = TRUE)
cat("grade = [", unique(sort(House$grade)), "]", append = TRUE)
cat("has_basement = [", unique(sort(House$has_basement)), "]", append = TRUE)
cat("is_renovated = [", unique(sort(House$is_renovated)), "]", append = TRUE)


# Dichotomous variable - waterfront, has_basement, is_renovated
# Polytomous variable - bedrooms, bathrooms, floors, view, condition, grade

# Box plots between categorical variables and price to view the relationship
boxplot(price~waterfront, data=House, main="Price v/s Waterfront", col=c("red","blue"), xlab="View", ylab="Price") # Nice correlation
boxplot(price~has_basement, data=House, main="Price v/s Has Basement", col=c("red","blue"), xlab="Has Basement", ylab="Price") # Nice correlation
boxplot(price~is_renovated, data=House, main="Price v/s Is Renovated", col=c("red","blue"), xlab="Is Renovated", ylab="Price") # Nice correlation
boxplot(price~view, data=House, main="Price v/s View", col=c("red","blue"), xlab="View", ylab="Price") # Nice correlation
boxplot(price~grade, data=House, main="Price v/s Grade", col=c("red","blue"), xlab="Grade", ylab="Price") # Nice correlation
boxplot(price~condition, data=House, main="Price v/s Condition", col=c("red","blue"), xlab="Condition", ylab="Price") # Nice correlation
boxplot(price~bathrooms, data=House, main="Price v/s Bathrooms", col=c("red","blue"), xlab="Bathrooms", ylab="Price") # Nice correlation with outlier at bathroom=7
boxplot(price~bedrooms, data=House, main="Price v/s Bedrooms", col=c("red","blue"), xlab="Bedrooms", ylab="Price") # Nice correlation with outlier at bathroom=7
boxplot(price~floors, data=House, main="Price v/s Floors", col=c("red","blue"), xlab="Floors", ylab="Price") # Nice correlation with outlier at bathroom=7

# Calculate correlation between categorical variables and price
categorical_variables = c("bedrooms", "bathrooms", "floors", "view", "condition", "grade")
cat("Correlation between price and waterfront =", biserial.cor(House$price, House$waterfront), append = TRUE)
cat("Correlation between price and has_basement =", biserial.cor(House$price, House$has_basement), append = TRUE)
cat("Correlation between price and is_renovated =", biserial.cor(House$price, House$is_renovated), append = TRUE)
for(i in 1:length(categorical_variables)) {
  cat("\nCorrelation between price and", categorical_variables[i], "=", cor(House["price"], House[categorical_variables[i]], method="spearman"), append = TRUE)
}

# Correlation between price and waterfront = -0.2663923
# Correlation between price and is_renovated = -0.126079
# Correlation between price and has_basement = -0.1800778
# Correlation between price and bedrooms = 0.344245
# Correlation between price and bathrooms = 0.4972984
# Correlation between price and floors = 0.3224824
# Correlation between price and view = 0.2939065
# Correlation between price and condition = 0.01799459
# Correlation between price and grade = 0.6581517

# Top 4 Highly correlated variables with price
# 1. sqft_living
# 2. grade
# 3. sqft_above
# 4. bathrooms

# ------------------------------------------------------------------------------- #
# Let us perform Simple Linear Regression between each of the top variables and price
# Split the data into training and testing
ratio = sample(1:nrow(House), size = 0.30*nrow(House))
Test = House[ratio,] #Test dataset 30% of total
Training = House[-ratio,] #Train dataset 70% of total

# Linear regression between price and sqft_living
linear_model_sqft_living<-lm(price~sqft_living, data = Training)
pred_hat_lm_sqft_living <- predict(linear_model_sqft_living, newdata = Test)
mse.lm.sqft_living <- mse(pred_hat_lm_sqft_living, Test$price)

# Linear regression between price and grade
linear_model_grade<-lm(price~grade, data = Training)
pred_hat_lm_grade <- predict(linear_model_grade, newdata = Test)
mse.lm.grade <- mse(pred_hat_lm_grade, Test$price)

# Linear regression between price and sqft_above
linear_model_sqft_above <- lm(price~sqft_above, data = Training)
pred_hat_lm_sqft_above <- predict(linear_model_sqft_above, newdata = Test)
mse.lm.sqft_above <- mse(pred_hat_lm_sqft_above, Test$price)

# Linear regression between price and bathrooms
linear_model_bathrooms <- lm(price~bathrooms, data = Training)
pred_hat_lm_bathrooms <- predict(linear_model_bathrooms, newdata = Test)
mse.lm.bathrooms <- mse(pred_hat_lm_bathrooms, Test$price)

cat("RMSE(Root Mean Squared Error) Linear Regression between sqft_living and price=", sqrt(mse.lm.sqft_living), append = TRUE)
cat("RMSE(Root Mean Squared Error) Linear Regression between grade and price=", sqrt(mse.lm.grade), append = TRUE)
cat("RMSE(Root Mean Squared Error) Linear Regression between sqft_above and price=", sqrt(mse.lm.sqft_above), append = TRUE)
cat("RMSE(Root Mean Squared Error) Linear Regression between bathrooms and price=", sqrt(mse.lm.bathrooms), append = TRUE)
plot(House$sqft_living, House$price, xlab = "Sqft_living", ylab="Price", main="Price vs Sqft Living")
abline(linear_model_sqft_living)
summary(linear_model_sqft_living)
# ------------------------------------------------------------------------------- #
# Multiple Regression on data correlated with pricing
plot(log(House$sqft_living), log(House$price), xlab = "log(sqft_living)", ylab="log(price)")
plot(House$grade, log(House$price), xlab = "grade", ylab="log(price)")
plot(House$sqft_above, log(House$price), xlab = "sqft_above", ylab="log(price)")
plot(House$bathrooms, log(House$price), xlab = "bathrooms", ylab="log(price)")
plot(log(House$bedrooms), log(House$price), xlab = "log(bedrooms)", ylab="log(price)")
plot(House$waterfront, log(House$price), xlab = "waterfront", ylab="log(price)")
plot(House$floors, log(House$price), xlab = "floors", ylab="log(price)")
plot(House$view, log(House$price), xlab = "View", ylab="log(price)")
plot(House$sqft_basement, log(House$price), xlab = "sqft_basement", ylab="log(price)")
plot(House$lat, log(House$price), xlab = "latitude", ylab="log(price)")

mult.lm <- lm(log(price)~log(sqft_living)+grade+sqft_above+bathrooms+log(bedrooms)+waterfront+floors+view+sqft_basement+lat, data=Training)
pred_hat_mult.lm <- predict(mult.lm, newdata = Test)
mse_mult.lm <- mse(pred_hat_mult.lm, Test$price)
cat("RMSE(Root Mean Squared Error) Multivariate Linear Regression =", sqrt(mse_mult.lm), append = TRUE)
cat("R squared Value Multivariate Linear Regression =", summary(mult.lm)$r.squared, append = TRUE)
summary(mult.lm)
result <- (cbind("ID"=Test$id,"Orginal Price"=Test$price,"New predicted price"=exp(pred_hat_mult.lm))) ##display the original price and predicted price
write.csv(x = result, file = "Predicted_values.csv")





################### RANDOM FOREST ####################




save_to_file <- function(filename){
  dev.copy(png, filename)
  dev.off()
}

increment_i <- function(val){
  val = val+1
  return(val)
}

ntree_vect = c("10","100","100_all","100_no_view_bed","250","500","1000")
rmse_vect = c(0,0,0,0,0,0,0)
i = 1

M <- cor(House[,c(1:20)]) 
cor_plot = corrplot(M, method = "circle") #plot matrix
save_to_file("price_all_cor.png")

#plotting top 7 highly co-related features with price
price_view_box = boxplot(House$price~House$view, data = House, ylab="Price", xlab = "View", col=(c("#19eaa9","#6a8778")))
save_to_file("price_view_box.png")


price_bath_box = boxplot(House$price~House$bathrooms, data = House, ylab="Price", xlab = "Bathrooms", col=(c("#19eaa9","#6a8778")))
save_to_file("price_bath_box.png")

price_living15_box = boxplot(House$price~House$sqft_living15, data = House, ylab="Price", xlab = "Sqft_Living15", col=(c("#19eaa9","#6a8778")))
save_to_file("price_living15_box.png")

price_above_box = boxplot(House$price~House$sqft_above, data = House, ylab="Price", xlab = "Sqft_Above", col=(c("#19eaa9","#6a8778")))
save_to_file("price_above_box.png")

price_grade_box = boxplot(House$price~House$grade, data = House, ylab="Price", xlab = "Grade", col=(c("#19eaa9","#6a8778")))
save_to_file("price_grade_box.png")

price_living_box = boxplot(House$price~House$sqft_living, data = House, ylab="Price", xlab = "Sqft_Living", col=(c("#19eaa9","#6a8778")))
save_to_file("price_living_box.png")

price_bed_box = boxplot(House$price~House$bedrooms, data = House, ylab="Price", xlab = "Bedrooms", col=(c("#19eaa9","#6a8778")))
save_to_file("price_bed_box.png")

#splitting the dataset
#using 70% of the data for training and remaining 30% for testing
data_part = sample(1:nrow(House), size=0.7*nrow(House))

training_set = House[data_part,]
test_set = House[-data_part,]



#RANDOM FORESTS#
#using id and price data in prediction
predicted_vals_set <- test_set[,c(1,2)]

print("Predicting with ntree = 10...")
#ntree=10
rf_model = randomForest(x = training_set[,c(3,4,5,9,11,12,19)],
                         y = training_set$price,
                         ntree = 10)
predicted_val = predict(rf_model, test_set[,c(3,4,5,9,11,12,19)])
#rmse
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_10 <- predicted_val



print("Predicting with ntree = 100...")
#ntree=100
rf_model = randomForest(x = training_set[,c(3,4,5,9,11,12,19)],
                         y = training_set$price,
                         ntree = 100)
predicted_val = predict(rf_model, test_set[,c(3,4,5,9,11,12,19)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_100 <- predicted_val



print("Predicting with ntree = 100 with all columns...")
#ntree=100
rf_model = randomForest(x = training_set[,c(3,20)],
                         y = training_set$price,
                         ntree = 100)
predicted_val = predict(rf_model, test_set[,c(3,20)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_100_all <- predicted_val



print("Predicting with ntree = 100 no view and bedrooms...")
#ntree=100
rf_model = randomForest(x = training_set[,c(4,5,11,12,19)],
                         y = training_set$price,
                         ntree = 100)
predicted_val = predict(rf_model, test_set[,c(4,5,11,12,19)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_100_no_view_bed <- predicted_val



print("Predicting with ntree = 250...")
#ntree=250
rf_model = randomForest(x = training_set[,c(3,4,5,9,11,12,19)],
                         y = training_set$price,
                         ntree = 250)
predicted_val = predict(rf_model, test_set[,c(3,4,5,9,11,12,19)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_250 <- predicted_val



print("Predicting with ntree = 500...")
#ntree=500
rf_model = randomForest(x = training_set[,c(3,4,5,9,11,12,19)],
                         y = training_set$price,
                         ntree = 500)
predicted_val = predict(rf_model, test_set[,c(3,4,5,9,11,12,19)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_500 <- predicted_val



print("Predicting with ntree = 1000...")
#ntree=1000
rf_model = randomForest(x = training_set[,c(3,4,5,9,11,12,19)],
                         y = training_set$price,
                         ntree = 1000)
predicted_val = predict(rf_model, test_set[,c(3,4,5,9,11,12,19)])
i = increment_i(i)
rmse_vect[i] = sqrt(mse(predicted_val, test_set$price))
predicted_vals_set$ntree_1000 <- predicted_val

#combine table depicting the root mean sqaured error observed for each of the model
combined_table = cbind(ntree_vect, rmse_vect)
print(combined_table)

total_predict <- data.frame(actual = test_set$price,
                            rf_10 = predicted_vals_set$ntree_10,
                            rf_100 = predicted_vals_set$ntree_100,
                            rf_100_all = predicted_vals_set$ntree_100_all,
                            rf_100_no_view_bed = predicted_vals_set$ntree_100_no_view_bed,
                            rf_250 = predicted_vals_set$ntree_250,
                            rf_500 = predicted_vals_set$ntree_500,
                            rf_1000 = predicted_vals_set$ntree_1000
)

#flatten all the prediction columns above into a single row 
total_predict <- gather(total_predict,key = model,value = predictions,2:8)

#plotting predictions with each of the above model observed to visaulize the best fit
rf_plot = ggplot(data=total_predict,aes(x = actual, y = predictions)) + 
  geom_point(colour = "#5c9ffb") + 
  geom_abline(intercept = 0, slope = 1, colour = "#5c5c5c") +
  geom_vline(xintercept = 23, colour = "#478903", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  ggtitle("Predicted vs. Actual")

ggsave(filename = "random_forest_predictions.png", plot = rf_plot)
#writing prediction results to csv file
write.csv(total_predict, "random_forest_predictions.csv")