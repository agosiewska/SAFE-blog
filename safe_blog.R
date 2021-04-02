library(ranger)
library(DALEX)
library(rSAFE)


train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

train_x <- train[, c("bedrooms", "bathrooms", "floors", "waterfront", "view", "condition", "grade", "lat", "long", "age", "m2_living", "m2_lot", "m2_basement", "price_log")]
test_x <- test[, c("bedrooms", "bathrooms", "floors", "waterfront", "view", "condition", "grade", "lat", "long", "age", "m2_living", "m2_lot", "m2_basement", "price_log")]


### Ranger model ###
set.seed(123)
model <- ranger(price_log~., data = train_x)

pred_train <- predict(model, train_x)
mltools::rmse(exp(train_x$price_log), exp(pred_train$predictions))
# 76957.27
pred_test <- predict(model, test_x)
mltools::rmse(exp(test_x$price_log), exp(pred_test$predictions))
# 129575.1

### log scale
pred_train <- predict(model, train_x)
mltools::rmse(train_x$price_log, pred_train$predictions)
# 0.08907647
pred_test <- predict(model, test_x)
mltools::rmse(test_x$price_log, pred_test$predictions)
# 0.1746403

### Linear model ###
model_lm1 <- glm(price_log~., data = train_x)
pred_train <- predict(model_lm1, train_x)
mltools::rmse(exp(train_x$price_log), exp(pred_train))
# 218066.1
pred_test <- predict(model_lm1, test_x)
mltools::rmse(exp(test_x$price_log), exp(pred_test))
# 279616.7


### log scale

pred_train <- predict(model_lm1, train_x)
mltools::rmse((train_x$price_log), (pred_train))
# 0.2572089
pred_test <- predict(model_lm1, test_x)
mltools::rmse((test_x$price_log), (pred_test))
# 0.2576455


# SAFE Transformations

explain_model <- explain(model,
                         data = train_x,
                         y = exp(train_x$price_log),
                         predict_function = function (x, y)
                           exp(yhat(x, y)))
set.seed(123)
safe <- safe_extraction(explain_model, response_type = "pdp")

train_trans <- safely_transform_data(safe, train_x[,-14])
train_trans_new <- train_trans[,grepl(".*new",colnames(train_trans))]
train_trans_new$waterfront <- train_x$waterfront
train_trans_new$price_log <- train_x$price_log

test_trans <- safely_transform_data(safe, test_x[,-14])
test_trans_new <- test_trans[,grepl(".*new",colnames(test_trans))]
test_trans_new$waterfront <- test_x$waterfront
test_trans_new$price_log <- test_x$price_log


model_lm <- glm(price_log~., data = train_trans_new)
pred_train <- predict(model_lm, train_trans_new[,-14])
mltools::rmse(exp(train_trans_new$price_log), exp(pred_train))
# 203636.3
pred_test <- predict(model_lm, test_trans_new[,-14])
mltools::rmse(exp(test_trans_new$price_log), exp(pred_test))
# 182619.7

## log scale
pred_train <- predict(model_lm, train_trans_new[,-14])
mltools::rmse((train_trans_new$price_log), (pred_train))
# 0.2487476

pred_test <- predict(model_lm, test_trans_new[,-14])
mltools::rmse((test_trans_new$price_log), (pred_test))
# 0.2451518


variables = colnames(train_trans_new)

i <- 1
plot(safe, variable = gsub("_new", "", variables[i]))
i <- i + 1

variables[i]
