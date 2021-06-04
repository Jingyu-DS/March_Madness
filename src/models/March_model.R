library(data.table)
library(tidyverse)
library(glmnet)
library(plotmo)

test <- fread("./project/volume/data/interim/test.csv")
train <- fread("./project/volume/data/interim/train.csv")

# subset out only the columns to model

drops<- c('team_1','team_2')

train_no_id <- train[, !drops, with = FALSE]
test_no_id <- test[, !drops, with = FALSE]

# Set the train result 
train_y <- train$result

# Cross Validation
train_matrix <- as.matrix(train_no_id[, !c('result'), with = FALSE])
test_matrix <- as.matrix(test_no_id[, !c('result'), with = FALSE])

# gl_model <- cv.glmnet(train_matrix, train_y, alpha = 0.5,family="binomial")
gl_model <- cv.glmnet(train_matrix, train_y, alpha = 0, family="binomial")
bestlam <- gl_model$lambda.min

# alphalist <- seq(0,1,by=0.1)
# elasticnet <- lapply(alphalist, function(a){
#  cv.glmnet(train_matrix, train_y, alpha=a, family="binomial", lambda.min.ratio=.001)
#})
# for (i in 1:11) {print(min(elasticnet[[i]]$cvm))}


# gl_model <- glmnet(train_matrix, train_y, alpha = 0.5,family="binomial")
gl_model <- glmnet(train_matrix, train_y, alpha = 0, family="binomial")

plot_glmnet(gl_model)

pred_coe <- predict(gl_model,s = bestlam, newx = test_matrix, type = "coefficient")


pred <- predict(gl_model,s = bestlam, newx = test_matrix, type = "response")
test$result <- pred

sample <- fread("./project/volume/data/raw/MSampleSubmissionStage2.csv")
test$id <- paste("2021", test$team_1, test$team_2, sep="_")

test_final <- test[,.(id, result)]

setnames(test_final, c("id"), c("ID"))

sample_test <- sample %>% left_join(test_final, by = "ID") %>% select(ID, result)

setnames(sample_test,c("result"), c("Pred"))

write.csv(sample_test, "./project/volume/data/processed/new4_submission.csv",row.names = FALSE)
