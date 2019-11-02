
############################################### Load packages and data #############################################

rm(list=ls()) 

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(purrr)
library(caret)
library(ranger)
library(xgboost)
library(kableExtra) # just to make the output nicer
library(mice)

theme_set(theme_bw()) 

df <- read.csv("pd_data_v2.csv", sep = ";", header = TRUE)

df_corp <- df

########################################## Set up the data ############################################################

str(df_corp)

df_corp$default <- as.factor(df_corp$default) 

df_corp %>% 
  ggplot(aes(x = default, fill = default)) +
  geom_bar()

# Look at the distribution and recategorize the factor classes

# Adverse_audit_opinion

table(df_corp$default, df_corp$adverse_audit_opinion)

df_corp$adverse_audit_opinion <- 
  ifelse(df_corp$adverse_audit_opinion == 0, yes = 0, no = 1)

table(df_corp$default, df_corp$adverse_audit_opinion) # If you got any Adverse audit opinons then its show as 1

# Industry

table(df_corp$default, df_corp$industry) # As we don't know anything about the type off industry we let it be

# Payment_reminders

table(df_corp$default, df_corp$payment_reminders) # Ok

# Change classes

df_corp$adverse_audit_opinion <- as.factor(df_corp$adverse_audit_opinion) 
df_corp$industry <- as.factor(df_corp$industry)
df_corp$payment_reminders <- as.factor(df_corp$payment_reminders) 
df_corp$equity <- as.numeric(df_corp$equity)
df_corp$total_assets <- as.numeric(df_corp$total_assets)
df_corp$revenue <- as.numeric(df_corp$revenue)
df_corp$age_of_company <- as.numeric(df_corp$age_of_company)

# We now look on the numerical classes

df_corp %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

summary(df_corp)

# Take a closer look on the distribution of the values we belive is errors

x <- df_corp %>% 
  select_if(is.numeric)

placeholder <- matrix(ncol=ncol(x), nrow=1)
colnames(placeholder) <- names(x)

for(i in 1:ncol(x)){
  placeholder[,i] <- ifelse(x[,i] > 100000000000 , yes = 1 , 
                            no = ifelse(x[,i] < -100000000000, yes = 1, no = 0)) %>% 
    sum()
}

placeholder

placeholder/nrow(df_corp)

# Changes the obvious errors to NA

df_corp[df_corp < -1000000000000000] <- NA #Don't care about the warning, because it is no errors in the factor classes
df_corp[df_corp >  1000000000000000] <- NA

df_corp %>%
  select_if(anyNA) %>% summary

# We take a new look after we changed the error with NA

df_corp %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

# We definitly need to do something about those outliers, only gross_operating_inc_perc and age_of_company looks to be okay.

############################################################ Dealing with the outliers ########################################################

df_corp %>% 
  select(which(sapply(.,class)=="numeric"),default) %>%   
  gather(metric, value, -default) %>% 
  ggplot(aes(x= default, y=value, fill = default))+
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

# Function for replacing outliers with NA's

remove_outliers_na <- function(x) { # To be used on other variables
  qnt <- as.vector(quantile(x, probs=c(0.025, 0.975), na.rm = TRUE))
  y <- x
  y[x < qnt[1]] <- NA
  y[x > qnt[2]] <- NA
  y
}

# Strategi: We set NA on all outliers that are less then 0.025 and more than 0.975 the quantiles. 
#           This is done for all numeric variables except age_of_company, gross_operarting_inc_perc, paid_debt_collection and unpaid_debt collection.
#           paid and unpaid debt collection will be categorized.

df_corp$profit_margin <- remove_outliers_na(df_corp$profit_margin)
df_corp$operating_margin <- remove_outliers_na(df_corp$operating_margin)
df_corp$EBITDA_margin <- remove_outliers_na(df_corp$EBITDA_margin)
df_corp$interest_coverage_ratio <- remove_outliers_na(df_corp$interest_coverage_ratio)
df_corp$cost_of_debt <- remove_outliers_na(df_corp$cost_of_debt)
df_corp$interest_bearing_debt <- remove_outliers_na(df_corp$interest_bearing_debt)
df_corp$revenue_stability <- remove_outliers_na(df_corp$revenue_stability)
df_corp$equity_ratio <- remove_outliers_na(df_corp$equity_ratio)
df_corp$equity_ratio_stability <- remove_outliers_na(df_corp$equity_ratio_stability)
df_corp$liquidity_ratio_1 <- remove_outliers_na(df_corp$liquidity_ratio_1)
df_corp$liquidity_ratio_2 <- remove_outliers_na(df_corp$liquidity_ratio_2)
df_corp$liquidity_ratio_3 <- remove_outliers_na(df_corp$liquidity_ratio_3)
df_corp$equity <- remove_outliers_na(df_corp$equity)
df_corp$total_assets <- remove_outliers_na(df_corp$total_assets)
df_corp$revenue <- remove_outliers_na(df_corp$revenue)
df_corp$amount_unpaid_debt <- remove_outliers_na(df_corp$amount_unpaid_debt)

# Looking at the distribution again

df_corp %>% 
  select(which(sapply(.,class)=="numeric"),default) %>%   
  gather(metric, value, -default) %>% 
  ggplot(aes(x= default, y=value, fill = default))+
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

df_corp %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

########################################################## Impute the NA's with MICE package ####################################################

df_corp <- df_corp %>% 
  select(-equity_ratio_stability) # Removed due to collinearity and if look at the earlier plots it make sens.

temp_imputed1 <- mice(df_corp, m=2,maxit=3,meth='mean',seed=500)

 # temp_imputed2 <- mice(df_corp, m=2,maxit=3,meth='pmm',seed=500)

 # saveRDS(temp_imputed2, file = "impute.Rdata") # with m=3 and maxit = 3

temp_imputed2 <- readRDS(file = "impute.Rdata")

densityplot(temp_imputed1, drop.unused.levels = TRUE)

densityplot(temp_imputed2, drop.unused.levels = TRUE)

# We complete the impution and check the result

complete_imputed <- complete(temp_imputed2, 1)

dim(df_corp)
dim(complete_imputed)

df_cleaned <- complete_imputed

########################## Now as the data is cleaned we want to make some binary variables. ##################

# We change paid_debt_collection and unpaid debt collection to binary

df_cleaned$unpaid_debt_collection <- ifelse(df_cleaned$unpaid_debt_collection > 0, yes = 1, no = 0) # 1 = have debt collection

df_cleaned$paid_debt_collection <- ifelse(df_cleaned$paid_debt_collection > 0, yes = 1, no = 0) # 1 = have had debt collection

df_cleaned$paid_debt_collection <- as.factor(df_cleaned$paid_debt_collection)
df_cleaned$unpaid_debt_collection <- as.factor(df_cleaned$unpaid_debt_collection)

table(df_cleaned$default, df_cleaned$unpaid_debt_collection)
table(df_cleaned$default, df_cleaned$paid_debt_collection)

summary(df_cleaned)

# Looking at the distribution again

df_cleaned %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric))+
  geom_density(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free")

#################################### Splitting the data to training and test ############################

df_reduced <- df_cleaned

set.seed(1)
index <- createDataPartition(df_reduced$default, p = 0.7, list = FALSE)
train_data <- df_reduced[index, ]
test_data <- df_reduced[-index, ]

# Check

nrow(train_data) + nrow(test_data) == nrow(df_reduced)

summary(train_data$default)[2]/nrow(train_data)

summary(test_data$default)[2]/nrow(test_data)

############################################# Check correlation due to multicolinarity ########################################

# We first check the correlation between the numeric variables

cor_df <- train_data %>% 
  select_if(is.numeric)

# Make a function that print variables that correlates more than a threshold

corr_check <- function(data, threshold){
  mat_cor <- cor(data)
  mat_cor
  
  for (i in 1:nrow(mat_cor)){
    correlations <-  which((abs(mat_cor[i,i:ncol(mat_cor)]) > threshold) & (mat_cor[i,i:ncol(mat_cor)] != 1))
    
    if(length(correlations)> 0){
      lapply(correlations,FUN =  function(x) (cat(paste(colnames(data)[i], "with",colnames(data)[x]), "\n")))
      
    }
  }
}

corr_check(cor_df, 0.7)

train_data <- train_data %>% 
  select(-operating_margin, -liquidity_ratio_1, -EBITDA_margin, -equity)

# Secondly we check the correlation with the cateorized variables

################################# Model selection by rigdge regression and lasso ###########################################

# glmnet does not use the model formula language can use x and y

library(glmnet)

x <- model.matrix(default ~.-1, data = train_data)
y <- train_data$default

#  Ridge-regression model

fit.redge <- glmnet(x,y, alpha = 0, family = "binomial") # 0 = ridge, 1 = Lasso
plot(fit.redge, xvar = "lambda", label = TRUE)
cv.ridge <- cv.glmnet(x,y, alpha = 0, family = "binomial")
plot(cv.ridge)
coef(cv.ridge)

# Lasso model

fit.lasso <- glmnet(x,y, alpha = 1, family = "binomial")
plot(fit.lasso, xvar = "dev", label = TRUE)
fit.lasso

cv.lasso <- cv.glmnet(x,y, alpha = 1, family = "binomial")
plot(cv.lasso)
coef(cv.lasso$glmnet.fit)


######################################### Making some models ########################################################

library(grid)

# Removed variables

train_data <- train_data %>% 
  select(-total_assets, -revenue, -industry, -paid_debt_collection)

# We need to handel the unbalanced dataset when modeling!

# Undersampling - Could have used oversampling, rose or smote. But took to mutch time to manage.

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, # pr?v med 5
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

########################################### Model 1: GLM ################################################################

set.seed(1)

model_glm <- train(default ~.,
                   data = train_data,
                   method = "glm",
                   trControl = ctrl)

model_glm
# Importance plot

plot(varImp(model_glm))

# Confusion matrix

glm_pred <- data.frame(actual = test_data$default,
                       predict(model_glm, newdata = test_data, type = "prob"))

glm_pred$predict <- ifelse(glm_pred$X1 > 0.5, 1, 0)
glm_pred$predict <- as.factor(glm_pred$predict)

cm_glm <-confusionMatrix(glm_pred$predict, test_data$default)
cm_glm

summary(model_glm)

# ROC curve glm

library(pROC)

result.predicted.prob <- predict(model_glm, test_data, type="prob") # Prediction

result.roc <- roc(test_data$default, result.predicted.prob$`1`) # Draw ROC 

plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy

########################################### Model 2: Random Forest ######################################################

rf_grid <- expand.grid(mtry = c(2, 3, 4, 5),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 3, 5))
rf_grid

set.seed(1)

model_rf <- caret::train(default ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)

# saveRDS(model_rf, file = "rf.Rdata")

model_rf <- readRDS("rf.Rdata")

# Importance plot

plot(varImp(model_rf))

# Confusion matrix

rf_pred <- data.frame(actual = test_data$default,
                      predict(model_rf, newdata = test_data, type = "prob"))
rf_pred$predict <- ifelse(rf_pred$X1 > 0.5, 1, 0)
rf_pred$predict <- as.factor(rf_pred$predict)

cm_rf <- confusionMatrix(rf_pred$predict, test_data$default)
cm_rf

# ROC curve rf

result.predicted.prob <- predict(model_rf, test_data, type="prob") # Prediction

result.roc <- roc(test_data$default, result.predicted.prob$`1`) # Draw ROC 

plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy

############################################# Model 3: Xgboost #######################################################

xgb_grid <- expand.grid(nrounds = 300,
                        max_depth = 7, #3
                        min_child_weight = 1,
                        subsample = 1,
                        gamma = 0.2,
                        colsample_bytree = 0.9,
                        eta = .3)

set.seed(1)

model_xgb <- caret::train(default ~ .,
                          data = train_data,
                          method = "xgbTree",
                          tuneGrid =xgb_grid,
                          preProcess = c("scale", "center"),
                          trControl = ctrl)


plot(model_xgb)

# saveRDS(model_xgb, file = "xgb.Rdata")

model_xgb <- readRDS("xgb.Rdata")

model_xg

# Importance plot

plot(varImp(model_xgb))

# Confusion matrix

xgb_pred <- data.frame(actual = test_data$default,
                      predict(model_xgb, newdata = test_data, type = "prob"))

xgb_pred$predict <- ifelse(xgb_pred$X1 > 0.5, 1, 0)
xgb_pred$predict <- as.factor(xgb_pred$predict)

cm_xgb <- confusionMatrix(xgb_pred$predict, test_data$default)
cm_xgb

# ROC curve xgb

result.predicted.prob <- predict(model_xgb, test_data, type="prob") # Prediction

result.roc <- roc(test_data$default, result.predicted.prob$`1`) # Draw ROC curve.

plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy


############################# Look at at difference all together ##########################

# Look at the performance

models <- list(glm = model_glm,
               rf = model_rf,
               xgb = model_xgb)

resampling <- resamples(models)

bwplot(resampling)



# density plots of accuracy

scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(resampling, scales=scales, pch = "|", allow.multiple = TRUE)

# Other snacks for comparing 

splom(resampling)

xyplot(resampling, models=c("rf", "xgb"))

summary(resampling)
