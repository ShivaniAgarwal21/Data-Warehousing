# ------------------------------------------------------------------------
#                           Random Forest Assignment
# ------------------------------------------------------------------------

# -------------------------------- Task Ideas ----------------------------

# 1.  Change the train / test split and observe differences in effectiveness.
#Ans: 
cc_order        <- order(cc_data$Time)
cc_sorted       <- cc_data[cc_order,]
cc_sorted$Time  <- NULL             # Never train on an index or index-like data.percent_train_1   <- 2.3/3
percent_train_1
train_start_1     <- 1
train_end_1       <- as.integer(nrow(cc_sorted) * percent_train_1)
train_end_1
test_start_1      <- train_end_1 + 1
test_end_1        <- nrow(cc_sorted)

cc_train_1        <- cc_sorted[train_start_1:train_end_1,]
cc_test_1         <- cc_sorted[test_start_1:test_end_1,]

sum(cc_test_1$Class)
sum(cc_train_1$Class)
#
#Here, we changed the training and test data split from 2/3 = 0.6667 to 2.3/3 = 0.7666. 
#Based on the change in the split for training data, the sum for class attribute 
#changed from 122 to 89.Initially traning data set starts from 1 to 189871 records and 
#testing starts from 189872 to 284807 records. Now afte spliting traning data set it starts 
#from 1 to 218352 records and testing starts from 218353 to 284807 records.
#
#
# 2.  Check the number of frauds in both the train / test sets.  Reasonable?
#Ans:
sum(cc_test_1$Class)
sum(cc_train_1$Class)
# Here, the number of frauds for test dataset has changed from 122 to 89 and for training 
#dataset has changed from 370 to 403. As, we changed split of training and testing set, in
#accordance to that our number of frauds detected in test and training dataset changed 
#reasonably.
#
# 3.  Try different values for ntree, nodesize, and maxnodes.
#Ans:
First Variation:
rf_model<- randomForest(Class ~ ., data = cc_train_1,ntree=4, nodesize=6, maxnodes=12)
rf_model

Here, we changed the ntree, nodesize and maxnodes for new split dataset. Initially MSE was 
0.0006088528 whereas now it changed to 0.0005841665. Also, for % Var it was 68.69 whereas 
now it is changed to 68.29.

Second Variation:
rf_model<- randomForest(Class ~ ., data = cc_train_1,ntree=2, nodesize=3, maxnodes=8)
rf_model
Upon decreasing the value for ntree, nodesize and maxnodes, the value for Mean of squared 
residuals has increased to 0.0007011197. It means that if we reduce the number of trees 
then the Mean of squared residual increases but % Var that is 61.94 decreases on accordance with the 
change in size.

Observation:
Overall, if we decrease the number of random forest trees than the error residual increases and 
vice versa is also true.
Also, for variance it is decreasing when we decrease the number of trees. 

# 4.  Allow Time to become a variable.  Look what happens.  Why?
#Ans:
cc_order        <- order(cc_data$Time)
cc_sorted       <- cc_data[cc_order,]
# --------------------- Split into Train and Test Sets -------------------

# Payment data is inherently sequential.  You would not take a random sample.
# Rather, the train/test split should be made on a chronological basis.
percent_train_1   <- 2.3/3
percent_train_1
train_start_1     <- 1
train_end_1       <- as.integer(nrow(cc_sorted) * percent_train_1)
train_end_1
test_start_1      <- train_end_1 + 1
test_end_1        <- nrow(cc_sorted)

cc_train_1        <- cc_sorted[train_start_1:train_end_1,]
cc_test_1         <- cc_sorted[test_start_1:test_end_1,]

sum(cc_test_1$Class)
sum(cc_train_1$Class)
# --------------------- Train Random Forest Algorithm --------------------

rf_model        <- randomForest(Class ~ ., data = cc_train_1, 
                                ntree=3, nodesize=4, maxnodes=10)
rf_model

Here, we considered Time as a variable where we observed that the MSE value has increased from 
0.0006088528 to 0.0008033649. Also, for % Var it was 68.69 whereas now it is changed to 56.39.
It means that adding index variable like Time will degrade the result by increasing the MSE and 
decreasing % variance.

# 5.  The results using all variables are unrealistically good.
#     Pick 5-10 variables and see what the results look like then.
#Ans: 
cc_order        <- order(cc_data$Time)
cc_sorted       <- cc_data[cc_order,V1,V2,V3,V4,V5,V6,V7,V8]
cc_sorted$Time  <- NULL             # Never train on an index or index-like data.
# --------------------- Split into Train and Test Sets -------------------

# Payment data is inherently sequential.  You would not take a random sample.
# Rather, the train/test split should be made on a chronological basis.
percent_train_1   <- 2.3/3
percent_train_1
train_start_1     <- 1
train_end_1       <- as.integer(nrow(cc_sorted) * percent_train_1)
train_end_1
test_start_1      <- train_end_1 + 1
test_end_1        <- nrow(cc_sorted)

cc_train_1        <- cc_sorted[train_start_1:train_end_1,]
cc_test_1         <- cc_sorted[test_start_1:test_end_1,]

sum(cc_test_1$Class)
sum(cc_train_1$Class)
# --------------------- Train Random Forest Algorithm --------------------

rf_model<- randomForest(Class ~ ., data = cc_train_1,ntree=3, nodesize=4, maxnodes=10)
rf_model

Here, we considered first eight variables where we observed that the MSE value has decreased 
from 0.0008033649 to 0.0007672507. Also, for % Var it was 56.39 whereas now it is changed to 
58.35. 
It means that considering just first five variables is improving the result by decreasing 
the MSE and increasing % variance.

# 6.  See if you can define a metric for variable selection, and pick the same number of 
# variables as in task 5 based on this metric.  Did your results improve?  Why or why not?
#Ans:
pctiles <- c(60, 65, 70, 75, 80)

quick_eval('RESULTS 1', rf_predict, cc_test_1$Class, pctiles, 120)

MSE_RF <- sum((rf_predict- cc_test_1$Class)^2)/NROW(cc_test_1)
MSE_RF

# --------------------- Restrict the Set of Variables --------------------

restriction       <- c(2:7)    # Variable indexes must be between >= 1 and <= 7.

label_small       <- c(restriction, 30)
label_small
cc_trn_small      <- cc_train_1[,label_small]
cc_tst_small      <- cc_test_1[,label_small]

small_model       <- randomForest(Class ~ ., data = cc_trn_small,
                                  ntree=3, nodesize=4, maxnodes=10)

predict_small     <- predict(small_model,cc_tst_small)

quick_eval('SMALL', predict_small, cc_test_1$Class, pctiles, 120)

# ------------------------------------------------------------------------
#                                  EOF
# ------------------------------------------------------------------------

NROW(cc_test_1)



# ----------------------------- Load Libraries ---------------------------

library(caTools)
library(randomForest)

source('D:\\PSU\\Winter 2020\\Data warehousing\\HW 6\\eval_model.r')

# ---------------------- Set Path and File Variables ---------------------

path_forest     <- 'D:\\PSU\\Winter 2020\\Data warehousing\\HW 6\\'

fp_data         <- paste(path_forest , 'creditcard.csv', sep='')

# -------------------- Load and Check Credit Card Data -------------------

cc_data         <- read.csv(fp_data)

a <- cc_order        <- order(cc_data$Time)
a
cc_sorted       <- cc_data[cc_order,]

cc_sorted$Time  <- NULL             # Never train on an index or index-like data.

# --------------------- Split into Train and Test Sets -------------------

# Payment data is inherently sequential.  You would not take a random sample.
# Rather, the train/test split should be made on a chronological basis.

percent_train   <- 2/3
percent_train
train_start     <- 1
train_end       <- as.integer(nrow(cc_sorted) * percent_train)

test_start      <- train_end + 1
test_end        <- nrow(cc_sorted)

cc_train        <- cc_sorted[train_start:train_end,]
cc_test         <- cc_sorted[test_start:test_end,]

sum(cc_test$Class)

# --------------------- Train Random Forest Algorithm --------------------

rf_model        <- randomForest(Class ~ ., data = cc_train, 
                         ntree=3, nodesize=4, maxnodes=10)

rf_predict <- predict(rf_model,cc_test)

# --------------------- Train Random Forest Algorithm --------------------

pctiles <- c(60, 65, 70, 75, 80)

quick_eval('RESULTS 1', rf_predict, cc_test_1$Class, pctiles, 120)

MSE_RF <- sum((rf_predict- cc_test_1$Class)^2)/NROW(cc_test_1)
MSE_RF

# --------------------- Restrict the Set of Variables --------------------

restriction       <- c(2:5, 7, 12, 17, 20)    # Variable indexes must be between >= 1 and <= 29.

label_small       <- c(restriction, 30)

cc_trn_small      <- cc_train_1[,label_small]
cc_tst_small      <- cc_test_1[,label_small]

small_model       <- randomForest(Class ~ ., data = cc_trn_small,
                                  ntree=3, nodesize=4, maxnodes=10)

predict_small     <- predict(small_model,cc_tst_small)

quick_eval('SMALL', predict_small, cc_test_1$Class, pctiles, 120)

# ------------------------------------------------------------------------
#                                  EOF
# ------------------------------------------------------------------------

NROW(cc_test)

