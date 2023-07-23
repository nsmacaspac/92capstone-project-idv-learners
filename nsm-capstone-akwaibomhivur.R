# CLASSIFICATION MODEL ON ANTIRETROVIRAL THERAPY REACTION AND FAILURE DEVELOPED ON THE UNIQUE RECORDS OF THE AKWA IBOM HIV DATABASE
# NICELLE SERNADILLA MACASPAC
# JULY 2023
# R VERSION: 4.3
# RUNNING TIME: 4 minutes



# UNIQUE RECORDS OF THE AKWA IBOM HIV DATABASE



# we import the dataset

options(timeout = 120) # timeout in seconds for some Internet operations
if(!file.exists("mmc1.xlsx")) download.file("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8142042/bin/mmc1.xlsx", "mmc1.xlsx")

if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(readxl)
dataset <- as.data.frame(read_xlsx("mmc1.xlsx", range = "N27:AB1083")) # range reads the Unique Records table, which is the combined version of the Individual Treatment Change Episodes table
head(dataset, n = 5) # fig1 in the Rmd file
#   PID SEX BCD4 FCD4 BRNA FRNA BWt(kg) FWt(kg)    DRUGCOMB       PR C1 C2 C3 C4 C5
# 1   1   F  148  106  3.0  1.3      42      43 TDF+3TC+EFV 53.56199  0  0  1  0  0
# 2   2   F  145  378  2.5  1.3      57      60 AZT+3TC+NVP 55.33422  0  0  0  1  0
# 3   3   M   78  131  4.1  1.7      70      75 AZT+3TC+NVP 50.00000  0  1  0  0  0
# 4   4   M  295  574  4.4  1.9      64      66 AZT+3TC+NVP 50.00000  0  0  1  0  0
# 5   5   F  397  792  1.9  1.3      52      55 AZT+3TC+NVP 76.00000  0  0  0  0  1
str(dataset)
# 'data.frame':	1056 obs. of  15 variables:
# $ PID     : num  1 2 3 4 5 6 7 8 9 10 ...
# $ SEX     : chr  "F" "F" "M" "M" ...
# $ BCD4    : num  148 145 78 295 397 155 303 370 210 120 ...
# $ FCD4    : num  106 378 131 574 792 280 679 615 242 278 ...
# $ BRNA    : num  3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ FRNA    : num  1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ BWt(kg) : num  42 57 70 64 52 59 62 78 82 85 ...
# $ FWt(kg) : num  43 60 75 66 55 56 60 68 82 80 ...
# $ DRUGCOMB: chr  "TDF+3TC+EFV" "AZT+3TC+NVP" "AZT+3TC+NVP" "AZT+3TC+NVP" ...
# $ PR      : num  53.6 55.3 50 50 76 ...
# $ C1      : num  0 0 0 0 0 0 0 0 1 0 ...
# $ C2      : num  0 0 1 0 0 1 0 0 0 0 ...
# $ C3      : num  1 0 0 1 0 0 1 0 0 1 ...
# $ C4      : num  0 1 0 0 0 0 0 1 0 0 ...
# $ C5      : num  0 0 0 0 1 0 0 0 0 0 ...



# we render the dataset into tidy format

colnames(dataset) <- c("id", "sex", "bcd4", "fcd4", "brna", "frna", "bweight", "fweight", "therapy", "response", "vhi_tf", "hi_tf", "li", "vli", "ni")
head(dataset, n = 5)
#   id sex bcd4 fcd4 brna frna bweight fweight     therapy response vhi_tf hi_tf li
# 1  1   F  148  106  3.0  1.3      42      43 TDF+3TC+EFV 53.56199      0     0  1
# 2  2   F  145  378  2.5  1.3      57      60 AZT+3TC+NVP 55.33422      0     0  0
# 3  3   M   78  131  4.1  1.7      70      75 AZT+3TC+NVP 50.00000      0     1  0
# 4  4   M  295  574  4.4  1.9      64      66 AZT+3TC+NVP 50.00000      0     0  1
# 5  5   F  397  792  1.9  1.3      52      55 AZT+3TC+NVP 76.00000      0     0  0
# vli ni
# 1   0  0
# 2   1  0
# 3   0  0
# 4   0  0
# 5   0  1

sum(is.na(dataset))
# [1] 0

sum((dataset$vhi_tf + dataset$hi_tf + dataset$li + dataset$vli + dataset$ni) > 1 ) # verifies if there is >1 drug reaction per row
# [1] 0
sum(dataset$vhi_tf) +
  sum(dataset$hi_tf) +
  sum(dataset$li) +
  sum(dataset$vli) +
  sum(dataset$ni) # checks if drug reactions total to the number of observations
# [1] 1056
# there is only 1 drug reaction per row

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
dataset1 <- dataset |>
  mutate(brna = brna*10^2) |>
  mutate(frna = frna*10^2) |> # simplifies the unit from times 10^2 copies to just copies
  mutate(dreaction = case_when(vhi_tf == 1 ~ "vhi_tf",
                              hi_tf == 1 ~ "hi_tf",
                              li == 1 ~ "li",
                              vli == 1 ~ "vli",
                              ni == 1 ~ "ni")) |> # relabels drug reactions as vhi_tf to ni and merges them under a newly defined dreaction column
  select(-vhi_tf, -hi_tf, -li, -vli, -ni)
head(dataset1, n = 5)
#   id sex bcd4 fcd4 brna frna bweight fweight     therapy response dreaction
# 1  1   F  148  106  300  130      42      43 TDF+3TC+EFV 53.56199       li
# 2  2   F  145  378  250  130      57      60 AZT+3TC+NVP 55.33422      vli
# 3  3   M   78  131  410  170      70      75 AZT+3TC+NVP 50.00000    hi_tf
# 4  4   M  295  574  440  190      64      66 AZT+3TC+NVP 50.00000       li
# 5  5   F  397  792  190  130      52      55 AZT+3TC+NVP 76.00000       ni
str(dataset1)
# 'data.frame':	1056 obs. of  11 variables:
# $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ sex      : chr  "F" "F" "M" "M" ...
# $ bcd4     : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4     : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna     : num  300 250 410 440 190 420 420 510 510 270 ...
# $ frna     : num  130 130 170 190 130 170 130 170 410 170 ...
# $ bweight  : num  42 57 70 64 52 59 62 78 82 85 ...
# $ fweight  : num  43 60 75 66 55 56 60 68 82 80 ...
# $ therapy  : chr  "TDF+3TC+EFV" "AZT+3TC+NVP" "AZT+3TC+NVP" "AZT+3TC+NVP" ...
# $ response : num  53.6 55.3 50 50 76 ...
# $ dreaction: chr  "li" "vli" "hi_tf" "li" ...



# we preprocess dataset1

options(digits = 3)
dataset1a <- dataset1 |>
  mutate(sex = as.numeric(factor(sex))) |>
  mutate(therapy = as.numeric(factor(therapy))) |>
  mutate(dreaction = as.numeric(factor(dreaction, c("ni", "vli", "li", "hi_tf", "vhi_tf"))))

summary(dataset1a)
#       id            sex            bcd4           fcd4           brna
# Min.   :   1   Min.   :1.00   Min.   :   4   Min.   :  52   Min.   :120
# 1st Qu.: 265   1st Qu.:1.00   1st Qu.: 238   1st Qu.: 423   1st Qu.:300
# Median : 528   Median :1.00   Median : 378   Median : 639   Median :385
# Mean   : 528   Mean   :1.33   Mean   : 436   Mean   : 659   Mean   :377
# 3rd Qu.: 792   3rd Qu.:2.00   3rd Qu.: 590   3rd Qu.: 874   3rd Qu.:470
# Max.   :1056   Max.   :2.00   Max.   :1810   Max.   :2120   Max.   :650
#      frna        bweight         fweight         therapy        response
# Min.   :110   Min.   :  4.7   Min.   :  6.0   Min.   :1.00   Min.   :30.0
# 1st Qu.:140   1st Qu.: 56.0   1st Qu.: 57.0   1st Qu.:2.00   1st Qu.:51.0
# Median :180   Median : 63.0   Median : 63.0   Median :3.00   Median :57.8
# Mean   :216   Mean   : 63.7   Mean   : 64.3   Mean   :2.63   Mean   :60.9
# 3rd Qu.:250   3rd Qu.: 71.0   3rd Qu.: 72.0   3rd Qu.:3.00   3rd Qu.:71.0
# Max.   :630   Max.   :125.0   Max.   :120.0   Max.   :3.00   Max.   :86.0
#   dreaction
# Min.   :1.00
# 1st Qu.:1.00
# Median :2.00
# Mean   :2.14
# 3rd Qu.:3.00
# Max.   :5.00

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
nearZeroVar(dataset1a)
# integer(0)

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
corrplot(cor(dataset1a), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "black", number.cex = 0.5, number.digits = 2) # shows the correlation coefficients with 2 decimal digits across all variables
# CD4 count is negatively correlated with drug reaction
# RNA load is positively correlated with drug reaction
# response is highly correlated with drug reaction as expected

dataset2 <- dataset1 |>
  select(bcd4, fcd4, brna, frna, dreaction) |> # keeps CD4 count and RNA load as predictors and drug reaction as outcome
  mutate(dreaction = factor(dreaction, c("ni", "vli", "li", "hi_tf", "vhi_tf")))
head(dataset2, n = 5)
#   bcd4 fcd4 brna frna dreaction
# 1  148  106  300  130        li
# 2  145  378  250  130       vli
# 3   78  131  410  170     hi_tf
# 4  295  574  440  190        li
# 5  397  792  190  130        ni
str(dataset2)
# 'data.frame':	1056 obs. of  5 variables:
# $ bcd4     : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4     : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna     : num  300 250 410 440 190 420 420 510 510 270 ...
# $ frna     : num  130 130 170 190 130 170 130 170 410 170 ...
# $ dreaction: Factor w/ 5 levels "ni","vli","li",..: 3 2 4 3 1 4 3 2 5 3 ...



###############
###   WIP   ###
###############










dataset2 |>
  ggplot(aes(brna, bcd4, color = dreaction)) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) + # adds a trend line
  scale_x_continuous("Baseline RNA Load", limits = c(100, 700)) +
  scale_y_continuous("Baseline CD4 Count", limits = c(0, 2000)) + # encompasses the full range of the axes of baseline and follow-up data for ease of comparison
  scale_color_discrete(name = "Drug Reaction")

dataset2 |>
  ggplot(aes(frna, fcd4, color = dreaction)) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Follow-up RNA Load", limits = c(100, 700)) +
  scale_y_continuous("Follow-up CD4 Count", limits = c(0, 2000)) +
  scale_color_discrete(name = "Drug Reaction")
# there is a general decrease in RNA load at follow-up










# we partition dataset2

set.seed(20, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(20) # if using R 3.5 or earlier
# for reproducibility during assessment
test_index <- createDataPartition(dataset2$dreaction, p = 0.2, list = FALSE)
train_set <- dataset2[-test_index,]
test_set <- dataset2[test_index,]



# we train and test the k-nearest neighbor model

set.seed(30, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(30) # if using R 3.5 or earlier
knn_model <- train(dreaction ~ ., train_set, method = "knn", tuneGrid = data.frame(k = seq(10, 30, 1))) # tunes neighbor number
ggplot(knn_model, highlight = TRUE)
knn_model
# k-Nearest Neighbors
#
# 844 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 844, 844, 844, 844, 844, 844, ...
# Resampling results across tuning parameters:
#
#   k   Accuracy  Kappa
# 10  0.532     0.323
# 11  0.534     0.326
# 12  0.536     0.328
# 13  0.537     0.329
# 14  0.540     0.332
# 15  0.537     0.328
# 16  0.541     0.333
# 17  0.540     0.332
# 18  0.537     0.328
# 19  0.538     0.330
# 20  0.542     0.336
# 21  0.542     0.336
# 22  0.544     0.339
# 23  0.545     0.340
# 24  0.543     0.337
# 25  0.542     0.336
# 26  0.543     0.337
# 27  0.541     0.334
# 28  0.541     0.333
# 29  0.539     0.331
# 30  0.536     0.326
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 23.

knn_dreaction <- predict(knn_model, test_set)
confusionMatrix(knn_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#         1  0  0  0  0  0
#         2  0  1  0  1  0
#         3  5 10 17  6  1
#         4  0  2 26 47 13
#         5  0  0  6 31 46
#
# Overall Statistics
#
# Accuracy : 0.524
# 95% CI : (0.454, 0.592)
# No Information Rate : 0.401
# P-Value [Acc > NIR] : 0.000203
#
# Kappa : 0.299
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.0000  0.07692   0.3469    0.553    0.767
# Specificity            1.0000  0.99497   0.8650    0.677    0.757
# Pos Pred Value            NaN  0.50000   0.4359    0.534    0.554
# Neg Pred Value         0.9764  0.94286   0.8150    0.694    0.891
# Prevalence             0.0236  0.06132   0.2311    0.401    0.283
# Detection Rate         0.0000  0.00472   0.0802    0.222    0.217
# Detection Prevalence   0.0000  0.00943   0.1840    0.415    0.392
# Balanced Accuracy      0.5000  0.53595   0.6060    0.615    0.762



# we train and test the recursive partitioning and regression trees model

set.seed(40, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(40) # if using R 3.5 or earlier
rpart_model <- train(dreaction ~ ., train_set, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))) # tunes tree complexity
ggplot(rpart_model, highlight = TRUE)
rpart_model
# CART
#
# 844 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 844, 844, 844, 844, 844, 844, ...
# Resampling results across tuning parameters:
#
#   cp    Accuracy  Kappa
# 0.00  0.970     0.958
# 0.01  0.971     0.958
# 0.02  0.964     0.949
# 0.03  0.937     0.910
# 0.04  0.914     0.877
# 0.05  0.896     0.850
# 0.06  0.874     0.817
# 0.07  0.867     0.806
# 0.08  0.855     0.788
# 0.09  0.849     0.778
# 0.10  0.849     0.778
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.01.

plot(rpart_model$finalModel, margin = 0.05) # margin adjusts the plot size
text(rpart_model$finalModel, cex = 1) # cex adjusts the label size

rpart_dreaction <- predict(rpart_model, test_set)
confusionMatrix(rpart_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#         1  5  1  0  0  0
#         2  0 11  0  0  0
#         3  0  1 47  1  0
#         4  0  0  2 83  0
#         5  0  0  0  1 60
#
# Overall Statistics
#
# Accuracy : 0.972
# 95% CI : (0.939, 0.99)
# No Information Rate : 0.401
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.96
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            1.0000   0.8462    0.959    0.976    1.000
# Specificity            0.9952   1.0000    0.988    0.984    0.993
# Pos Pred Value         0.8333   1.0000    0.959    0.976    0.984
# Neg Pred Value         1.0000   0.9900    0.988    0.984    1.000
# Prevalence             0.0236   0.0613    0.231    0.401    0.283
# Detection Rate         0.0236   0.0519    0.222    0.392    0.283
# Detection Prevalence   0.0283   0.0519    0.231    0.401    0.288
# Balanced Accuracy      0.9976   0.9231    0.973    0.980    0.997

# higher than the accuracy of the k-nearest neighbor model



# we train and test the Rborist model

if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
library(Rborist)
set.seed(50, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(50) # if using R 3.5 or earlier
rborist_model <- train(dreaction ~ ., train_set, method = "Rborist", tuneGrid = expand.grid(predFixed = seq(1, 4), minNode = seq(1, 4))) # tunes combinations of predictor number and node size
ggplot(rborist_model, highlight = TRUE)
rborist_model
# Random Forest
#
# 844 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 844, 844, 844, 844, 844, 844, ...
# Resampling results across tuning parameters:
#
#   predFixed  minNode  Accuracy  Kappa
# 1          1        0.970     0.958
# 1          2        0.970     0.958
# 1          3        0.971     0.959
# 1          4        0.971     0.958
# 2          1        0.980     0.971
# 2          2        0.981     0.973
# 2          3        0.980     0.972
# 2          4        0.981     0.973
# 3          1        0.980     0.972
# 3          2        0.981     0.973
# 3          3        0.980     0.972
# 3          4        0.980     0.972
# 4          1        0.981     0.973
# 4          2        0.980     0.972
# 4          3        0.980     0.972
# 4          4        0.981     0.972
#
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were predFixed = 3 and minNode = 2.

varImp(rborist_model)
# Rborist variable importance
#
#     Overall
# brna   100.0
# fcd4    51.1
# bcd4    45.3
# frna     0.0

# baseline RNA load is the most important predictor

rborist_dreaction <- predict(rborist_model, test_set)
confusionMatrix(rborist_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#         1  5  0  0  0  0
#         2  0 13  0  0  0
#         3  0  0 49  1  0
#         4  0  0  0 84  0
#         5  0  0  0  0 60
#
# Overall Statistics
#
# Accuracy : 0.995
# 95% CI : (0.974, 1)
# No Information Rate : 0.401
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.993
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            1.0000   1.0000    1.000    0.988    1.000
# Specificity            1.0000   1.0000    0.994    1.000    1.000
# Pos Pred Value         1.0000   1.0000    0.980    1.000    1.000
# Neg Pred Value         1.0000   1.0000    1.000    0.992    1.000
# Prevalence             0.0236   0.0613    0.231    0.401    0.283
# Detection Rate         0.0236   0.0613    0.231    0.396    0.283
# Detection Prevalence   0.0236   0.0613    0.236    0.396    0.283
# Balanced Accuracy      1.0000   1.0000    0.997    0.994    1.000

# higher than the accuracies of the k-nearest neighbor model and the recursive partitioning and regression trees model



# we train and test the quadratic discriminant analysis model

set.seed(60, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(60) # if using R 3.5 or earlier
qda_model <- train(dreaction ~ ., train_set, method = "qda")
qda_model
# Quadratic Discriminant Analysis
#
# 844 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 844, 844, 844, 844, 844, 844, ...
# Resampling results:
#
#   Accuracy  Kappa
# 0.724     0.602

qda_dreaction <- predict(qda_model, test_set)
confusionMatrix(qda_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#         1  4  2  1  0  0
#         2  1  9  2  0  0
#         3  0  2 26 10  0
#         4  0  0 20 71 12
#         5  0  0  0  4 48
#
# Overall Statistics
#
# Accuracy : 0.745
# 95% CI : (0.681, 0.802)
# No Information Rate : 0.401
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.631
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.8000   0.6923    0.531    0.835    0.800
# Specificity            0.9855   0.9849    0.926    0.748    0.974
# Pos Pred Value         0.5714   0.7500    0.684    0.689    0.923
# Neg Pred Value         0.9951   0.9800    0.868    0.872    0.925
# Prevalence             0.0236   0.0613    0.231    0.401    0.283
# Detection Rate         0.0189   0.0425    0.123    0.335    0.226
# Detection Prevalence   0.0330   0.0566    0.179    0.486    0.245
# Balanced Accuracy      0.8928   0.8386    0.728    0.792    0.887

# higher than the accuracy of the k-nearest neighbor model
# lower than the accuracies of the recursive partitioning and regression trees model and the Rborist model



# we examine the errors of the Rborist model

which_index <- c(which(rborist_dreaction != test_set$dreaction))
# [1] 138
tibble(rborist = rborist_dreaction[which_index], rpart = rpart_dreaction[which_index], qda = qda_dreaction[which_index], knn = knn_dreaction[which_index], test_set = test_set$dreaction[which_index]) |> print(n = 40)
# A tibble: 1 × 5
#   rborist rpart qda   knn   test_set
#   <fct>   <fct> <fct> <fct> <fct>
# 1 3       3     3     4     4

# the error cannot be improved with an ensemble



# we retrain and retest the Rborist model using the baseline CD4 count and RNA load only

set.seed(70, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(70) # if using R 3.5 or earlier
rborist_model1 <- train(dreaction ~ bcd4 + brna, train_set, method = "Rborist", tuneGrid = expand.grid(predFixed = seq(1, 2), minNode = seq(1, 4)))
ggplot(rborist_model1, highlight = TRUE)
rborist_model1
# Random Forest
#
# 844 samples
# 2 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 844, 844, 844, 844, 844, 844, ...
# Resampling results across tuning parameters:
#
#   predFixed  minNode  Accuracy  Kappa
# 1          1        0.776     0.676
# 1          2        0.779     0.679
# 1          3        0.777     0.677
# 1          4        0.780     0.680
# 2          1        0.757     0.650
# 2          2        0.758     0.651
# 2          3        0.759     0.653
# 2          4        0.760     0.653
#
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were predFixed = 1 and minNode = 4.

rborist_dreaction1 <- predict(rborist_model1, test_set)
confusionMatrix(rborist_dreaction1, test_set$dreaction)
# Confusion Matrix and Statistics
#
# Reference
# Prediction  1  2  3  4  5
# 1  1  0  0  0  0
# 2  1  7  3  0  0
# 3  3  5 29  4  0
# 4  0  1 17 79  0
# 5  0  0  0  2 60
#
# Overall Statistics
#
# Accuracy : 0.83
# 95% CI : (0.773, 0.878)
# No Information Rate : 0.401
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.752
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.20000   0.5385    0.592    0.929    1.000
# Specificity           1.00000   0.9799    0.926    0.858    0.987
# Pos Pred Value        1.00000   0.6364    0.707    0.814    0.968
# Neg Pred Value        0.98104   0.9701    0.883    0.948    1.000
# Prevalence            0.02358   0.0613    0.231    0.401    0.283
# Detection Rate        0.00472   0.0330    0.137    0.373    0.283
# Detection Prevalence  0.00472   0.0519    0.193    0.458    0.292
# Balanced Accuracy     0.60000   0.7592    0.759    0.894    0.993

# higher than the accuracies of the k-nearest neighbor model and the quadratic discriminant analysis model
# lower than the accuracies of the recursive partitioning and regression trees model and the Rborist model

