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
# PID SEX BCD4 FCD4 BRNA FRNA BWt(kg) FWt(kg)    DRUGCOMB       PR C1 C2 C3 C4 C5
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
# id sex bcd4 fcd4 brna frna bweight fweight     therapy response vhi_tf hi_tf li
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
  mutate(reaction = case_when(vhi_tf == 1 ~ "vhi_tf",
                              hi_tf == 1 ~ "hi_tf",
                              li == 1 ~ "li",
                              vli == 1 ~ "vli",
                              ni == 1 ~ "ni")) |> # relabels drug reactions as vhi_tf to ni and merges them under a newly defined reaction column
  select(-vhi_tf, -hi_tf, -li, -vli, -ni)
head(dataset1, n = 5)
# id sex bcd4 fcd4 brna frna bweight fweight     therapy response reaction
# 1  1   F  148  106  300  130      42      43 TDF+3TC+EFV 53.56199       li
# 2  2   F  145  378  250  130      57      60 AZT+3TC+NVP 55.33422      vli
# 3  3   M   78  131  410  170      70      75 AZT+3TC+NVP 50.00000    hi_tf
# 4  4   M  295  574  440  190      64      66 AZT+3TC+NVP 50.00000       li
# 5  5   F  397  792  190  130      52      55 AZT+3TC+NVP 76.00000       ni
str(dataset1)
# 'data.frame':	1056 obs. of  11 variables:
# $ id      : num  1 2 3 4 5 6 7 8 9 10 ...
# $ sex     : chr  "F" "F" "M" "M" ...
# $ bcd4    : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4    : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna    : num  300 250 410 440 190 420 420 510 510 270 ...
# $ frna    : num  130 130 170 190 130 170 130 170 410 170 ...
# $ bweight : num  42 57 70 64 52 59 62 78 82 85 ...
# $ fweight : num  43 60 75 66 55 56 60 68 82 80 ...
# $ therapy : chr  "TDF+3TC+EFV" "AZT+3TC+NVP" "AZT+3TC+NVP" "AZT+3TC+NVP" ...
# $ response: num  53.6 55.3 50 50 76 ...
# $ reaction: chr  "li" "vli" "hi_tf" "li" ...



###############
###   WIP   ###
###############



# we preprocess dataset1




mutate(sex = ifelse(sex == "F", 1, 2)) |> # relabels sexes as 1-2
  mutate(therapy = case_when(therapy == "AZT+3TC+EFV" ~ 1,
                             therapy == "AZT+3TC+NVP" ~ 2,
                             therapy == "TDF+3TC+EFV" ~ 3)) |> # relabels antiretroviral therapies as 1-3







dataset |>
  group_by(sex) |>
  summarize(n())
# A tibble: 2 × 2
# sex   `n()`
# <chr> <int>
# 1 F       704
# 2 M       352

dataset |>
  group_by(therapy) |>
  summarize(n())
# A tibble: 3 × 2
# therapy    `n()`
# <chr>       <int>
# 1 AZT+3TC+EFV    28
# 2 AZT+3TC+NVP   330
# 3 TDF+3TC+EFV   698


summary(dataset1)
# id              sex             bcd4             fcd4
# Min.   :   1.0   Min.   :1.000   Min.   :   4.0   Min.   :  52.0
# 1st Qu.: 264.8   1st Qu.:1.000   1st Qu.: 238.0   1st Qu.: 422.8
# Median : 528.5   Median :1.000   Median : 378.5   Median : 639.0
# Mean   : 528.5   Mean   :1.333   Mean   : 436.1   Mean   : 658.5
# 3rd Qu.: 792.2   3rd Qu.:2.000   3rd Qu.: 590.0   3rd Qu.: 873.5
# Max.   :1056.0   Max.   :2.000   Max.   :1810.0   Max.   :2120.0
# brna            frna          bweight          fweight
# Min.   :1.200   Min.   :1.100   Min.   :  4.70   Min.   :  6.00 # refers to pediatric patients
# 1st Qu.:3.000   1st Qu.:1.400   1st Qu.: 56.00   1st Qu.: 57.00
# Median :3.850   Median :1.800   Median : 63.00   Median : 63.00
# Mean   :3.773   Mean   :2.164   Mean   : 63.66   Mean   : 64.34
# 3rd Qu.:4.700   3rd Qu.:2.500   3rd Qu.: 71.00   3rd Qu.: 72.00
# Max.   :6.500   Max.   :6.300   Max.   :125.00   Max.   :120.00
# therapy          response      reaction
# Min.   :1.000   Min.   :30.00   Min.   :1.000
# 1st Qu.:2.000   1st Qu.:50.99   1st Qu.:3.000
# Median :3.000   Median :57.84   Median :4.000
# Mean   :2.634   Mean   :60.85   Mean   :3.859
# 3rd Qu.:3.000   3rd Qu.:71.00   3rd Qu.:5.000
# Max.   :3.000   Max.   :86.00   Max.   :5.000

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
nearZeroVar(dataset1)
# integer(0)

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
corrplot(cor(dataset1), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "black", number.cex = 0.4, number.digits = 2) # shows the correlation of all variables
# CD4 count is positively correlated with response and drug reaction
# RNA load is negatively correlated with response and drug reaction
# response is highly correlated with drug reaction as expected

dataset1 |>
  ggplot(aes(bcd4, response, color = factor(reaction))) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Baseline CD4 Count", limits = c(0, 2000)) +
  scale_y_continuous("Response", limits = c(30, 110)) + # encompasses the limits of the axes of both baseline and follow-up data for ease of comparison
  scale_color_manual(name = "drug reaction", values = c("tomato4", "orange3", "yellow3", "green4", "dodgerblue4"))

dataset1 |>
  ggplot(aes(fcd4, response, color = factor(reaction))) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Follow-up CD4 Count", limits = c(0, 2000)) +
  scale_y_continuous("Response", limits = c(30, 110)) +
  scale_color_manual(name = "drug reaction", values = c("tomato4", "orange3", "yellow3", "green4", "dodgerblue4"))
# CD4 count is positively correlated with response and drug reaction
# there is a general increase in CD4 count at follow-up

dataset1 |>
  ggplot(aes(brna, response, color = factor(reaction))) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Baseline RNA Load", limits = c(1, 7)) +
  ylab("Response") +
  scale_color_manual(name = "drug reaction", values = c("tomato4", "orange3", "yellow3", "green4", "dodgerblue4"))

dataset1 |>
  ggplot(aes(frna, response, color = factor(reaction))) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Follow-up RNA Load", limits = c(1, 7)) +
  ylab("Response") +
  scale_color_manual(name = "drug reaction", values = c("tomato4", "orange3", "yellow3", "green4", "dodgerblue4"))
# RNA load is negatively correlated with response and drug reaction
# there is a general decrease in RNA load at follow-up

dataset2 <- dataset1 |>
  select(bcd4, fcd4, brna, frna, reaction) |> # keeps CD4 count and RNA load as predictors and drug reaction as outcome
  mutate(reaction = factor(reaction))
head(dataset2, n = 5)
# bcd4 fcd4 brna frna reaction
# 1  148  106  3.0  1.3            3
# 2  145  378  2.5  1.3            4
# 3   78  131  4.1  1.7            2
# 4  295  574  4.4  1.9            3
# 5  397  792  1.9  1.3            5
# 6  155  280  4.2  1.7            2
str(dataset2)
# 'data.frame':	1056 obs. of  5 variables:
# $ bcd4        : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4        : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna        : num  3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ frna        : num  1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ reaction: Factor w/ 5 levels "1","2","3","4",..: 3 4 2 3 5 2 3 4 1 3 ...



# we partition dataset2

options(digits = 3)
set.seed(20, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(20) # if using R 3.5 or earlier
# for reproducibility during assessment
test_index <- createDataPartition(dataset2$reaction, p = 0.2, list = FALSE)
train_set <- dataset2[-test_index,]
test_set <- dataset2[test_index,]



# we train and test the k-nearest neighbor model

set.seed(30, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(30) # if using R 3.5 or earlier
knn_model <- train(reaction ~ ., train_set, method = "knn", tuneGrid = data.frame(k = seq(10, 30, 1))) # tunes neighbor number
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

knn_reaction <- predict(knn_model, test_set)
confusionMatrix(knn_reaction, test_set$reaction)
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
rpart_model <- train(reaction ~ ., train_set, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))) # tunes tree complexity
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

rpart_reaction <- predict(rpart_model, test_set)
confusionMatrix(rpart_reaction, test_set$reaction)
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
rborist_model <- train(reaction ~ ., train_set, method = "Rborist", tuneGrid = expand.grid(predFixed = seq(1, 4), minNode = seq(1, 4))) # tunes combinations of predictor number and node size
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

rborist_reaction <- predict(rborist_model, test_set)
confusionMatrix(rborist_reaction, test_set$reaction)
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
qda_model <- train(reaction ~ ., train_set, method = "qda")
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

qda_reaction <- predict(qda_model, test_set)
confusionMatrix(qda_reaction, test_set$reaction)
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

which_index <- c(which(rborist_reaction != test_set$reaction))
# [1] 138
tibble(rborist = rborist_reaction[which_index], rpart = rpart_reaction[which_index], qda = qda_reaction[which_index], knn = knn_reaction[which_index], test_set = test_set$reaction[which_index]) |> print(n = 40)
# A tibble: 1 × 5
#   rborist rpart qda   knn   test_set
#   <fct>   <fct> <fct> <fct> <fct>
# 1 3       3     3     4     4

# the error cannot be improved with an ensemble



# we retrain and retest the Rborist model using the baseline CD4 count and RNA load only

set.seed(70, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(70) # if using R 3.5 or earlier
rborist_model1 <- train(reaction ~ bcd4 + brna, train_set, method = "Rborist", tuneGrid = expand.grid(predFixed = seq(1, 2), minNode = seq(1, 4)))
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

rborist_reaction1 <- predict(rborist_model1, test_set)
confusionMatrix(rborist_reaction1, test_set$reaction)
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

