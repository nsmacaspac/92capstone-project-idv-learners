# MULTI-CLASS DISCRIMINATIVE CLASSIFICATION MODEL ON ANTIRETROVIRAL THERAPY REACTION AND FAILURE DEVELOPED ON THE UNIQUE RECORDS OF THE AKWA IBOM HIV DATABASE
# NICELLE SERNADILLA MACASPAC
# AUGUST 2023
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
#   vli ni
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



# we preprocess the tidy dataset

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
# fig2 in the Rmd file
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



# CD4 COUNT AND RNA LOAD



# we visualize the predictors and outcome of the preprocessed dataset

dataset2 |>
  ggplot(aes(brna, bcd4, color = dreaction)) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) + # adds a trend line
  scale_x_continuous("Baseline RNA Load", limits = c(100, 700)) +
  scale_y_continuous("Baseline CD4 Count", limits = c(0, 2000)) + # encompasses the full range of the axes of baseline and follow-up data for ease of comparison
  scale_color_discrete(name = "Drug Reaction") # fig3 in the Rmd file
# there is a general increase in CD4 count at follow-up

dataset2 |>
  ggplot(aes(frna, fcd4, color = dreaction)) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  scale_x_continuous("Follow-up RNA Load", limits = c(100, 700)) +
  scale_y_continuous("Follow-up CD4 Count", limits = c(0, 2000)) +
  scale_color_discrete(name = "Drug Reaction")
# there is a general decrease in RNA load at follow-up

# there are patients who retained a low CD4 count and high RNA load at follow-up



# we partition the preprocessed dataset

set.seed(20, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(20) # if using R 3.5 or earlier
# for reproducibility during assessment
train_index <- createDataPartition(dataset2$dreaction, p = 0.8, list = FALSE)
train_set <- dataset2[train_index,]
test_set <- dataset2[-train_index,]










###############
###   WIP   ###
###############



# we train and test the k-nearest neighbor model

set.seed(30, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(30) # if using R 3.5 or earlier
knn_model <- train(dreaction ~ ., train_set, method = "knn", tuneGrid = data.frame(k = seq(3, 9, 1))) # tunes neighbor number
ggplot(knn_model, highlight = TRUE)
knn_model
# k-Nearest Neighbors
#
# 847 samples
# 4 predictor
# 5 classes: 'ni', 'vli', 'li', 'hi_tf', 'vhi_tf'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 847, 847, 847, 847, 847, 847, ...
# Resampling results across tuning parameters:
#
# k  Accuracy  Kappa
# 3  0.804     0.719
# 4  0.797     0.710
# 5  0.799     0.711
# 6  0.797     0.709
# 7  0.796     0.707
# 8  0.798     0.710
# 9  0.798     0.710
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 3.

knn_dreaction <- predict(knn_model, test_set)
confusionMatrix(knn_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ni vli li hi_tf vhi_tf
#     ni     55   4  1     0      0
#     vli     4  75  5     1      0
#     li      0   5 41     2      0
#     hi_tf   0   0  1     8      0
#     vhi_tf  0   0  0     2      5
#
# Overall Statistics
#
# Accuracy : 0.88
# 95% CI : (0.829, 0.921)
# No Information Rate : 0.402
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.829
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: ni Class: vli Class: li Class: hi_tf Class: vhi_tf
# Sensitivity              0.932      0.893     0.854       0.6154        1.0000
# Specificity              0.967      0.920     0.957       0.9949        0.9902
# Pos Pred Value           0.917      0.882     0.854       0.8889        0.7143
# Neg Pred Value           0.973      0.927     0.957       0.9750        1.0000
# Prevalence               0.282      0.402     0.230       0.0622        0.0239
# Detection Rate           0.263      0.359     0.196       0.0383        0.0239
# Detection Prevalence     0.287      0.407     0.230       0.0431        0.0335
# Balanced Accuracy        0.949      0.906     0.905       0.8051        0.9951



# we train and test the recursive partitioning and regression trees model

set.seed(40, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(40) # if using R 3.5 or earlier
rpart_model <- train(dreaction ~ ., train_set, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))) # tunes tree complexity
ggplot(rpart_model, highlight = TRUE)
rpart_model
# CART
#
# 847 samples
# 4 predictor
# 5 classes: 'ni', 'vli', 'li', 'hi_tf', 'vhi_tf'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 847, 847, 847, 847, 847, 847, ...
# Resampling results across tuning parameters:
#
# cp    Accuracy  Kappa
# 0.00  0.961     0.944
# 0.01  0.959     0.942
# 0.02  0.950     0.929
# 0.03  0.923     0.890
# 0.04  0.907     0.866
# 0.05  0.898     0.853
# 0.06  0.888     0.838
# 0.07  0.875     0.819
# 0.08  0.866     0.805
# 0.09  0.856     0.790
# 0.10  0.839     0.763
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.

plot(rpart_model$finalModel, margin = 0.05) # margin adjusts the plot size
text(rpart_model$finalModel, cex = 0.8) # cex adjusts the label size

rpart_dreaction <- predict(rpart_model, test_set)
confusionMatrix(rpart_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ni vli li hi_tf vhi_tf
#     ni     57   0  0     0      0
#     vli     2  84  0     0      0
#     li      0   0 46     1      0
#     hi_tf   0   0  2    12      0
#     vhi_tf  0   0  0     0      5
#
# Overall Statistics
#
# Accuracy : 0.976
# 95% CI : (0.945, 0.992)
# No Information Rate : 0.402
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.966
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: ni Class: vli Class: li Class: hi_tf Class: vhi_tf
# Sensitivity              0.966      1.000     0.958       0.9231        1.0000
# Specificity              1.000      0.984     0.994       0.9898        1.0000
# Pos Pred Value           1.000      0.977     0.979       0.8571        1.0000
# Neg Pred Value           0.987      1.000     0.988       0.9949        1.0000
# Prevalence               0.282      0.402     0.230       0.0622        0.0239
# Detection Rate           0.273      0.402     0.220       0.0574        0.0239
# Detection Prevalence     0.273      0.411     0.225       0.0670        0.0239
# Balanced Accuracy        0.983      0.992     0.976       0.9564        1.0000

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
# 847 samples
# 4 predictor
# 5 classes: 'ni', 'vli', 'li', 'hi_tf', 'vhi_tf'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 847, 847, 847, 847, 847, 847, ...
# Resampling results across tuning parameters:
#
# predFixed  minNode  Accuracy  Kappa
# 1          1        0.964     0.949
# 1          2        0.965     0.950
# 1          3        0.964     0.948
# 1          4        0.963     0.947
# 2          1        0.975     0.964
# 2          2        0.975     0.964
# 2          3        0.975     0.964
# 2          4        0.975     0.964
# 3          1        0.977     0.967
# 3          2        0.977     0.967
# 3          3        0.977     0.967
# 3          4        0.977     0.966
# 4          1        0.977     0.968
# 4          2        0.977     0.968
# 4          3        0.977     0.967
# 4          4        0.977     0.967
#
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were predFixed = 4 and minNode = 1.

varImp(rborist_model)
# Rborist variable importance
#
#       Overall
# brna   100.0
# fcd4    44.9
# bcd4     7.5
# frna     0.0

# baseline RNA load is the most important predictor

rborist_dreaction <- predict(rborist_model, test_set)
confusionMatrix(rborist_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ni vli li hi_tf vhi_tf
#     ni     59   0  0     0      0
#     vli     0  84  0     0      0
#     li      0   0 46     0      0
#     hi_tf   0   0  2    13      0
#     vhi_tf  0   0  0     0      5
#
# Overall Statistics
#
# Accuracy : 0.99
# 95% CI : (0.966, 0.999)
# No Information Rate : 0.402
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.986
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: ni Class: vli Class: li Class: hi_tf Class: vhi_tf
# Sensitivity              1.000      1.000     0.958       1.0000        1.0000
# Specificity              1.000      1.000     1.000       0.9898        1.0000
# Pos Pred Value           1.000      1.000     1.000       0.8667        1.0000
# Neg Pred Value           1.000      1.000     0.988       1.0000        1.0000
# Prevalence               0.282      0.402     0.230       0.0622        0.0239
# Detection Rate           0.282      0.402     0.220       0.0622        0.0239
# Detection Prevalence     0.282      0.402     0.220       0.0718        0.0239
# Balanced Accuracy        1.000      1.000     0.979       0.9949        1.0000

# higher than the accuracies of the k-nearest neighbor model and the recursive partitioning and regression trees model



# we train and test the quadratic discriminant analysis model

set.seed(60, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(60) # if using R 3.5 or earlier
qda_model <- train(dreaction ~ ., train_set, method = "qda")
qda_model
# Quadratic Discriminant Analysis
#
# 847 samples
# 4 predictor
# 5 classes: 'ni', 'vli', 'li', 'hi_tf', 'vhi_tf'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 847, 847, 847, 847, 847, 847, ...
# Resampling results:
#
# Accuracy  Kappa
# 0.727     0.601

qda_dreaction <- predict(qda_model, test_set)
confusionMatrix(qda_dreaction, test_set$dreaction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction ni vli li hi_tf vhi_tf
#     ni     48   2  1     0      0
#     vli     9  67 16     0      0
#     li      2  15 31     4      0
#     hi_tf   0   0  0     8      0
#     vhi_tf  0   0  0     1      5
#
# Overall Statistics
#
# Accuracy : 0.761
# 95% CI : (0.697, 0.817)
# No Information Rate : 0.402
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.655
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: ni Class: vli Class: li Class: hi_tf Class: vhi_tf
# Sensitivity              0.814      0.798     0.646       0.6154        1.0000
# Specificity              0.980      0.800     0.870       1.0000        0.9951
# Pos Pred Value           0.941      0.728     0.596       1.0000        0.8333
# Neg Pred Value           0.930      0.855     0.892       0.9751        1.0000
# Prevalence               0.282      0.402     0.230       0.0622        0.0239
# Detection Rate           0.230      0.321     0.148       0.0383        0.0239
# Detection Prevalence     0.244      0.440     0.249       0.0383        0.0287
# Balanced Accuracy        0.897      0.799     0.758       0.8077        0.9975

# lower than the accuracies of the k-nearest neighbor model, recursive partitioning and regression trees model and the Rborist model



# we examine the errors of the Rborist model

which_index <- c(which(rborist_dreaction != test_set$dreaction))
# [1] 15 61
tibble(rborist = rborist_dreaction[which_index], rpart = rpart_dreaction[which_index], qda = qda_dreaction[which_index], knn = knn_dreaction[which_index], test_set = test_set$dreaction[which_index])
# A tibble: 2 × 5
# rborist rpart qda   knn   test_set
# <fct>   <fct> <fct> <fct> <fct>
# 1 hi_tf   hi_tf li    li    li
# 2 hi_tf   hi_tf li    li    li

# the error cannot be improved with an ensemble

