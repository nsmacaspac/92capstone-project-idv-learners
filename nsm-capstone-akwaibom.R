# ???
# NICELLE SERNADILLA MACASPAC
# ??? 2023
# RUNNING TIME: ??? minutes



# we import the dataset

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(tidyverse)
options(timeout = 120) # timeout in seconds for some Internet operations
if(!file.exists("mmc1.xlsx")) download.file("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8142042/bin/mmc1.xlsx", "mmc1.xlsx")

if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(readxl)
dataset <- read_xlsx("mmc1.xlsx", range = "N27:AB1083")
head(dataset)
# A tibble: 6 × 15
# PID SEX    BCD4  FCD4  BRNA  FRNA `BWt(kg)` `FWt(kg)` DRUGCOMB       PR    C1
# <dbl> <chr> <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl> <chr>       <dbl> <dbl>
# 1  1051 F       520   531   2.6   2.4        87        68 TDF+3TC+EFV  71       0
# 2  1052 F       293   283   5.3   5.9        85        85 TDF+3TC+EFV  30       1
# 3  1053 F       382   385   4.6   6.2        79        83 TDF+3TC+EFV  35       0
# 4  1054 F       440   464   6.1   5.5        72        75 TDF+3TC+EFV  35       0
# 5  1055 M       341   361   4.9   5          70        72 TDF+3TC+EFV  31.9     0
# 6  1056 F       268   286   5.9   6.3        88        78 TDF+3TC+EFV  30       1
str(dataset)
# tibble [1,056 × 15] (S3: tbl_df/tbl/data.frame)
# $ PID     : num [1:1056] 1 2 3 4 5 6 7 8 9 10 ...
# $ SEX     : chr [1:1056] "F" "F" "M" "M" ...
# $ BCD4    : num [1:1056] 148 145 78 295 397 155 303 370 210 120 ...
# $ FCD4    : num [1:1056] 106 378 131 574 792 280 679 615 242 278 ...
# $ BRNA    : num [1:1056] 3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ FRNA    : num [1:1056] 1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ BWt(kg) : num [1:1056] 42 57 70 64 52 59 62 78 82 85 ...
# $ FWt(kg) : num [1:1056] 43 60 75 66 55 56 60 68 82 80 ...
# $ DRUGCOMB: chr [1:1056] "TDF+3TC+EFV" "AZT+3TC+NVP" "AZT+3TC+NVP" "AZT+3TC+NVP" ...
# $ PR      : num [1:1056] 53.6 55.3 50 50 76 ...
# $ C1      : num [1:1056] 0 0 0 0 0 0 0 0 1 0 ...
# $ C2      : num [1:1056] 0 0 1 0 0 1 0 0 0 0 ...
# $ C3      : num [1:1056] 1 0 0 1 0 0 1 0 0 1 ...
# $ C4      : num [1:1056] 0 1 0 0 0 0 0 1 0 0 ...
# $ C5      : num [1:1056] 0 0 0 0 1 0 0 0 0 0 ...


dataset |>
  group_by(SEX) |>
  summarize(n())
# A tibble: 2 × 2
# SEX   `n()`
# <chr> <int>
# 1 F       704
# 2 M       352
dataset |>
  group_by(DRUGCOMB) |>
  summarize(n())
# A tibble: 3 × 2
# DRUGCOMB    `n()`
# <chr>       <int>
# 1 AZT+3TC+EFV    28
# 2 AZT+3TC+NVP   330
# 3 TDF+3TC+EFV   698
dataset <- dataset |>
  mutate(SEX = ifelse(SEX == "F", 1, 2)) |>
  mutate(DRUGCOMB = case_when(DRUGCOMB == "AZT+3TC+EFV" ~ 1,
                              DRUGCOMB == "AZT+3TC+NVP" ~ 2,
                              DRUGCOMB == "TDF+3TC+EFV" ~ 3))

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(corrplot)
corrplot(cor(dataset), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "dodgerblue4", number.cex = 0.4, number.digits = 2)



#######################
#####     WIP     #####
#######################



# we render the dataset into tidy format and numeric

colnames(dataset) <- c("id", "limit", "sex", "education", "civil", "age", "status9", "status8", "status7", "status6", "status5", "status4", "bill9", "bill8", "bill7", "bill6", "bill5", "bill4", "payment9", "payment8", "payment7", "payment6", "payment5", "payment4", "default")
dataset <- dataset[-1,]

status_tibble <- dataset |>
  select("id", "limit", "sex", "education", "civil", "age", "status9", "status8", "status7", "status6", "status5", "status4") |>
  rename("9" = status9) |>
  rename("8" = status8) |>
  rename("7" = status7) |>
  rename("6" = status6) |>
  rename("5" = status5) |>
  rename("4" = status4) |>
  pivot_longer("9":"4", names_to = "month", values_to = "status")
head(status_tibble)
# A tibble: 6 × 8
#   id    limit sex   education civil age   month status
#   <chr> <chr> <chr> <chr>     <chr> <chr> <chr> <chr>
# 1 1     20000 2     2         1     24    9     2
# 2 1     20000 2     2         1     24    8     2
# 3 1     20000 2     2         1     24    7     -1
# 4 1     20000 2     2         1     24    6     -1
# 5 1     20000 2     2         1     24    5     -2
# 6 1     20000 2     2         1     24    4     -2

payment_tibble <- dataset |>
  select("payment9", "payment8", "payment7", "payment6", "payment5", "payment4") |>
  rename("9" = payment9) |>
  rename("8" = payment8) |>
  rename("7" = payment7) |>
  rename("6" = payment6) |>
  rename("5" = payment5) |>
  rename("4" = payment4) |>
  pivot_longer("9":"4", names_to = "month", values_to = "payment") |>
  select(payment)
head(payment_tibble)
# A tibble: 6 × 1
#   payment
#   <chr>
# 1 0
# 2 689
# 3 0
# 4 0
# 5 0
# 6 0

bill_tibble <- dataset |>
  select("bill9", "bill8", "bill7", "bill6", "bill5", "bill4", "default") |>
  rename("9" = bill9) |>
  rename("8" = bill8) |>
  rename("7" = bill7) |>
  rename("6" = bill6) |>
  rename("5" = bill5) |>
  rename("4" = bill4) |>
  pivot_longer("9":"4", names_to = "month", values_to = "bill") |>
  select(bill, default)
head(bill_tibble)
# A tibble: 6 × 2
#   bill  default
#   <chr> <chr>
# 1 3913  1
# 2 3102  1
# 3 689   1
# 4 0     1
# 5 0     1
# 6 0     1

dataset <- as.data.frame(bind_cols(status_tibble, payment_tibble, bill_tibble))
str(dataset)
# 'data.frame':	180000 obs. of  11 variables:
# $ id       : chr  "1" "1" "1" "1" ...
# $ limit    : chr  "20000" "20000" "20000" "20000" ...
# $ sex      : chr  "2" "2" "2" "2" ...
# $ education: chr  "2" "2" "2" "2" ...
# $ civil    : chr  "1" "1" "1" "1" ...
# $ age      : chr  "24" "24" "24" "24" ...
# $ month    : chr  "9" "8" "7" "6" ...
# $ status   : chr  "2" "2" "-1" "-1" ...
# $ payment  : chr  "0" "689" "0" "0" ...
# $ bill     : chr  "3913" "3102" "689" "0" ...
# $ default  : chr  "1" "1" "1" "1" ...

sum(is.na(dataset))
# [1] 0

# dataset <- as.numeric(dataset)
# Error: 'list' object cannot be coerced to type 'double'
is.list(dataset$ID)
# [1] FALSE
is.list(dataset[1])
# [1] TRUE
for(c in 1:11){
  dataset[c] <- as.numeric(unlist(dataset[c]))
}
head(dataset)
#   id limit sex education civil age month status payment bill default
# 1  1 20000   2         2     1  24     9      2       0 3913       1
# 2  1 20000   2         2     1  24     8      2     689 3102       1
# 3  1 20000   2         2     1  24     7     -1       0  689       1
# 4  1 20000   2         2     1  24     6     -1       0    0       1
# 5  1 20000   2         2     1  24     5     -2       0    0       1
# 6  1 20000   2         2     1  24     4     -2       0    0       1
str(dataset)
# 'data.frame':	180000 obs. of  11 variables:
# $ id       : num  1 1 1 1 1 1 2 2 2 2 ...
# $ limit    : num  20000 20000 20000 20000 20000 20000 120000 120000 120000 120000 ...
# $ sex      : num  2 2 2 2 2 2 2 2 2 2 ...
# $ education: num  2 2 2 2 2 2 2 2 2 2 ...
# $ civil    : num  1 1 1 1 1 1 2 2 2 2 ...
# $ age      : num  24 24 24 24 24 24 26 26 26 26 ...
# $ month    : num  9 8 7 6 5 4 9 8 7 6 ...
# $ status   : num  2 2 -1 -1 -2 -2 -1 2 0 0 ...
# $ payment  : num  0 689 0 0 0 0 0 1000 1000 1000 ...
# $ bill     : num  3913 3102 689 0 0 ...
# $ default  : num  1 1 1 1 1 1 1 1 1 1 ...



# we examine the dataset

summary(dataset)
# id            limit              sex          education
# Min.   :    1   Min.   :  10000   Min.   :1.000   Min.   :0.000
# 1st Qu.: 7501   1st Qu.:  50000   1st Qu.:1.000   1st Qu.:1.000
# Median :15000   Median : 140000   Median :2.000   Median :2.000
# Mean   :15000   Mean   : 167484   Mean   :1.604   Mean   :1.853
# 3rd Qu.:22500   3rd Qu.: 240000   3rd Qu.:2.000   3rd Qu.:2.000
# Max.   :30000   Max.   :1000000   Max.   :2.000   Max.   :6.000
# civil            age            month         status
# Min.   :0.000   Min.   :21.00   Min.   :4.0   Min.   :-2.0000
# 1st Qu.:1.000   1st Qu.:28.00   1st Qu.:5.0   1st Qu.:-1.0000
# Median :2.000   Median :34.00   Median :6.5   Median : 0.0000
# Mean   :1.552   Mean   :35.49   Mean   :6.5   Mean   :-0.1824
# 3rd Qu.:2.000   3rd Qu.:41.00   3rd Qu.:8.0   3rd Qu.: 0.0000
# Max.   :3.000   Max.   :79.00   Max.   :9.0   Max.   : 8.0000
# payment             bill            default
# Min.   :      0   Min.   :-339603   Min.   :0.0000
# 1st Qu.:    390   1st Qu.:   2400   1st Qu.:0.0000
# Median :   1900   Median :  19270   Median :0.0000
# Mean   :   5275   Mean   :  44977   Mean   :0.2212
# 3rd Qu.:   4592   3rd Qu.:  57417   3rd Qu.:0.0000
# Max.   :1684259   Max.   :1664089   Max.   :1.0000

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(corrplot)
corrplot(cor(dataset), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "dodgerblue4", number.cex = 0.4, number.digits = 2)

dataset |>
  group_by(education) |>
  summarize(n())
# A tibble: 7 × 2
#   education `n()`
#       <dbl> <int>
# 1         0    84
# 2         1 63510 # graduate school
# 3         2 84180 # university
# 4         3 29502 # high school
# 5         4   738 # others
# 6         5  1680
# 7         6   306

dataset |>
  group_by(civil) |>
  summarize(n())
# A tibble: 4 × 2
#   civil `n()`
#   <dbl> <int>
# 1     0   324
# 2     1 81954 # married
# 3     2 95784 # single
# 4     3  1938 # others

dataset |>
  group_by(status) |>
  summarize(n())
# A tibble: 11 × 2
#   status `n()`
#   <dbl> <int>
# 1     -2 24415
# 2     -1 34640 # pay duly
# 3      0 95919
# 4      1  3722 # payment delay for 1 month
# 5      2 18964 # payment delay for 2 month
# 6      3  1430 # payment delay for 3 month
# 7      4   453 # payment delay for 4 month
# 8      5   137 # payment delay for 5 month
# 9      6    74 # payment delay for 6 month
# 10      7   218 # payment delay for 7 month
# 11      8    28 # payment delay for 8 month



# we preprocess the dataset into dataset1

options(digits = 2)
dataset1 <- dataset |>
  mutate(education = ifelse(education %in% c(0, 4:6), 4, education)) |>
  mutate(civil = ifelse(civil == 0, 3, civil)) |>
  mutate(age = round(age, digits = -1)) |>
  mutate(status = ifelse(status %in% 4:8, 3, status)) |>
  mutate(payment = payment/limit) |>
  mutate(bill = bill/limit)

corrplot(cor(dataset1), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "dodgerblue4", number.cex = 0.4, number.digits = 2)



dataset1 <- dataset1 |>
  select(status, bill, default) |>
  mutate(default = as.factor(default))

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(caret)
set.seed(20, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(20) # if using R 3.5 or earlier
test_index <- createDataPartition(y = dataset1$default, times = 1, p = 0.2, list = FALSE)
train_set <- dataset1[-test_index,]
test_set <- dataset1[test_index,]



set.seed(30, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(30) # if using R 3.5 or earlier
glm <- train(default ~ status, method = "glm", data = train_set)
glm
# Generalized Linear Model
#
# 143999 samples
# 1 predictor
# 2 classes: '0', '1'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 143999, 143999, 143999, 143999, 143999, 143999, ...
# Resampling results:
#
#   Accuracy  Kappa
# 0.78      0.048
glm_default <- predict(glm, newdata = test_set)
confusionMatrix(glm_default, test_set$default)
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1
# 0 27865  7661
# 1   172   303
#
# Accuracy : 0.782
# 95% CI : (0.778, 0.787)
# No Information Rate : 0.779
# P-Value [Acc > NIR] : 0.0485
#
# Kappa : 0.048
#
# Mcnemar's Test P-Value : <2e-16
#
#             Sensitivity : 0.994
#             Specificity : 0.038
#          Pos Pred Value : 0.784
#          Neg Pred Value : 0.638
#              Prevalence : 0.779
#          Detection Rate : 0.774
#    Detection Prevalence : 0.987
#       Balanced Accuracy : 0.516
#
#        'Positive' Class : 0



set.seed(40, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(40) # if using R 3.5 or earlier
# tuneGrid = data.frame(k = seq(5, 10, 2))
# trControl = trainControl(method = "cv", number = 5, p = 0.8)
knn <- train(default ~ ., method = "knn", data = train_set)
ggplot(knn, highlight = TRUE)
knn
# k-Nearest Neighbors
#
# 23999 samples
# 12 predictor
# 2 classes: '0', '1'
#
# No pre-processing
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 19199, 19199, 19200, 19199, 19199
# Resampling results across tuning parameters:
#
#   k   Accuracy  Kappa
# 50  0.82      0.37
# 55  0.82      0.37
# 60  0.82      0.37
# 65  0.82      0.37
# 70  0.82      0.37
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 60.
knn_default <- predict(knn, newdata = test_set)
confusionMatrix(knn_default, test_set$default)
# Confusion Matrix and Statistics
#
# Reference
# Prediction    0    1
# 0 4450  850
# 1  223  478
#
# Accuracy : 0.821
# 95% CI : (0.811, 0.831)
# No Information Rate : 0.779
# P-Value [Acc > NIR] : 2.51e-16
#
# Kappa : 0.376
#
# Mcnemar's Test P-Value : < 2e-16
#
#             Sensitivity : 0.952
#             Specificity : 0.360
#          Pos Pred Value : 0.840
#          Neg Pred Value : 0.682
#              Prevalence : 0.779
#          Detection Rate : 0.742
#    Detection Prevalence : 0.883
#       Balanced Accuracy : 0.656
#
#        'Positive' Class : 0



set.seed(50, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(50) # if using R 3.5 or earlier
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(gam)
gamloess <- train(default ~ ., method = "gamLoess", trControl = trainControl(method = "cv", number = 5, p = 0.8), tuneGrid = expand.grid(span = seq(0.15, 0.65, len = 5), degree = 1), data = train_set)
ggplot(gamloess, highlight = TRUE)
gamloess
# Generalized Additive Model using LOESS
#
# 23999 samples
# 12 predictor
# 2 classes: '0', '1'
#
# No pre-processing
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 19200, 19199, 19199, 19199, 19199
# Resampling results across tuning parameters:
#
#   span  Accuracy  Kappa
# 0.15  0.71      0.183
# 0.28  0.81      0.289
# 0.40  0.81      0.285
# 0.53  0.81      0.282
# 0.65  0.55      0.066
#
# Tuning parameter 'degree' was held constant at a value of 1
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were span = 0.53 and degree = 1.
gamloess_default <- predict(gamloess, newdata = test_set)
confusionMatrix(gamloess_default, test_set$default)
# Confusion Matrix and Statistics
#
# Reference
# Prediction    0    1
# 0 4517  981
# 1  156  347
#
# Accuracy : 0.811
# 95% CI : (0.8, 0.82)
# No Information Rate : 0.779
# P-Value [Acc > NIR] : 8.19e-10
#
# Kappa : 0.293
#
# Mcnemar's Test P-Value : < 2e-16
#
#             Sensitivity : 0.967
#             Specificity : 0.261
#          Pos Pred Value : 0.822
#          Neg Pred Value : 0.690
#              Prevalence : 0.779
#          Detection Rate : 0.753
#    Detection Prevalence : 0.916
#       Balanced Accuracy : 0.614
#
#        'Positive' Class : 0



##########WIP
set.seed(60, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(60) # if using R 3.5 or earlier
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(randomForest)
rf <- train(default ~ ., method = "rf", data = train_set)
ggplot(rf, highlight = TRUE)
rf

rf_default <- predict(rf, newdata = test_set)
confusionMatrix(rf_default, test_set$default)

