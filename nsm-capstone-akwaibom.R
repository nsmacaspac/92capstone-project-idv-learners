# ???
# NICELLE SERNADILLA MACASPAC
# ??? 2023
# RUNNING TIME: ??? minutes



# we import the dataset

options(timeout = 120) # timeout in seconds for some Internet operations
if(!file.exists("mmc1.xlsx")) download.file("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8142042/bin/mmc1.xlsx", "mmc1.xlsx")

if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
library(readxl)
dataset <- as.data.frame(read_xlsx("mmc1.xlsx", range = "N27:AB1083")) # range reads the Unique Records table, which is the combined version of the Individual Treatment Change Episodes table
head(dataset)
# PID SEX BCD4 FCD4 BRNA FRNA BWt(kg) FWt(kg)    DRUGCOMB       PR C1 C2 C3 C4 C5
# 1   1   F  148  106  3.0  1.3      42      43 TDF+3TC+EFV 53.56199  0  0  1  0  0
# 2   2   F  145  378  2.5  1.3      57      60 AZT+3TC+NVP 55.33422  0  0  0  1  0
# 3   3   M   78  131  4.1  1.7      70      75 AZT+3TC+NVP 50.00000  0  1  0  0  0
# 4   4   M  295  574  4.4  1.9      64      66 AZT+3TC+NVP 50.00000  0  0  1  0  0
# 5   5   F  397  792  1.9  1.3      52      55 AZT+3TC+NVP 76.00000  0  0  0  0  1
# 6   6   F  155  280  4.2  1.7      59      56 TDF+3TC+EFV 50.00000  0  1  0  0  0
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



# we render the dataset into tidy format and numeric

colnames(dataset) <- c("id", "sex", "bcd4", "fcd4", "brna", "frna", "bweight", "fweight", "drug", "response", "dinteraction1", "dinteraction2", "dinteraction3", "dinteraction4", "dinteraction5")
str(dataset)
# 'data.frame':	1056 obs. of  15 variables:
# $ id           : num  1 2 3 4 5 6 7 8 9 10 ...
# $ sex          : chr  "F" "F" "M" "M" ...
# $ bcd4         : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4         : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna         : num  3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ frna         : num  1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ bweight      : num  42 57 70 64 52 59 62 78 82 85 ...
# $ fweight      : num  43 60 75 66 55 56 60 68 82 80 ...
# $ drug         : chr  "TDF+3TC+EFV" "AZT+3TC+NVP" "AZT+3TC+NVP" "AZT+3TC+NVP" ...
# $ response     : num  53.6 55.3 50 50 76 ...
# $ dinteraction1: num  0 0 0 0 0 0 0 0 1 0 ...
# $ dinteraction2: num  0 0 1 0 0 1 0 0 0 0 ...
# $ dinteraction3: num  1 0 0 1 0 0 1 0 0 1 ...
# $ dinteraction4: num  0 1 0 0 0 0 0 1 0 0 ...
# $ dinteraction5: num  0 0 0 0 1 0 0 0 0 0 ...

sum(is.na(dataset))
# [1] 0

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # require() checks if the package exists
library(tidyverse)
dataset |>
  group_by(sex) |>
  summarize(n())
# A tibble: 2 × 2
# sex   `n()`
# <chr> <int>
# 1 F       704
# 2 M       352

dataset |>
  group_by(drug) |>
  summarize(n())
# A tibble: 3 × 2
# drug    `n()`
# <chr>       <int>
# 1 AZT+3TC+EFV    28
# 2 AZT+3TC+NVP   330
# 3 TDF+3TC+EFV   698

sum((dataset$dinteraction1 + dataset$dinteraction2 + dataset$dinteraction3 + dataset$dinteraction4 + dataset$dinteraction5) > 1 )
# [1] 0
sum(dataset$dinteraction1) +
  sum(dataset$dinteraction2) +
  sum(dataset$dinteraction3) +
  sum(dataset$dinteraction4) +
  sum(dataset$dinteraction5)
# [1] 1056
# there is only 1 drug interaction per row

dataset1 <- dataset |>
  mutate(sex = ifelse(sex == "F", 1, 2)) |>
  mutate(drug = case_when(drug == "AZT+3TC+EFV" ~ 1,
                          drug == "AZT+3TC+NVP" ~ 2,
                          drug == "TDF+3TC+EFV" ~ 3)) |> # relabels drugs as 1-3
  mutate(dinteraction = case_when(dinteraction1 == 1 ~ 1,
                                  dinteraction2 == 1 ~ 2,
                                  dinteraction3 == 1 ~ 3,
                                  dinteraction4 == 1 ~ 4,
                                  dinteraction5 == 1 ~ 5,)) |> # relabels drug interactions as 1-5 then merges them under column dinteraction
  select(-dinteraction1, -dinteraction2, -dinteraction3, -dinteraction4, -dinteraction5)
str(dataset1)
# 'data.frame':	1056 obs. of  11 variables:
# $ id          : num  1 2 3 4 5 6 7 8 9 10 ...
# $ sex         : num  1 1 2 2 1 1 1 2 1 2 ...
# $ bcd4        : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4        : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna        : num  3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ frna        : num  1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ bweight     : num  42 57 70 64 52 59 62 78 82 85 ...
# $ fweight     : num  43 60 75 66 55 56 60 68 82 80 ...
# $ drug        : num  3 2 2 2 2 3 2 3 2 2 ...
# $ response    : num  53.6 55.3 50 50 76 ...
# $ dinteraction: num  3 4 2 3 5 2 3 4 1 3 ...



# we examine dataset1

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
# drug          response      dinteraction
# Min.   :1.000   Min.   :30.00   Min.   :1.000
# 1st Qu.:2.000   1st Qu.:50.99   1st Qu.:3.000
# Median :3.000   Median :57.84   Median :4.000
# Mean   :2.634   Mean   :60.85   Mean   :3.859
# 3rd Qu.:3.000   3rd Qu.:71.00   3rd Qu.:5.000
# Max.   :3.000   Max.   :86.00   Max.   :5.000

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
corrplot(cor(dataset1), method = "square", diag = FALSE, addCoef.col = "gray", tl.col = "black", number.cex = 0.4, number.digits = 2)
# CD4 count is positively correlated with response and drug interaction
# RNA load is negatively correlated with response and drug interaction
# response is highly correlated with drug interaction as expected

dataset1 |>
  ggplot(aes(fcd4, frna, color = response)) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  xlab("Follow-up CD4 Count") +
  ylab("Follow-up RNA Load") +
  scale_color_gradient(name = "Response", low = "skyblue", high = "dodgerblue4") # reverses the color gradient
# CD4 count and response are negatively correlated with RNA load

dataset1 |>
  ggplot(aes(fcd4, frna, color = factor(dinteraction))) +
  geom_point() +
  geom_smooth(color = "black", size = 0.5, method = "lm", se = FALSE) +
  xlab("Follow-up CD4 Count") +
  ylab("Follow-up RNA Load") +
  scale_color_manual(name = "Drug Interaction", values = c("tomato4", "orange3", "yellow3", "green4", "dodgerblue4"))
# CD4 count and drug interaction are negatively correlated with RNA load



# we preprocess dataset1

dataset1 <- dataset1 |>
  select(bcd4, fcd4, brna, frna, dinteraction) |> # retains CD4 count and RNA load as predictors and dinteraction as outcome
  mutate(dinteraction = factor(dinteraction))
head(dataset1)
# bcd4 fcd4 brna frna dinteraction
# 1  148  106  3.0  1.3            3
# 2  145  378  2.5  1.3            4
# 3   78  131  4.1  1.7            2
# 4  295  574  4.4  1.9            3
# 5  397  792  1.9  1.3            5
# 6  155  280  4.2  1.7            2
str(dataset1)
# 'data.frame':	1056 obs. of  5 variables:
# $ bcd4        : num  148 145 78 295 397 155 303 370 210 120 ...
# $ fcd4        : num  106 378 131 574 792 280 679 615 242 278 ...
# $ brna        : num  3 2.5 4.1 4.4 1.9 4.2 4.2 5.1 5.1 2.7 ...
# $ frna        : num  1.3 1.3 1.7 1.9 1.3 1.7 1.3 1.7 4.1 1.7 ...
# $ dinteraction: Factor w/ 5 levels "1","2","3","4",..: 3 4 2 3 5 2 3 4 1 3 ...



#######################
#####     WIP     #####
#######################



options(digits = 3)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
set.seed(20, sample.kind = "Rounding") # if using R 3.6 or later # for reproducibility during assessment
# set.seed(20) # if using R 3.5 or earlier
test_index <- createDataPartition(dataset1$dinteraction, p = 0.1, list = FALSE) # maximizes the train set
train_set <- dataset1[-test_index,]
test_set <- dataset1[test_index,]



set.seed(30, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(30) # if using R 3.5 or earlier
knn <- train(dinteraction ~ ., train_set, method = "knn", tuneGrid = data.frame(k = seq(15, 30, 2)))
ggplot(knn, highlight = TRUE)
knn
# k-Nearest Neighbors
#
# 948 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 948, 948, 948, 948, 948, 948, ...
# Resampling results across tuning parameters:
#
#   k   Accuracy  Kappa
# 15  0.519     0.300
# 17  0.527     0.311
# 19  0.532     0.318
# 21  0.529     0.313
# 23  0.523     0.305
# 25  0.528     0.312
# 27  0.523     0.304
# 29  0.522     0.303
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 19.
knn_dinteraction <- predict(knn, test_set)
confusionMatrix(knn_dinteraction, test_set$dinteraction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#          1  0  0  0  0  0
#          2  1  1  0  0  0
#          3  2  6 12  3  1
#          4  0  0 12 25  6
#          5  0  0  1 15 23
#
# Overall Statistics
#
# Accuracy : 0.565
# 95% CI : (0.466, 0.66)
# No Information Rate : 0.398
# P-Value [Acc > NIR] : 0.00034
#
# Kappa : 0.368
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.0000  0.14286    0.480    0.581    0.767
# Specificity            1.0000  0.99010    0.855    0.723    0.795
# Pos Pred Value            NaN  0.50000    0.500    0.581    0.590
# Neg Pred Value         0.9722  0.94340    0.845    0.723    0.899
# Prevalence             0.0278  0.06481    0.231    0.398    0.278
# Detection Rate         0.0000  0.00926    0.111    0.231    0.213
# Detection Prevalence   0.0000  0.01852    0.222    0.398    0.361
# Balanced Accuracy      0.5000  0.56648    0.668    0.652    0.781



set.seed(40, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(40) # if using R 3.5 or earlier
rpart <- train(dinteraction ~ ., train_set, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(rpart, highlight = TRUE)
rpart
# CART
#
# 948 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 948, 948, 948, 948, 948, 948, ...
# Resampling results across tuning parameters:
#
#   cp    Accuracy  Kappa
# 0.00  0.974     0.963
# 0.01  0.974     0.963
# 0.02  0.959     0.941
# 0.03  0.942     0.917
# 0.04  0.924     0.891
# 0.05  0.904     0.861
# 0.06  0.878     0.823
# 0.07  0.868     0.806
# 0.08  0.860     0.795
# 0.09  0.853     0.784
# 0.10  0.845     0.771
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.01.
plot(rpart$finalModel, margin = 0.05)
text(rpart$finalModel, cex = 0.9)
rpart_dinteraction <- predict(rpart, test_set)
confusionMatrix(rpart_dinteraction, test_set$dinteraction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#          1  3  1  0  0  0
#          2  0  5  0  0  0
#          3  0  1 23  0  0
#          4  0  0  2 43  0
#          5  0  0  0  0 30
#
# Overall Statistics
#
# Accuracy : 0.963
# 95% CI : (0.908, 0.99)
# No Information Rate : 0.398
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.947
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            1.0000   0.7143    0.920    1.000    1.000
# Specificity            0.9905   1.0000    0.988    0.969    1.000
# Pos Pred Value         0.7500   1.0000    0.958    0.956    1.000
# Neg Pred Value         1.0000   0.9806    0.976    1.000    1.000
# Prevalence             0.0278   0.0648    0.231    0.398    0.278
# Detection Rate         0.0278   0.0463    0.213    0.398    0.278
# Detection Prevalence   0.0370   0.0463    0.222    0.417    0.278
# Balanced Accuracy      0.9952   0.8571    0.954    0.985    1.000



set.seed(50, sample.kind = "Rounding") # if using R 3.6 or later
# set.seed(50) # if using R 3.5 or earlier
rf <- train(dinteraction ~ ., train_set, method = "rf", tuneGrid = data.frame(mtry = seq(1, 4)))
ggplot(rf, highlight = TRUE)
rf
# Random Forest
#
# 948 samples
# 4 predictor
# 5 classes: '1', '2', '3', '4', '5'
#
# No pre-processing
# Resampling: Bootstrapped (25 reps)
# Summary of sample sizes: 948, 948, 948, 948, 948, 948, ...
# Resampling results across tuning parameters:
#
#   mtry  Accuracy  Kappa
# 2     0.982     0.974
# 3     0.982     0.974
# 4     0.980     0.972
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 2.
varImp(rf)
# rf variable importance
#
# Overall
# brna   100.0
# bcd4    84.3
# fcd4    31.1
# frna     0.0
rf_dinteraction <- predict(rf, test_set)
confusionMatrix(rf_dinteraction, test_set$dinteraction)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  1  2  3  4  5
#          1  3  0  0  0  0
#          2  0  6  0  0  0
#          3  0  1 25  0  0
#          4  0  0  0 43  0
#          5  0  0  0  0 30
#
# Overall Statistics
#
# Accuracy : 0.991
# 95% CI : (0.949, 1)
# No Information Rate : 0.398
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.987
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            1.0000   0.8571    1.000    1.000    1.000
# Specificity            1.0000   1.0000    0.988    1.000    1.000
# Pos Pred Value         1.0000   1.0000    0.962    1.000    1.000
# Neg Pred Value         1.0000   0.9902    1.000    1.000    1.000
# Prevalence             0.0278   0.0648    0.231    0.398    0.278
# Detection Rate         0.0278   0.0556    0.231    0.398    0.278
# Detection Prevalence   0.0278   0.0556    0.241    0.398    0.278
# Balanced Accuracy      1.0000   0.9286    0.994    1.000    1.000










