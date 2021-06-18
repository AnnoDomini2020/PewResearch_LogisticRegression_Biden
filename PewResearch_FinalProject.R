#### Heading ####
#### "Predicting votes for Joe Biden in 2020 via logistic regression,"
#### by Andrew Infantino.
#### Programmed in R 3.6.2.
#### Completed June 4th, 2021.

#### Load libraries ####
library(tidyverse)
library(caret)
library(foreign)

#### Load data ####
# Note: 'ATP W75.sav' and 'ATP W78.sav' must be in working 
# directory for the following commands to succeed.
unzip(zipfile = "W75_Sep20", list = FALSE)
data <- read.spss("ATP W75.sav", to.data.frame = TRUE)  # Wave 75

unzip(zipfile = "W78_Nov20", list = FALSE)
data2 <- read.spss("ATP W78.sav", to.data.frame = TRUE) # Wave 78

#### Filter out non-voters ####
data <- data %>% 
  filter(PLAN1_W75 %in% c("Plan to vote", "already voted"))
data2 <- data2 %>%
  filter(VOTED_W78 == "I definitely voted in the 2020 presidential election")

#### Binarize votes/intentions ####
# Wave 75 presidential vote intentions: 1 for Biden, 0 for not Biden.
data <- data %>%
  filter(!is.na(VOTEGEN20_W75)) %>%  # Filter NA votes intentions.
  filter(is.na(VOTEGEN20_LEAN_W75) | # Accomodate for leans without eliminating
           VOTEGEN20_LEAN_W75 != "Refused") %>%           # certain intentions.
  mutate(BinaryVote = ifelse(VOTEGEN20_W75 %in% c("Donald Trump, the Republican",
                                                  "Jo Jorgensen, the Libertarian Party candidate",
                                                  "Howie Hawkins, the Green Party candidate"), 0,
                             ifelse(VOTEGEN20_LEAN_W75 %in% c("Lean more toward Donald Trump",
                                                              "Lean more toward Jo Jorgensen",
                                                              "Lean more toward Howie Hawkins",
                                                              "None/Other"), 0, 1)) %>% as.factor())

# Wave 78 presidential vote: 1 for Biden. 0 for not Biden.
data2 <- data2 %>%
  filter(VOTEGEN_POST_W78 != "Refused") %>%
  mutate(BinaryVote = as.factor(ifelse(VOTEGEN_POST_W78 == "Joe Biden, the Democrat", 1, 0)))

#### Coin-flip algorithm ####
# W75: 50.3% accuracy.
set.seed(1, sample.kind = "Rounding") # Yield consistent results.
cf_array <- as.factor(sample(x = 0:1, size = length(data$BinaryVote), replace = TRUE))
confusionMatrix(cf_array, data$BinaryVote)$overall["Accuracy"]

# W78: 50.1% accuracy.
set.seed(1, sample.kind = "Rounding") # Yield consistent results.
cf_array2 <- as.factor(sample(x = 0:1, size = length(data2$BinaryVote), replace = TRUE))
confusionMatrix(cf_array2, data2$BinaryVote)$overall["Accuracy"]

#### Streamline political variables ####
# Satisfaction w/ country: (0 for dissatisfied, 1 for satisfied)
data %>% group_by(SATIS_W75) %>% summarize(n())
data <- data %>%
  filter(SATIS_W75 != "Refused") %>%
  mutate(SATIS = SATIS_W75)

data2 %>% group_by(SATIS_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(SATIS_W78 != "Refused") %>%
  mutate(SATIS = SATIS_W78)

# Evaluation of contemporary economy:
data %>% group_by(ECON1_W75) %>% summarize(n())
data <- data %>%
  filter(ECON1_W75 != "Refused") %>%
  mutate(ECON1 = ECON1_W75)

data2 %>% group_by(ECON1_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(ECON1_W78 != "Refused") %>%
  mutate(ECON1 = ECON1_W78)

# Evaluation of future economy:
data %>% group_by(ECON1B_W75) %>% summarize(n())
data <- data %>%
  filter(ECON1B_W75 != "Refused") %>%
  mutate(ECON2 = ECON1B_W75)

data2 %>% group_by(ECON1B_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(ECON1B_W78 != "Refused") %>%
  mutate(ECON2 = ECON1B_W78)

# Fearful for country: (1 for fearful, 0 for not fearful) (Form 2 for W75)
data %>% group_by(FEEL_COUNTRY_FEAR_W75) %>% summarize(n())
data <- data %>%
  filter(is.na(FEEL_COUNTRY_FEAR_W75) | # Keep form 1 responses.
           FEEL_COUNTRY_FEAR_W75 != "Refused") %>%
  mutate(FEAR = FEEL_COUNTRY_FEAR_W75)

data2 %>% group_by(FEEL_COUNTRY_FEAR_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(FEEL_COUNTRY_FEAR_W78 != "Refused") %>%
  mutate(FEAR = FEEL_COUNTRY_FEAR_W78)

# Hopeful for country: (1 for hopeful, 0 for not hopeful) (Form 2 for W75)
data %>% group_by(FEEL_COUNTRY_HOPEFUL_W75) %>% summarize(n())
data <- data %>%
  filter(is.na(FEEL_COUNTRY_HOPEFUL_W75) | # Keep form 1 responses.
           FEEL_COUNTRY_HOPEFUL_W75 != "Refused") %>%
  mutate(HOPE = FEEL_COUNTRY_HOPEFUL_W75)

data2 %>% group_by(FEEL_COUNTRY_HOPEFUL_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(FEEL_COUNTRY_HOPEFUL_W78 != "Refused") %>%
  mutate(HOPE = FEEL_COUNTRY_HOPEFUL_W78)

# Community-level electoral administration: (Form 2 for W75)
data %>% group_by(VTADMIN_COM_W75) %>% summarize(n())
data <- data %>%
  filter(is.na(VTADMIN_COM_W75) |
           VTADMIN_COM_W75 != "Refused") %>%
  mutate(VTADMIN_COM = VTADMIN_COM_W75)

data2 %>% group_by(VTADMIN_POST_COM_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(is.na(VTADMIN_POST_COM_W78) |
           VTADMIN_POST_COM_W78 != "Refused") %>%
  mutate(VTADMIN_COM = VTADMIN_POST_COM_W78)

# US-level electoral administration: (Form 2 for W75)
data %>% group_by(VTADMIN_US_W75) %>% summarize(n())
data <- data %>%
  filter(is.na(VTADMIN_US_W75) |
           VTADMIN_US_W75 != "Refused") %>%
  mutate(VTADMIN_US = VTADMIN_US_W75)

data2 %>% group_by(VTADMIN_POST_US_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(is.na(VTADMIN_POST_US_W78) |
           VTADMIN_POST_US_W78 != "Refused") %>%
  mutate(VTADMIN_US = VTADMIN_POST_US_W78)

# Expected difficulty of voting: (Form 2 for W75)
data %>% group_by(VTEASY_W75) %>% summarize(n())
data <- data %>%
  filter(is.na(VTEASY_W75) |
           VTEASY_W75 != "Refused") %>%
  mutate(VTEASY = VTEASY_W75)

data2 %>% group_by(VTEASY_POST_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(is.na(VTEASY_POST_W78) |
           VTEASY_POST_W78 != "Refused") %>%
  mutate(VTEASY = VTEASY_POST_W78)

#### Political algorithm ####
# Wave 75: 78.4% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 +       # Uniform items
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US + # Form 2 items
                 VTADMIN_COM + VTADMIN_US + VTEASY,       # Form 2 items
               data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# Wave 78: 90.8% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index2,]
test2 <- data2[test_index2,]
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 +               # Uniform
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US + VTEASY, # Form 2
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# Inter-wave: 78.4% accuracy.
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 +               # Uniform items
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US + VTEASY, # Form 2 items
               data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Streamline demographic variables ####
# Metropolitan status:
data %>% group_by(F_METRO) %>% summarize(n())
data2 %>% group_by(F_METRO) %>% summarize(n())

# Census division:
data %>% group_by(F_CDIVISION) %>% summarize(n())
data2 %>% group_by(F_CDIVISION) %>% summarize(n())

# Age category:
data %>% group_by(F_AGECAT) %>% summarize(n())
data2 %>% group_by(F_AGECAT) %>% summarize(n())
data <- data %>%
  filter(F_AGECAT != "Refused")
data2 <- data2 %>%
  filter(F_AGECAT != "Refused")

# Gender:
data %>% group_by(F_GENDER) %>% summarize(n())
data2 %>% group_by(F_GENDER) %>% summarize(n())
data <- data %>%
  filter(F_GENDER != "Refused")
data2 <- data2 %>%
  filter(F_GENDER != "Refused")

# Education:
data %>% group_by(F_EDUCCAT) %>% summarize(n())
data2 %>% group_by(F_EDUCCAT) %>% summarize(n())
data <- data %>%
  filter(F_EDUCCAT != "Refused")
data2 <- data2 %>%
  filter(F_EDUCCAT != "Refused")

# Race and Hispanic origin:
data %>% group_by(F_RACETHNMOD) %>% summarize(n())
data2 %>% group_by(F_RACETHNMOD) %>% summarize(n())
data <- data %>%
  filter(F_RACETHNMOD != "Refused")
data2 <- data2 %>%
  filter(F_RACETHNMOD != "Refused")

# Birthplace:
data %>% group_by(F_BIRTHPLACE2) %>% summarize(n())
data2 %>% group_by(F_BIRTHPLACE2) %>% summarize(n())
data <- data %>%
  filter(F_BIRTHPLACE2 != "Refused")
data2 <- data2 %>%
  filter(F_BIRTHPLACE2 != "Refused")

# Religion:
data %>% group_by(F_RELIG) %>% summarize(n())
data2 %>% group_by(F_RELIG) %>% summarize(n())
data <- data %>%
  filter(F_RELIG != "Refused")
data2 <- data2 %>%
  filter(F_RELIG != "Refused")

# Internet use frequency:
data %>% group_by(F_INTFREQ) %>% summarize(n())
data2 %>% group_by(F_INTFREQ) %>% summarize(n())
data <- data %>%
  filter(!is.na(F_INTFREQ)) %>%
  filter(F_INTFREQ != "Refused")
data2 <- data2 %>%
  filter(!is.na(F_INTFREQ)) %>%
  filter(F_INTFREQ != "Refused")

# Device used:
data %>% group_by(DEVICE_TYPE_W75) %>% summarize(n())
data2 %>% group_by(DEVICE_TYPE_W78) %>% summarize(n())
data <- data %>%
  mutate(DEVICE_TYPE = DEVICE_TYPE_W75)
data2 <- data2 %>%
  mutate(DEVICE_TYPE = DEVICE_TYPE_W78)

# Partisanship and ideology:
data %>% group_by(F_PARTYSUMIDEO) %>% summarize(n())
data2 %>% group_by(F_PARTYSUMIDEO) %>% summarize(n())
data <- data %>%
  filter(F_PARTYSUMIDEO != "Refused either F_IDEO or F_PARTYSUM_FINAL")
data2 <- data2 %>%
  filter(F_PARTYSUMIDEO != "Refused either F_IDEO or F_PARTYSUM_FINAL")

#### Demographic algorithm ####
# W75: 72.2% accuracy. (No political info.)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ F_METRO + F_CDIVISION + F_AGECAT +
                 F_GENDER + F_EDUCCAT + F_RACETHNMOD + F_BIRTHPLACE2 +
                 F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# W78: 73.4% accuracy. (No political info.)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ F_METRO + F_CDIVISION + F_AGECAT +
                 F_GENDER + F_EDUCCAT + F_RACETHNMOD + F_BIRTHPLACE2 +
                 F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# Inter-wave: 72.6% accuracy. (No political info.)
fit_glm <- glm(BinaryVote ~ F_METRO + F_CDIVISION + F_AGECAT +
                 F_GENDER + F_EDUCCAT + F_RACETHNMOD + F_BIRTHPLACE2 +
                 F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Political algorithm 2 ####
# W75: 93.3% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO, data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# W78: 93.8% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# Inter-wave: 94.2% accuracy.
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO, data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Combined algorithm ####
# Partisanship, ideology, and demographics (W75): 93.4% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# '' (W78): 93.7% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# '' (Interwave): 94.2% accuracy.
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Combined algorithm 2 ####
# Partisanship, ideology, and other political variables (W75): 93.0% accuracy
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY, 
               data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# '' (W78): 94.6% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY, 
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# '' (Interwave): 94.3% accuracy.
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY, 
               data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Combined algorithm 3 ####
# All variables (W75): 92.1% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$BinaryVote, times = 1, p = 0.15, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train, family = "binomial")
p_hat_glm <- predict(fit_glm, test, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test$BinaryVote)$overall["Accuracy"]

# '' (W78): 94.8% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# '' (Interwave): 94.3% accuracy.
fit_glm <- glm(BinaryVote ~ F_PARTYSUMIDEO + SATIS + ECON1 + ECON2 + 
                 FEAR + HOPE + VTADMIN_COM + VTADMIN_US +
                 VTEASY + VTADMIN_COM + VTADMIN_US + VTEASY + F_METRO + F_CDIVISION + 
                 F_AGECAT + F_GENDER + F_EDUCCAT + F_RACETHNMOD + 
                 F_BIRTHPLACE2 + F_RELIG + F_INTFREQ + DEVICE_TYPE, 
               data = data, family = "binomial")
p_hat_glm <- predict(fit_glm, data2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, data2$BinaryVote)$overall["Accuracy"]

#### Streamline political variables 2 (W78 only) ####
# In-person voting confidence:
data2 %>% group_by(VTCOUNT_POST_INP_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(is.na(VTCOUNT_POST_INP_W78) |
           VTCOUNT_POST_INP_W78 != "Refused") %>%
  mutate(VTCOUNT_INP = VTCOUNT_POST_INP_W78)

# Absentee voting confidence:
data2 %>% group_by(VTCOUNT_POST_ABS_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(is.na(VTCOUNT_POST_ABS_W78) |
           VTCOUNT_POST_ABS_W78 != "Refused") %>%
  mutate(VTCOUNT_ABS = VTCOUNT_POST_ABS_W78)

# Confidence in Trump over pandemic:
data2 %>% group_by(DTCONF_a_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(DTCONF_a_W78 != "Refused") %>%
  mutate(DT_COVID = DTCONF_a_W78)

# Confidence in Biden over pandemic:
data2 %>% group_by(JBCONF_a_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(JBCONF_a_W78 != "Refused") %>%
  mutate(JB_COVID = JBCONF_a_W78)

# Confidence in Trump over economy:
data2 %>% group_by(DTCONF_c_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(DTCONF_c_W78 != "Refused") %>%
  mutate(DT_ECON = DTCONF_c_W78)

# Confidence in Biden over economy:
data2 %>% group_by(JBCONF_c_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(JBCONF_c_W78 != "Refused") %>%
  mutate(JB_ECON = JBCONF_c_W78)

# Feeling angry about the country:
data2 %>% group_by(FEEL_COUNTRY_ANGRY_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(FEEL_COUNTRY_ANGRY_W78 != "Refused") %>%
  mutate(ANGER = FEEL_COUNTRY_ANGRY_W78)

# Feeling proud of country:
data2 %>% group_by(FEEL_COUNTRY_PROUD_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(FEEL_COUNTRY_PROUD_W78 != "Refused") %>%
  mutate(PRIDE = FEEL_COUNTRY_PROUD_W78)

# Satisfaction with candidates:
data2 %>% group_by(SATCAND_POST_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(SATCAND_POST_W78 != "Refused") %>%
  mutate(SATCAND = SATCAND_POST_W78)

# Favorite platform for tracking election results:
data2 %>% group_by(ELECTRESULTPLAT_W78) %>% summarize(n())
`%!in%` <- Negate(`%in%`)
data2 <- data2 %>%
  filter(ELECTRESULTPLAT_W78 %!in% c("Refused", "Did not follow election results",
                                     "'No answer to following election results'") ) %>%
  mutate(ELECTPLAT = ELECTRESULTPLAT_W78)

# COVID-19 assistance packages:
data2 %>% group_by(COVID_2ASSISTLD_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(COVID_2ASSISTLD_W78 != "Refused") %>%
  mutate(COVID_ASSIST = COVID_2ASSISTLD_W78)

# Participation in online rallies:
data2 %>% group_by(CIVENG_POST_ONLRLY_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(CIVENG_POST_ONLRLY_W78 != "Refused") %>%
  mutate(RALLY = CIVENG_POST_ONLRLY_W78)

# Political engagement on social media:
data2 %>% group_by(CIVENG_POST_SM_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(CIVENG_POST_SM_W78 != "Refused") %>%
  mutate(SOCIAL = CIVENG_POST_SM_W78)

# Lockdown restrictions:
data2 %>% group_by(COVID_OPENMORE_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(COVID_OPENMORE_W78 != "Refused") %>%
  mutate(LOCKDOWN = COVID_OPENMORE_W78)

# Belief that citizens influence government:
data2 %>% group_by(CITIZ_INFL_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(CITIZ_INFL_W78 != "Refused") %>%
  mutate(CITIZEN = CITIZ_INFL_W78)

# Confidence that vote will be counted:
data2 %>% group_by(VTCOUNT_OWN_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(VTCOUNT_OWN_W78 != "Refused") %>%
  mutate(VOTECOUNT = VTCOUNT_OWN_W78)

# Expectations of future polarization:
data2 %>% group_by(POL12_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(POL12_W78 != "Refused") %>%
  mutate(POL1 = POL12_W78)

# Evaluation of current polarization:
data2 %>% group_by(DIVISIONSRD_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(DIVISIONSRD_W78 != "Refused") %>%
  mutate(POL2 = DIVISIONSRD_W78)

# Concern over polarization:
data2 %>% group_by(DIVISIONSCONC_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(DIVISIONSCONC_W78 != "Refused") %>%
  mutate(POL3 = DIVISIONSCONC_W78)

# Belief that more/fewer people should vote:
data2 %>% group_by(VOTELIST_US_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(VOTELIST_US_W78 != "Refused") %>%
  mutate(VOTEMORE = VOTELIST_US_W78)

# Feeling guilt over not voting:
data2 %>% group_by(VOTELIST_GUILT_W78) %>% summarize(n())
data2 <- data2 %>%
  filter(VOTELIST_GUILT_W78 != "Refused") %>%
  mutate(VOTEGUILT = VOTELIST_GUILT_W78)

#### Political algorithm 3 (W78 only) ####
# Without ideology or partisanship: 95.8% accuracy
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 + # Satisfaction with country and economy.
                 JB_COVID + DT_COVID +              # Coronavirus convidence.
                 JB_ECON + DT_ECON +                # Economic confidence.
                 FEAR + HOPE + ANGER + PRIDE +      # Feelings about country.
                 SATCAND +                          # Satisfaction with candidates.
                 ELECTPLAT +                        # Favorite platform for election results.
                 COVID_ASSIST +                     # Assistance packagees.
                 RALLY + SOCIAL +                   # Online civic engagement.
                 LOCKDOWN +                         # Lockdown restrictions.
                 CITIZEN + VOTECOUNT +              # Voter efficacy.
                 POL1 + POL2 + POL3 +               # Views on polarization.
                 VOTEMORE + VOTEGUILT,              # Views on civic engagement.
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# With ideology and partisanship: 96.6% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 + # Satisfaction with country and economy.
                 JB_COVID + DT_COVID +              # Coronavirus convidence.
                 JB_ECON + DT_ECON +                # Economic confidence.
                 FEAR + HOPE + ANGER + PRIDE +      # Feelings about country.
                 SATCAND +                          # Satisfaction with candidates.
                 ELECTPLAT +                        # Favorite platform for election results.
                 COVID_ASSIST +                     # Assistance packagees.
                 RALLY + SOCIAL +                   # Online civic engagement.
                 LOCKDOWN +                         # Lockdown restrictions.
                 CITIZEN + VOTECOUNT +              # Voter efficacy.
                 POL1 + POL2 + POL3 +               # Views on polarization.
                 VOTEMORE + VOTEGUILT +             # Views on civic engagement.
                 F_PARTYSUMIDEO,                    # Partisanship and ideology
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]

# With ideology, partisanship, and demographics: 96.5% accuracy.
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = data2$BinaryVote, times = 1, p = 0.15, list = FALSE)
train2 <- data2[-test_index,]
test2 <- data2[test_index,]
fit_glm <- glm(BinaryVote ~ SATIS + ECON1 + ECON2 + # Satisfaction with country and economy.
                 JB_COVID + DT_COVID +              # Coronavirus convidence.
                 JB_ECON + DT_ECON +                # Economic confidence.
                 FEAR + HOPE + ANGER + PRIDE +      # Feelings about country.
                 SATCAND +                          # Satisfaction with candidates.
                 ELECTPLAT +                        # Favorite platform for election results.
                 COVID_ASSIST +                     # Assistance packagees.
                 RALLY + SOCIAL +                   # Online civic engagement.
                 LOCKDOWN +                         # Lockdown restrictions.
                 CITIZEN + VOTECOUNT +              # Voter efficacy.
                 POL1 + POL2 + POL3 +               # Views on polarization.
                 VOTEMORE + VOTEGUILT +             # Views on civic engagement.
                 F_PARTYSUMIDEO +                   # Partisanship and ideology.
                 F_METRO + F_CDIVISION + F_AGECAT + # Metropolitan status, census division, age group.
                 F_GENDER + F_EDUCCAT +             # Gender and education.
                 F_RACETHNMOD + F_BIRTHPLACE2 +     # Race, Hispanic origin and birthplace.
                 F_RELIG + F_INTFREQ + DEVICE_TYPE, # Religion, internet use frequency, and electronic device used.
               data = train2, family = "binomial")
p_hat_glm <- predict(fit_glm, test2, type = "response")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(y_hat_glm, test2$BinaryVote)$overall["Accuracy"]