## Random effects analysis of embedding bias

## import data
path <- "path to data folder"
education <- read.csv(paste0(path,"education_bias.csv"))
wave1 <- read.csv(paste0(path,"wave1_bias.csv"))
wave2 <- read.csv(paste0(path,"wave2_bias.csv"))

## libraries
library(lme4)
library(tidyverse)
library(ggplot2)
library(merTools)

## EDUCATION--------------------------------------------------------------------

## check for NA
is.na(any(education))

## convert time and sample to factor
education$time_block <- as.factor(education$time_block)
education$sample_nr <- as.factor(education$sample_nr)

## plot raw values
education$timeplot <-  dplyr::recode(education$time_block, 
              '1' = "1619 - 1792", 
              '2' = "1792 - 1870", 
              '3' = "1870 - 1960", 
              '4' = "1960 - 1990")


ggplot(data=education, aes(x=timeplot, y=education_bias)) +
  geom_bin2d() +
  labs(#title="Embedding bias for education per time period",
       x="time period", y = "education embedding bias") +
  scale_fill_gradient(low = '#ffffff',
                      high ='#474747'
                      ) +
  theme_classic()


## model

# change reference level
education <- within(education, time_block <- relevel(time_block, ref = 2))

ed_model <- lmer(education_bias ~ time_block + (1 | sample_nr), data = education)
sum_ed <- summary(ed_model)
confint(ed_model, level = 0.99)

predictInterval(ed_model)   # for various model predictions, possibly with new data

REsim(ed_model)             # mean, median and sd of the random effect estimates
plotREsim(REsim(ed_model))# plot of above 2 lines

accounted_variance_ed = 0.00001237/(0.000006264+0.00001237)

tapply(education$education_bias, education$time_block, summary)

# Relative change in bias
t1 = sum_ed$coefficients[1]
t2 = sum_ed$coefficients[2]
t3 = sum_ed$coefficients[3]
t4 = sum_ed$coefficients[4]

(t1+t2)/t1
(t1+t3)/t1
(t1+t4)/t1
# WAVE 1------------------------------------------------------------------------

## check for NA
is.na(any(wave1))

## convert time and sample to factor
wave1$time_block <- as.factor(wave1$time_block)
wave1$sample_nr <- as.factor(wave1$sample_nr)

## rename bias column
wave1 <- rename(wave1, wave1_bias = education_bias)

## plot raw values
wave1$timeplot <-  dplyr::recode(wave1$time_block, 
                                     '1' = "1619 - 1792", 
                                     '2' = "1792 - 1870", 
                                     '3' = "1870 - 1960", 
                                     '4' = "1960 - 1990")


ggplot(data=wave1, aes(x=timeplot, y=wave1_bias)) +
  geom_bin2d() +
  labs(#title="Embedding bias for first feminist wave demands per time period",
       x="time period", y = "first wave demands embedding bias") +
  scale_fill_gradient(low = '#ffffff',
                      high ='#474747'
  ) +
  theme_classic()


## model

# change reference level
wave1 <- within(wave1, time_block <- relevel(time_block, ref = 3))

wave1_model <- lmer(wave1_bias ~ time_block + (1 | sample_nr), data = wave1)
sum1 <- summary(wave1_model)
confint(wave1_model,level = 0.99)

predictInterval(wave1_model)   # for various model predictions, possibly with new data

REsim(wave1_model)             # mean, median and sd of the random effect estimates
plotREsim(REsim(wave1_model))# plot of above 2 lines

accounted_variance_1 = 8.685/(8.685+4.168)

# Relative change in bias
t1 = sum1$coefficients[2]
t2 = sum1$coefficients[3]
t3 = sum1$coefficients[1]
t4 = sum1$coefficients[4]

(t1+t3)/t3
(t2+t3)/t3
(t4+t3)/t3

# WAVE 2------------------------------------------------------------------------

## check for NA
is.na(any(wave2))

## convert time and sample to factor
wave2$time_block <- as.factor(wave2$time_block)
wave2$sample_nr <- as.factor(wave2$sample_nr)

## rename bias column
wave2 <- rename(wave2, wave2_bias = education_bias)

## plot raw values
wave2$timeplot <-  dplyr::recode(wave2$time_block, 
                                 '1' = "1619 - 1792", 
                                 '2' = "1792 - 1870", 
                                 '3' = "1870 - 1960", 
                                 '4' = "1960 - 1990")


ggplot(data=wave2, aes(x=timeplot, y=wave2_bias)) +
  geom_bin2d() +
  labs(#title="Embedding bias for second feminist wave demands per time period",
       x="time period", y = "second wave demands embedding bias") +
  scale_fill_gradient(low = '#ffffff',
                      high ='#474747'
  ) +
  theme_classic()


## model
# change reference level
wave2 <- within(wave2, time_block <- relevel(time_block, ref = 4))

wave2_model <- lmer(wave2_bias ~ time_block + (1 | sample_nr), data = wave2)
sum2 <- summary(wave2_model)
confint(wave2_model, level=0.99)

predictInterval(wave2_model)   # for various model predictions, possibly with new data

REsim(wave1_model)             # mean, median and sd of the random effect estimates
plotREsim(REsim(wave2_model))# plot of above 2 lines

accounted_variance_2 = 5.474 / (5.474+5.277)

# Relative change in bias
t1 = sum2$coefficients[2]
t2 = sum2$coefficients[3]
t3 = sum2$coefficients[4]
t4 = sum2$coefficients[1]

(t1 + t4) / t4
(t2 + t4) / t4
(t3 + t4) / t4
