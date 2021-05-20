## Import data
data_path = "" # Define file folder of your data
block1 <- read.csv(paste0(data_path,"1618_1791_preprocessed.csv"))
block2 <- read.csv(paste0(data_path,"1792_1870_preprocessed.csv"))
block3 <- read.csv(paste0(data_path,"1870_1960_preprocessed.csv"))
block4 <- read.csv(paste0(data_path,"1960_1990_preprocessed.csv"))

## libraries
library(dplyr)
library(ggplot2)
library(stringr)

## Make histograms of the data
class(test$date)

# keep only the year of the date
test$year <- str_sub(test$date,end=4)
block1$year <- str_sub(block1$date,end=4)
block2$year <- str_sub(block2$date,end=4)
block3$year <- str_sub(block3$date,end=4)
block4$year <- str_sub(block4$date,end=4)

# remove all unnecessary columns
block1 <- as.data.frame(as.numeric(block1$year), col.names = c("year"))
block1 <- rename(block1, 'year' = "as.numeric(block1$year)")

block2 <- as.data.frame(as.numeric(block2$year), col.names = c("year"))
block2 <- rename(block2, 'year' = "as.numeric(block2$year)")

block3 <- as.data.frame(as.numeric(block3$year), col.names = c("year"))
block3 <- rename(block3, 'year' = "as.numeric(block3$year)")

block4 <- as.data.frame(as.numeric(block4$year), col.names = c("year"))
block4 <- rename(block4, 'year' = "as.numeric(block4$year)")

# Merge dataframes
df = rbind(block1, block2, block3, block4)

ggplot(df, aes(x = year)) +
  geom_histogram(binwidth = 5) +
  theme_classic() +
  labs(y = "Amount of newspaper articles",
       x = "Year of publication")
