library(ggplot2)
library(stringr)
library(caret)
library(dplyr)


## Loading data!
df <- read.csv('Interview.csv')
## Making sure no data we need is missing
df <- train[!with(train, is.na(Expected.Attendance) | is.na(Position.to.be.closed)
                  | is.na(Observed.Attendance)),]
df <- df %>% select(Observed.Attendance, Expected.Attendance, Position.to.be.closed)
df <- df %>% 
  mutate(Observed.Attendance = tolower(Observed.Attendance)) %>%
  mutate(Observed.Attendance = str_trim(Observed.Attendance)) %>%
  filter(Observed.Attendance!="")
df$Observed.Attendance <- factor(df$Observed.Attendance)
levels(df$Observed.Attendance)

## 
df.colnames

## Splitting train and test sets
train <- sample_frac(df, 0.7)
sid <- as.numeric(rownames(train)) # because rownames() returns character
test <- df[-sid,]

train %>%  ggplot(aes(Observed.Attendance, fill=Observed.Attendance)) +
      geom_histogram(stat='count')
