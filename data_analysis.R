update.packages()
install.packages('tidymodels')
install.packages('dplyr')
install.packages('e1071', dependencies=TRUE)
install.packages("ggplot2")

library(stringr) # Needed to do manipulation on strings
library(caret) # Needed
library(dplyr) # data manipulation lib
library(tidymodels) # Optional, gives better models such as logistic regression (not used yet)
## Loading data!
df <- read.csv('data/Interview.csv')
## Making sure no data we need is missing
df <- train[!with(train, is.na(Expected.Attendance) | is.na(Position.to.be.closed)
                  | is.na(Observed.Attendance)),]
df <- df %>% select(Observed.Attendance, Expected.Attendance, Position.to.be.closed)
df <- df %>% 
        mutate(Observed.Attendance = tolower(Observed.Attendance)) %>% # turn 'Yes ' into 'yes '
        mutate(Observed.Attendance = str_trim(Observed.Attendance)) %>%
        filter(Observed.Attendance!="")
df$Observed.Attendance <- factor(df$Observed.Attendance)
levels(df$Observed.Attendance)

## Let's check the different values in Expected.Attendance
levels(df$Expected.Attendance)
## We can see that there is bad quality data. 'Yes', exist as 'yes', 'Yes ' and 'yes '
## Let's correct that.
df <- df %>% 
  mutate(Expected.Attendance = tolower(Expected.Attendance)) %>%
  mutate(Expected.Attendance = str_trim(Expected.Attendance)) %>% # Remove whitespace at end of sentence
  filter(Expected.Attendance!="")

df$Expected.Attendance <- factor(df$Expected.Attendance)
levels(df$Expected.Attendance)
## Are there any missing value? Let's ask


## Splitting train and test sets
train <- sample_frac(df, 0.7)
sid <- as.numeric(rownames(train)) # because rownames() returns character
test <- df[-sid,]
 
## Refactoring the data
train$Observed.Attendance <- factor(train$Observed.Attendance)
levels(train$Observed.Attendance)

## We try to predict Observed.Attendance, so we are isolating it
## We also need to change 'Yes' into 1 and 'No' into 0. So we are using `ifelse` 
## function.

y_train = train %>% select(Observed.Attendance)
y_train <- ifelse(y_train=='yes', 1, 0)
length(train$Observed.Attendance) == length(y_train)

y_test = test %>% select(Observed.Attendance)
y_test <- ifelse(y_test=='yes', 1, 0)
length(y_test)

## Bonus point: We have splitted the data into two. But do we have the same
## proportion of 0 and 1 in each split (train and test)?

### TODO: Do a visualization about it.

## Define a glm w/ the binomial data type, get a summary and give the response for each.
glm.fit <- glm(Observed.Attendance ~ Expected.Attendance + Position.to.be.closed, family=binomial, data=train)
summary(glm.fit)
## How to interpret logistic regression summary?
## It is important to look at the z value (or z-score) which is the
## Estimate divided by its standard error. A nonsignificant z score
## suggests a coefficient can be dropped from the model.
## Attention! Si deux variables sont corrélées, elle peuvent mutuellement
## influencer leur z-score au point que ce derniers soit faibles alors que les variables
## ont une influence sur le modèle. Il faut alors de procéder itérativement:
## Retirer la variable avec le z-score le plus faible, et recommencer

probs <- predict(glm.fit, type='response')
## We define the threshold for the decision to be made based on the probabilities
y <- ifelse(probs<=0.5, 'no', 'yes')

y <- factor(y)
y_train <- factor(y_train)

conf_mat(data=y_hat, y_gt) # Confusion matrix to give a high view of the model results. 
