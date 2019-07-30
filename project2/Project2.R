
user = 'Bradley'

if (user == "Bradley") {
  setwd('/Users/Bradley/Documents/GitHub/stats6021_project1/project2/')
}

if (user == "Charlie") {
  setwd("C:\\Users\\cmp2c\\Desktop\\Summer2019\\STAT6021\\Project2")
}


#install.packages("remotes")
#remotes::install_github("nrguimaraes/sentimentSetsR")

library(ggplot2) #for plotting
library(olsrr) #for stepwise regression
library(boot) #for crossvalidation
library(caret) #cross validation stuff
#install.packages('e1071', dependencies=TRUE)
library(sentimentSetsR)

#Custom Query----
#select * 
#  from Posts
#where PostTypeID = 1
#and CreationDate >= '2017-01-01'

#df <- read.csv("nondeletedposts.csv", stringsAsFactors = F)

df <- read.csv("QueryResults.csv", stringsAsFactors = F)

#only posts that were questions
#df <- df[df$PostTypeId==1,]
#df <- df[seq(1,nrow(df), 10),] # just temporary to make things faster while I'm testing things out

#Data Cleaning / Feature Engineering----
df$y <- as.factor(ifelse(!is.na(df$AcceptedAnswerId),1,0))
# remove html tags from the post
df$Body <- gsub("<[^>]+>", "", df$Body)
# get question word count, put the number in new column called QuestionWordCount
df <- transform(df, QuestionWordCount=sapply(strsplit(df$Body, " "), length))
# get title word count, put the count in new column called TitleWordCount
df <- transform(df, TitleWordCount=sapply(strsplit(df$Title, " "), length))

#Median is at 8
cumsum(table(df$TitleWordCount))
median(df$TitleWordCount)

df$TitleWordCountBucket <- as.factor(ifelse(df$TitleWordCount > 8,1,0))

## Remove "<", ">", and "-" characters from tags 
df$Tags <- gsub("<", " ", df$Tags)
df$Tags <- gsub(">", " ", df$Tags)

## get number of tags from "Tags" column. Put tag count in new column called "TagCount"
df <- transform(df, TagCount=sapply(strsplit(df$Tags, " "), length))

df$TagCount <- df$TagCount / 2

df

#Analyze tag values
s <- strsplit(df$Tags, split = " ")

#Creats df of one tag per line, so multiple lines per post
tags_df <- data.frame(Id = rep(df$Id, sapply(s, length)), Tag = unlist(s))

#Aggregates on tag to see how frequent certain tags are
tag_cnt_df <- data.frame(table(tags_df$Tag))

tag_cnt_df <- tag_cnt_df[order(-tag_cnt_df$Freq),]

tag_cnt_df

#Get list of sexy tag values that could drive answers
sexy_ml_tags = c('machine-learning','neural-network', 'clustering','predictive-models','deep-learning','classification','boosting','svm','feature-selection','random-forest','python','conv-neural-network','scikit-learn','k-means','unbalanced-classes')

non_sexy_stats_tags = c('r','regression','time-series','hypothesis-testing','bayesian','correlation','statistical-significance','mathematical-statistics','normal-distribution','anova','multiple-regression','mixed-model','confidence-interval','t-test','generalized-linear-model')

sexy_ml_pattern = paste(sexy_ml_tags,collapse = "|")

non_sexy_stat_pattern = paste(non_sexy_stats_tags,collapse = "|")

#1 if post has any of the above sexy ml tags, 0 if none
df$sexy_ml_tag <- as.factor(ifelse(grepl(sexy_ml_pattern,df$Tags),1,0))

df$non_sexy_stat_tags <- as.factor(ifelse(grepl(non_sexy_stat_pattern,df$Tags),1,0))

#title has question mark? does asking a question increase your likelihood?
df$titleQMark <- as.factor(ifelse(grepl("\\?",df$Title),1,0))

df$lowerTitle = tolower(df$Title)

#Categorize questions by type
df$QType <- as.factor(ifelse(startsWith(df$lowerTitle, 'what'),'what',
                ifelse(startsWith(df$lowerTitle, 'why'),'why',
                  ifelse(startsWith(df$lowerTitle, 'where'), 'where',
                    ifelse(startsWith(df$lowerTitle, 'how'), 'how',
                      ifelse(startsWith(df$lowerTitle, 'who'), 'who',
                        ifelse(startsWith(df$lowerTitle, 'is'), 'is',
                          ifelse(startsWith(df$lowerTitle, 'when'), 'when', 'other'
                        )
                      )
                    )
                  )
                )
              )
            )
          )

df <- within(df, QType <- relevel(QType, ref = 'other'))

## try to clean up timestamp data (not sure if this is necessary tbh)
df$Hours <- format(as.POSIXct(df$CreationDate, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H")


#Exploratory Data Analysis----

## plot AnswerCount vs. Hours
plot(df$Hours, df$AnswerCount)

# get character count for question body
df$numCharInQuestion <- sapply(df$Body, nchar)
# taylor's graph
plot(df$numCharInQuestion, df$y)

# plot tagcount vs. answer count -- if you include more tags, will you get more answers? based on inspection, looks like this may not be the case :(
plot(df$TagCount, df$AnswerCount)

# plot numCharInQuestion vs. answer count -- are shorter questions more likely to get answered? looks like that may be the case based on the graph!
#log relationship?
plot(df$numCharInQuestion, df$AnswerCount)

# plot QuestionWordCount vs. answer count -- are shorter answers (by word count) more likely to get answered?
#log relationship?
plot(df$QuestionWordCount, df$AnswerCount)

# What is the difference between answers & comments? Can we gauge the activty of a post based on the word count? Is this even the right way of doing this (i.e. is there double counting?)
plot(df$QuestionWordCount, df$AnswerCount + df$CommentCount)

# Question + Title Word Count vs. Answer + comment Count
plot(df$QuestionWordCount + df$TitleWordCount, df$AnswerCount + df$CommentCount)

# Are questions that are viewed more more likely to be answered?
plot(df$ViewCount, df$AnswerCount)

#Seems to be that adding machine learning tags helps views
boxplot(log(df$ViewCount) ~ df$sexy_ml_tag)

boxplot(log(df$ViewCount) ~ df$non_sexy_stat_tags)

#Making sure your question contains a question mark also seems to help
boxplot(log(df$ViewCount) ~ df$titleQMark)

boxplot(log(df$ViewCount) ~ df$QType, notch = TRUE)

boxplot(log(df$ViewCount) ~ df$TagCount)

df$TagCount

#Modeling----

smp_size <- floor(0.9 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

traindf <- df[train_ind, ]
testdf <- df[-train_ind, ]

nrow(traindf)
nrow(testdf)

#ols_step_both_p(lm(AnswerCount ~ QuestionWordCount + TitleWordCount + TagCount, data = df), penter = .3, details = T)
## Mapping out whether an answer is selected (y) vs. Answercount, questionwordcount, titlewordcount, and tagcount
ols_step_both_p(lm(y ~ QuestionWordCount + TitleWordCount + TagCount, data = df), penter = .05, details = T)
## drop everything but TitleWordCount & TagCount
mod <- glm(y ~ TitleWordCount + TagCount,family = binomial(link = "logit"), data = traindf)
#mod <- glm(y ~ TitleWordCount + TagCount, family = binomial(link = "logit"), data = df)
plot(df$TitleWordCount + df$TagCount, df$y)

#Add sexy ml tag
mod2 <- glm(y ~ TitleWordCount + TagCount + sexy_ml_tag, family = binomial(link = "logit"), data = traindf)

summary(mod2)
anova(mod, mod2, test = 'Chisq')

nullmod <- glm(y ~ 1, family = binomial(link = 'logit'), data = traindf)

anova(nullmod, mod2)

summary(mod2)

#Add question mark
mod3 <- glm(y ~ TitleWordCount + TagCount + titleQMark, family = binomial(link = "logit"), data = traindf)

summary(mod3)
#remove the title word count

#Remove title word count
mod4 <- glm(y ~ TagCount + titleQMark, family = binomial(link = "logit"), data = traindf)
summary(mod4)

#Add sexy ml tag again
mod5 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag, family = binomial(link = "logit"), data = traindf)

summary(mod5)

anova(mod4, mod5, test = "Chisq")

dev_mod5 <- anova(mod4, mod5)$Deviance[2]
df_mod5 <- anova(mod4, mod5)$Df[2]

#against the previous model
1-pchisq(dev_mod5, df_mod5)

sat_dev_mod5 <- anova(mod5)$`Resid. Dev`[2]
sat_df_mod5 <- anova(mod5)$`Resid. Df`[2]

#against the saturdated model
1 - pchisq(sat_dev_mod5, sat_df_mod5)


mod6 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag + QType, family = binomial(link = "logit"), data = traindf)

summary(mod6)

anova(mod5, mod6, test = 'Chisq')

summary(mod6)


#Evaluate Accuracy of mod 5----
pred <- predict(mod6, newdata = testdf, type ='response')

percent_answered <- sum(as.integer(testdf$y)-1)/nrow(testdf)

decision_boundary <- quantile(pred, c(1-percent_answered))

ypred <- as.factor(ifelse(pred >= decision_boundary, 1, 0))

confusionMatrix(data =ypred, testdf$y)


#Cross Validation----
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(y ~ TagCount + titleQMark + sexy_ml_tag,  data=df, method="glm", family="binomial", trControl = ctrl, tuneLength = 5, metric = 'Accuracy')


pred = predict(mod_fit, newdata=testdf)
confusionMatrix(data=pred, testdf$y)
#Want this to be above 0.05, not below, so this is good
#Here we are comparing this to the saturated model
1 - pchisq(dev_mod3, df_mod3)


# Get sentiment of title
df$titleSentiment <- getVaderRuleBasedSentiment(df$Title, compound=TRUE)
