
user = 'NA'

if (user == "Bradley") {
  setwd('/Users/Bradley/Documents/GitHub/stats6021_project1/project2/')
}

if (user == "Charlie") {
  setwd("C:\\Users\\cmp2c\\Desktop\\Summer2019\\STAT6021\\Project2")
}

#Import packages----
#install.packages("remotes")
#remotes::install_github("nrguimaraes/sentimentSetsR")

library(ggplot2) #for plotting
library(olsrr) #for stepwise regression
library(boot) #for crossvalidation
library(caret) #cross validation stuff
library(dplyr)
#install.packages('e1071', dependencies=TRUE)

#install.packages('gmodels')
library(gmodels) #to build crosstabs

#install.packages('pROC')
library(pROC) #to generate ROC plot

#install.packages('ROCR')
library(ROCR) #generate ROC plot

library(xtable)#LaTeX
#install.packages('latexpdf')
library(latexpdf)

#Custom Query from Stack Exchanges Query Engine----
#select * 
#  from Posts
#where PostTypeID = 1
#and CreationDate >= '2017-01-01'

df <- read.csv("QueryResults.csv", stringsAsFactors = F)


#Data Cleaning / Feature Engineering----
df$y <- as.factor(ifelse(!is.na(df$AcceptedAnswerId),1,0))

#flag to see if code snippet is in body
df$code_flag <- as.factor(ifelse(grepl('\\<code\\>',df$Body),1,0))

#flag to see if formula is in body
df$formula_flag <- as.factor(ifelse(grepl('\\$\\$', df$Body),1,0))

#remove html tags from the post
df$Body <- gsub("<[^>]+>", "", df$Body)
# get question word count, put the number in new column called QuestionWordCount
df <- transform(df, QuestionWordCount=sapply(strsplit(df$Body, " "), length))
# get title word count, put the count in new column called TitleWordCount
df <- transform(df, TitleWordCount=sapply(strsplit(df$Title, " "), length))

#Median is at 8
median(df$TitleWordCount)

#Bucket number of words into two groups (more than median, less than median)
df$TitleWordCountBucket <- as.factor(ifelse(df$TitleWordCount > 8,1,0))

## Remove "<", ">", and "-" characters from tags 
df$Tags <- gsub("<", " ", df$Tags)
df$Tags <- gsub(">", " ", df$Tags)

## get number of tags from "Tags" column. Put tag count in new column called "TagCount"
df <- transform(df, TagCount=sapply(strsplit(df$Tags, " "), length))

#Tag count somehow get doubled, halve it
df$TagCount <- df$TagCount / 2

#Analyze tag values
s <- strsplit(df$Tags, split = " ")

#Creats df of one tag per line, so multiple lines per post
tags_df <- data.frame(Id = rep(df$Id, sapply(s, length)), Tag = unlist(s))

#Aggregates on tag to see how frequent certain tags are
tag_cnt_df <- data.frame(table(tags_df$Tag))

#Order by most frequently occurring tags
tag_cnt_df <- tag_cnt_df[order(-tag_cnt_df$Freq),]

#Get number of unique tags
nrow(tag_cnt_df)

#Get list of sexy tag values that could drive answers
sexy_ml_tags = c('machine-learning','neural-network', 'clustering','predictive-models','deep-learning','classification','boosting','svm','feature-selection','random-forest','python','conv-neural-network','scikit-learn','k-means','unbalanced-classes')

#GEt list of non sexy stats tags
non_sexy_stats_tags = c('r','regression','time-series','hypothesis-testing','bayesian','correlation','statistical-significance','mathematical-statistics','normal-distribution','anova','multiple-regression','mixed-model','confidence-interval','t-test','generalized-linear-model')

#grep pattern building
sexy_ml_pattern = paste(sexy_ml_tags,collapse = "|")
#grep pattern building

non_sexy_stat_pattern = paste(non_sexy_stats_tags,collapse = "|")

#1 if post has any of the above sexy ml tags, 0 if none
df$sexy_ml_tag <- as.factor(ifelse(grepl(sexy_ml_pattern,df$Tags),1,0))

df$non_sexy_stat_tags <- as.factor(ifelse(grepl(non_sexy_stat_pattern,df$Tags),1,0))

#title has question mark? does asking a question increase your likelihood?
df$titleQMark <- as.factor(ifelse(grepl("\\?",df$Title),1,0))

#convert title to lower case for next part
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

#Set default question type to "other"
df <- within(df, QType <- relevel(QType, ref = 'other'))

## try to clean up timestamp data (not sure if this is necessary tbh)
df$Hours <- format(as.POSIXct(df$CreationDate, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H")

#convert hours to double dtype
df$HoursContinuous <- as.double(df$Hours)

#group into morning, afternoon, and evening by 8 hour chunks
df$TimeOfDayGrouped <- ifelse(df$HoursContinuous <= 8, 'Morning',
                        ifelse(df$HoursContinuous <= 16, 'Afternoon','Evening'))

#Get rid of extra rows where Hours Continuous is na
df <- df[is.na(df$HoursContinuous)==FALSE,]

# get character count for question body
df$numCharInQuestion <- sapply(df$Body, nchar)

#Add VADER Scores----
cleaned_body_df <- select(df, Id, Body)

#Write to csv so we can run VADER in python
write.csv(cleaned_body_df, 'cleaned_body.csv')

#read in VADER values
sent_scores_df <- read.csv('cleaned_body_sentiment_scores.csv')

#Add sentiment scores to dataframe
df <- merge(df, sent_scores_df, by.x = 'Id', by.y = 'Id', sort = TRUE)

#Plots/EDA----
#Making sure your question contains a question mark also seems to help
png('qmark.png')
boxplot(log(df$ViewCount) ~ df$titleQMark, xlab = '? Flag', ylab = 'log(Views)', main = 'Effect of Question Mark on log(Views)')
dev.off()

#boxplot of question type versus views
png('qtype.png')
boxplot(log(df$ViewCount) ~ df$QType, xlab = 'Question Type', ylab = 'log(Views)', main = 'Effect of Question Type on log(Views)')
dev.off()

#title length vs. views
png('titlelength.png')
plot(x = df$TitleWordCount, y = log(df$ViewCount), xlab = 'Words in Title', ylab = 'log(Views)', main = 'Effect of Words in Title on log(Views)')
dev.off()

#sexy ml vs views
png('sexymltags.png')
boxplot(log(df$ViewCount) ~ df$sexy_ml_tag, xlab = 'ML Tags Used', ylab = 'log(Views)', main = 'Effect of ML Tags on Views')
dev.off()

#not-sexy stats vs views
png('notsexystatstags.png')
boxplot(log(df$ViewCount) ~ df$non_sexy_stat_tags, xlab = 'Statistics Tags Used', ylab = 'log(Views)', main = 'Effect of Statistics Tags on Views')
dev.off()

#tag count vs. views
png('tagcount.png')
boxplot(log(df$ViewCount) ~ df$TagCount, xlab = 'Tags in Post', ylab = 'log(Views)', main = 'Effect of Number of Tags on log(Views)')
dev.off()

#hour of day vs views
png('hourofday.png')
boxplot(log(df$ViewCount) ~ df$HoursContinuous, xlab = 'Hour of Day (UTC)', ylab = 'log(Views)', main = 'Effect of Hour of Day on log(Views)')
dev.off()

#time of day vs views
png('timeofday.png')
boxplot(log(df$ViewCount) ~ df$TimeOfDayGrouped, xlab = 'Time of Day (UTC)', ylab = 'log(Views)', main = 'Effect of Time of Day (UTC) on log(Views)')
dev.off()

#sentiment vs views
png('sentiment_views.png')
plot(x = df$compound, y= log(df$ViewCount), xlab = 'Sentiment', ylab = 'log(Views)', main = 'Effect of Sentiment on log(Views)')
dev.off()

#distirubiton of sentiment
png('sentiment.png')
hist(x = df$compound, xlab = 'Sentiment', ylab = 'Frequency', main = 'Distribution of Sentiment Scores on Body')
dev.off()

#code flag vs views
png('codeflag.png')
boxplot(log(df$ViewCount) ~ df$code_flag, xlab = 'Code Flag', ylab = 'log(Views)', main = 'Effect of Code Presence on Views')
dev.off()

#formula flag vs views
png('formulaflag.png')
boxplot(log(df$ViewCount) ~ df$formula_flag, xlab = 'Formula Flag', ylab = 'log(Views)', main = 'Effect of Formula Presence on Views')
dev.off()


#Views and answers
png('viewsanswers.png')
plot(log(df$ViewCount), df$AnswerCount, xlab = 'log(Views)', ylab = '# of Answers', main = 'Relationship between log(Views) on Answers')
dev.off()

#Views and accepted answers
png('viewsacceptedanswers.png')
boxplot(log(df$ViewCount) ~ df$y, xlab = 'Accepted Answer', ylab = 'log(Views)', main = 'Relatinonship between log(Views) and Accepted Answers')
dev.off()

#Train/test split----

#set sample size to 90% of data
smp_size <- floor(0.9 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

#split df into train and test
traindf <- df[train_ind, ]
testdf <- df[-train_ind, ]

#validate split
nrow(traindf)
nrow(testdf)

#Modeling----

## drop everything but TitleWordCount & TagCount
mod <- glm(y ~ TitleWordCount + TagCount,family = binomial(link = "logit"), data = traindf)

#Add sexy ml tag
mod2 <- glm(y ~ TitleWordCount + TagCount + sexy_ml_tag, family = binomial(link = "logit"), data = traindf)

summary(mod2)
anova(mod, mod2, test = 'Chisq')

nullmod <- glm(y ~ 1, family = binomial(link = 'logit'), data = traindf)

anova(nullmod, mod2, test ='Chisq')

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

#against the previous
1-pchisq(dev_mod5, df_mod5)

sat_dev_mod5 <- anova(mod5)$`Resid. Dev`[2]
sat_df_mod5 <- anova(mod5)$`Resid. Df`[2]

#against the saturated model
1 - pchisq(sat_dev_mod5, sat_df_mod5)

#Add question type
mod6 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag + QType, family = binomial(link = "logit"), data = traindf)

summary(mod6)

anova(mod5, mod6, test = 'Chisq')

#Add hour of day
mod7 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag + QType + HoursContinuous, family = binomial(link = "logit"), data = traindf)

summary(mod7)

anova(mod6, mod7, test = 'Chisq')

#Add sentiment of body\
mod8 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag + QType + HoursContinuous + compound, family = binomial(link = "logit"), data = traindf)

summary(mod8)

anova(mod7, mod8, test = 'Chisq')

#against the saturated model
sat_dev_mod8 <- anova(mod8)$`Resid. Dev`[2]
sat_df_mod8 <- anova(mod8)$`Resid. Df`[2]

1 - pchisq(sat_dev_mod8, sat_df_mod8)

#variable importance in Model 8

varImp(mod8)

#add code flag
mod9 <- glm(y ~ TagCount + titleQMark + sexy_ml_tag + QType + HoursContinuous + compound + code_flag, family = binomial(link = "logit"), data = traindf)

summary(mod9)

#add formula flag, remove sexy ml tag
mod10 <- glm(y ~ TagCount + titleQMark + QType + HoursContinuous + compound + code_flag + formula_flag, family = binomial(link = "logit"), data = traindf)

xtable(summary(mod10))

#Compare to null model
anova(nullmod, mod10, test = 'Chisq')

#against the saturated model
sat_dev_mod10 <- anova(mod10)$`Resid. Dev`[2]
sat_df_mod10 <- anova(mod10)$`Resid. Df`[2]

1-pchisq(sat_dev_mod10, sat_df_mod10)



#ROC curves----

#generate predictions for each model
pred1 <- predict(mod, newdata = testdf, type ='response')
pred2 <- predict(mod2, newdata = testdf, type ='response')
pred3 <- predict(mod3, newdata = testdf, type ='response')
pred4 <- predict(mod4, newdata = testdf, type ='response')
pred5 <- predict(mod5, newdata = testdf, type ='response')
pred6 <- predict(mod6, newdata = testdf, type ='response')
pred7 <- predict(mod7, newdata = testdf, type ='response')
pred8 <- predict(mod8, newdata = testdf, type ='response')
pred9 <- predict(mod9, newdata = testdf, type ='response')
pred10 <- predict(mod10, newdata = testdf, type ='response')

#Store ROC curve data for each model
p1 <- prediction(pred1, testdf$y)
perf1 <- performance(p1,"tpr",'fpr')

p2 <- prediction(pred2, testdf$y)
perf2 <- performance(p2,"tpr",'fpr')

p3 <- prediction(pred3, testdf$y)
perf3 <- performance(p3,"tpr",'fpr')

p4 <- prediction(pred4, testdf$y)
perf4 <- performance(p4,"tpr",'fpr')

p5 <- prediction(pred5, testdf$y)
perf5 <- performance(p5,"tpr",'fpr')

p6 <- prediction(pred6, testdf$y)
perf6 <- performance(p6,"tpr",'fpr')

p7 <- prediction(pred7, testdf$y)
perf7 <- performance(p7,"tpr",'fpr')

p8 <- prediction(pred8, testdf$y)
perf8 <- performance(p8,"tpr",'fpr')

p9 <- prediction(pred9, testdf$y)
perf9 <- performance(p9,"tpr",'fpr')

p10 <- prediction(pred10, testdf$y)
perf10 <- performance(p10,"tpr",'fpr')

#To plot the 45 degree line
xplot <- seq(0,1,0.1)
yplot <- seq(0,1,0.1)

plot(perf1,colorize=TRUE, main = "Model 1")
lines(xplot, yplot)

plot(perf2,colorize=TRUE, main = "Model 2")
lines(xplot, yplot)

plot(perf3,colorize=TRUE, main = "Model 3")
lines(xplot, yplot)

plot(perf4,colorize=TRUE, main = "Model 4")
lines(xplot, yplot)

plot(perf5,colorize=TRUE, main = "Model 5")
lines(xplot, yplot)

plot(perf6,colorize=TRUE, main = "Model 6")
lines(xplot, yplot)

plot(perf7,colorize=TRUE, main = "Model 7")
lines(xplot, yplot)

plot(perf8,colorize=TRUE, main = "Model 8")
lines(xplot, yplot)

plot(perf9,colorize=TRUE, main = "Model 9")
lines(xplot, yplot)

plot(perf10,colorize=TRUE, main = "Model 10")
lines(xplot, yplot)


#Function that determins the cutoff to be used in the prediction model, inferred from the ROC curve by maximizing sensitivity and specificity
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

#Get cutoff, sensitivity and specificify for best model
mod10_cutoff <- opt.cut(perf10, p10)[3]
mod10_sens <- opt.cut(perf10, p10)[1]
mod10_spec <- opt.cut(perf10, p10)[2]


#plotting sensitivity vs accuracy to demonstrate accuracy isnt always good
png('mod10accuracy.png')
perf10_acc <- performance(p10, "sens","acc")
plot(perf10_acc,colorize=TRUE, main = "Model 10 Accuracy")
segments(x0=mod10_spec, y0 = 0, x1 = mod10_spec, y1 = mod10_sens, col = 'blue')
segments(x0=0, y0 = mod10_sens, x1 = mod10_spec, y1 = mod10_sens, col = 'blue')
dev.off()

#plotting sensitivity vs accuracy to demonstrate accuracy isnt always good
png('mod10precision.png')
perf10_prec <- performance(p10, "sens","prec")
plot(perf10_prec,colorize=TRUE, main = "Model 10 Precision")
segments(x0=mod10_spec, y0 = 0, x1 = mod10_spec, y1 = mod10_sens, col = 'blue')
segments(x0=0, y0 = mod10_sens, x1 = mod10_spec, y1 = mod10_sens, col = 'blue')
dev.off()

#plotting sensitivity vs accuracy to demonstrate accuracy isnt always good
png('mod10specvsaccuracy.png')
perf10_spec_acc <- performance(p10, "spec","acc")
plot(perf10_spec_acc,colorize=TRUE, main = "Model 10 Precision")
dev.off()

#Sensitivity Cutoff----

#Alphas are accuracy
#ys are sensitivity
#xs are specifitity

#cutoff to get 80 sensitivity is 24.5%
target_sensitivity2 <- 0.95


sensitivity2 <- perf10_spec@y.values[[1]][perf10_spec@y.values[[1]] > target_specificity][1]

#Use sensitivity value to determine cutoff
sens_target_cutoff <- perf10_spec@alpha.values[[1]][perf10_spec@y.values[[1]] > target_specificity][1]
#24.5%)

specificity2 <- perf10_spec@x.values[[1]][perf10_spec@y.values[[1]] > target_specificity][1]



#Specificity cutoff----

target_specificity <- 0.95


specificity <- tail(perf10_spec@x.values[[1]][perf10_spec@x.values[[1]] > target_specificity], n = 1)

#Use sensitivity value to determine cutoff
spec_target_cutoff <- tail(perf10_spec@alpha.values[[1]][perf10_spec@x.values[[1]] > target_specificity], n = 1)
#24.5%)

sensitivity <- tail(perf10_spec@y.values[[1]][perf10_spec@x.values[[1]] > target_specificity], n = 1)

#ROC plot for best model
png('mod10roc.png')
perf10 <- performance(p10, "tpr","fpr")
plot(perf10,colorize=TRUE, main = "Model 10")
abline(0,1)
dev.off()

#Sensitivity vs specficity for best model
png('mod10sensspec.png')
perf10_spec <- performance(p10, "sens","spec")
plot(perf10_spec,colorize=TRUE, main = "Model 10 - Sensitivity vs. Specificity")
abline(1,-1)
#Plots optimal values for specificity and sensitivity when optimizing on cutoff
segments(x0=specificity, y0 = 0, x1 = specificity, y1 = sensitivity, col = 'blue')
segments(x0=0, y0 = sensitivity, x1 = specificity, y1 = sensitivity, col = 'blue')
dev.off()

#Sensitivity vs specficity for best model
png('mod10sensspec2.png')
perf10_spec <- performance(p10, "sens","spec")
plot(perf10_spec,colorize=TRUE, main = "Model 10 - Sensitivity vs. Specificity")
abline(1,-1)
#Plots optimal values for specificity and sensitivity when optimizing on cutoff
segments(x0=specificity2, y0 = 0, x1 = specificity2, y1 = sensitivity2, col = 'blue')
segments(x0=0, y0 = sensitivity2, x1 = specificity2, y1 = sensitivity2, col = 'blue')
dev.off()


#Summary of model in LaTeX
xtable(summary(mod10))

#AIC Calculations
mod$aic
mod2$aic
mod3$aic
mod4$aic
mod5$aic
mod6$aic
mod7$aic
mod8$aic
mod9$aic
mod10$aic

#Model Performance----

#Data used in table for report
mod$call[[2]]
mod$aic
mod1_auc <- performance(p1, measure = 'auc', fpr.stop=0.1)
mod1_auc@y.values

mod2$call[[2]]
mod2$aic
mod2_auc <- performance(p2, measure = 'auc', fpr.stop=0.1)
mod2_auc@y.values

mod3$call[[2]]
mod3$aic
mod3_auc <- performance(p3, measure = 'auc', fpr.stop=0.1)
mod3_auc@y.values

mod4$call[[2]]
mod4$aic
mod4_auc <- performance(p4, measure = 'auc', fpr.stop=0.1)
mod4_auc@y.values

mod5$call[[2]]
mod5$aic
mod5_auc <- performance(p5, measure = 'auc', fpr.stop=0.1)
mod5_auc@y.values


mod6$call[[2]]
mod6$aic
mod6_auc <- performance(p6, measure = 'auc', fpr.stop=0.1)
mod6_auc@y.values

mod7$call[[2]]
mod7$aic
mod7_auc <- performance(p7, measure = 'auc', fpr.stop=0.1)
mod7_auc@y.values

mod8$call[[2]]
mod8$aic
mod8_auc <- performance(p8, measure = 'auc', fpr.stop=0.1)
mod8_auc@y.values

mod9$call[[2]]
mod9$aic
mod9_auc <- performance(p9, measure = 'auc', fpr.stop=0.1)
mod9_auc@y.values

mod10$call[[2]]
mod10$aic
mod10_auc <- performance(p10, measure = 'auc', fpr.stop=0.1)
mod10_auc@y.values[[1]] + 0.5


xtable(anova(nullmod, mod10, test = 'Chisq'))

anova(mod10)
#Better than nothing
1 - pchisq(sat_dev_mod10, sat_df_mod10)


#Plot distribution of prediction values in best model
png('prediction_distribution.png')
hist(pred10, xlab = 'Probability Question was Answered Sufficiently', ylab = 'Frequency',main = 'Distribtion of Predictions from Test Set')
dev.off()

png('pred_dist_cutoff.png')
hist(pred10, xlab = 'Probability Question was Answered Sufficiently', ylab = 'Frequency',main = 'Distribtion of Predictions from Test Set')
abline(v = spec_target_cutoff, col = 'red')
dev.off()

?abline

#Model Prediction -----
#Store predictions again into pred
pred <- predict(mod10, newdata = testdf, type ='response')

#Use best cutoff value to encode probabilities to 1s and 0s
ypredspec <- as.factor(ifelse(pred >= spec_target_cutoff, 1, 0))
ypredsens <- as.factor(ifelse(pred >= sens_target_cutoff, 1, 0))
#Generate confusion matrix
confusionMatrix(data =ypredspec, testdf$y)
confusionMatrix(data =ypredsens, testdf$y)

max(pred)

#Confusion Matrix----

#Plot Specifity confusion matrix using GGPlot
Actualspec <- factor(c(0, 0, 1, 1))
Predictedspec <- factor(c(0, 1, 0, 1))
Yspec      <- c(3394, 178, 1283, 145)
resultsspec <- data.frame(Actual, Predicted, Y)

png('confusionmatrix_specifity.png')
ggplot(data =  resultsspec, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Yspec), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Yspec)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + theme(legend.position = "none") +
  ggtitle('Confusion Matrix for 95% Specificity') +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#Plot Sensitivity confusion matrix using GGPlot
Actualsens <- factor(c(0, 0, 1, 1))
Predictedsens <- factor(c(0, 1, 0, 1))
Ysens      <- c(238, 3334, 71, 1357)
resultssens <- data.frame(Actual, Predicted, Y)

png('confusionmatrix_sensitivity.png')
ggplot(data =  resultssens, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Ysens), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Ysens)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + theme(legend.position = "none") +
  ggtitle('Confusion Matrix for 95% Sensitivity') +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#use high cutoff value to illustrate how high accuracy isnt always good
ypred2 <- as.factor(ifelse(pred >= .9, 1, 0))
confusionMatrix(data =ypred2, testdf$y)

#plot confusion matrix of maximum accuracy using GGPlot
Actual2 <- factor(c(0, 0, 1, 1))
Predicted2 <- factor(c(0, 1, 0, 1))
Y2      <- c(3572 , 0, 1428, 0)
results2 <- data.frame(Actual2, Predicted2, Y2)

png('confusionmatrix_accuracy.png')
ggplot(data =  results2, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Y2), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Y2)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + theme(legend.position = "none")
dev.off()



#Test Predictions----
#Used in slides
pTagCount = c(0,0,0,0,0,5,5)
ptitleQMark = c(0,0,0,0,1,1,1)
pQType = c('other','why','why','why','why','why','why')
pHoursContinuous = c(0,0,0,0,0,0,0)
pcompound = c(0,0,0,0,0,0,-1)
pcode_flag = c(0,0,0,1,1,1,1)
pformula_flag = c(0,0,1,1,1,1,1)

#build data frame
test_prediction_data <- data.frame (TagCount = pTagCount, titleQMark = ptitleQMark, QType = pQType, HoursContinuous = pHoursContinuous, compound = pcompound, code_flag = pcode_flag, formula_flag = pformula_flag, stringsAsFactors = TRUE)

#transform predictors into factors
test_prediction_data$titleQMark <- as.factor(test_prediction_data$titleQMark)
test_prediction_data$code_flag <- as.factor(test_prediction_data$code_flag)
test_prediction_data$formula_flag <- as.factor(test_prediction_data$formula_flag)

#generate and print predictions
(test_predictions <- predict(mod10, newdata = test_prediction_data, type ='response') )

