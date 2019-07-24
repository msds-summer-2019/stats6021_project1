library(ggplot2)

df <- read.csv("nondeletedposts.csv", stringsAsFactors = F)
#df <- df[seq(1,nrow(df), 10),] # just temporary to make things faster while I'm testing things out
df$y <- !is.na(df$AcceptedAnswerId)
# remove html tags from the post
df$Body <- gsub("<[^>]+>", "", df$Body)
# get question word count, put the number in new column called QuestionWordCount
df <- transform(df, QuestionWordCount=sapply(strsplit(df$Body, " "), length))
# get title word count, put the count in new column called TitleWordCount
df <- transform(df, TitleWordCount=sapply(strsplit(df$Title, " "), length))
## Remove "<", ">", and "-" characters from tags 
df$Tags <- gsub("<", "", df$Tags)
df$Tags <- gsub(">", "", df$Tags)
df$Tags <- gsub("-", " ", df$Tags)
## get number of tags from "Tags" column. Put tag count in new column called "TagCount"
df <- transform(df, TagCount=sapply(strsplit(df$Tags, " "), length))


## try to clean up timestamp data (not sure if this is necessary tbh)
df$Hours <- format(as.POSIXct(df$CreationDate, "%Y-%m-%d %H:%M:%S", tz = ""), format = "%H")
## plot AnswerCount vs. Hours
plot(df$Hours, df$AnswerCount)

# get character count for question body
df$numCharInQuestion <- sapply(df$Body, nchar)
# taylor's graph
plot(df$numCharInQuestion, df$y)

# plot tagcount vs. answer count -- if you include more tags, will you get more answers? based on inspection, looks like this may not be the case :(
plot(df$TagCount, df$AnswerCount)

# plot numCharInQuestion vs. answer count -- are shorter questions more likely to get answered? looks like that may be the case based on the graph!
plot(df$numCharInQuestion, df$AnswerCount)

# plot QuestionWordCount vs. answer count -- are shorter answers (by word count) more likely to get answered?
plot(df$QuestionWordCount, df$AnswerCount)

# What is the difference between answers & comments? Can we gauge the activty of a post based on the word count? Is this even the right way of doing this (i.e. is there double counting?)
plot(df$QuestionWordCount, df$AnswerCount + df$CommentCount)

# Question + Title Word Count vs. Answer + comment Count
plot(df$QuestionWordCount + df$TitleWordCount, df$AnswerCount + df$CommentCount)

# Are questions that are viewed more more likely to be answered?
plot(df$ViewCount, df$AnswerCount)

#ols_step_both_p(lm(AnswerCount ~ QuestionWordCount + TitleWordCount + TagCount, data = df), penter = .3, details = T)
## Mapping out whether an answer is selected (y) vs. Answercount, questionwordcount, titlewordcount, and tagcount
ols_step_both_p(lm(y ~ AnswerCount + QuestionWordCount + TitleWordCount + TagCount, data = df), penter = .3, details = T)
## drop everything but TitleWordCount & TagCount
lm(y ~ TitleWordCount + TagCount, data = df)
mod <- glm(y ~ TitleWordCount + TagCount, family = binomial(link = "logit"), data = df)
plot(df$TitleWordCount + df$TagCount, df$y)
lines(df$TitleWordCount, predict(mod, type = "response"), type = "l", col = "red")
# ^ what the heck is this