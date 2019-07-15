#Project 1

#import statements
library("reshape2")
library("plyr")
library("ggplot2")
library("dplyr")



#install.packages('caret')
#library('caret')
library(data.table)
#install.packages('mltools')
library(mltools)

library(MASS)
#install.packages('car')
library(car)

#fix issue b/w dplyer and MASS with select
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)

install.packages('lmtest')
library(lmtest)
#install.packages('ROlogit')
#library(ROlogit)

install.packages('DAAG')
library(DAAG)

library(RColorBrewer)

#EDA ----
#Read in data
df <- read.csv('clean_diamond_data.csv')
ddf <- tbl_df(df)

df$logprice <- log10(df$price)

summary(df)
col_names <- colnames(df)

#look at distribution of values by column


#price#
hist(df$price, breaks = 1000)

#Look at log price's distribution
hist(log(df$price, base = 10), breaks = 1000)
#>looks to be bi-modal

#cdf seems to back this up
plot(ecdf(log(df$price, base = 10)))

#check after removing diamonds >$1M
plot(ecdf(price_df$price), col=aCDFcolor, main=NA)


#carat#

hist(df$carat, breaks = 200)
#looks like a bi-modal distribution

hist(log(df$carat, base = 10), breaks = 500)
#seems to be a bias towards certain values (rounding?)


#clarity#

##barplot(prop.table(table(df$clarity)))

df$clarity_rank <- 99
#encode clarity to clarity_rank to imply order
df$clarity_rank[df$clarity == 'IF'] <- 1
df$clarity_rank[df$clarity == 'FL'] <- 2
df$clarity_rank[df$clarity == 'VVS1'] <- 3
df$clarity_rank[df$clarity == 'VVS2'] <- 4
df$clarity_rank[df$clarity == 'VS1'] <- 5
df$clarity_rank[df$clarity == 'VS2'] <- 6
df$clarity_rank[df$clarity == 'SI1'] <- 7
df$clarity_rank[df$clarity == 'SI2'] <- 8

df$clarity_rank2 <- 99
df$clarity_rank2[df$clarity == 'IF'] <- 1
df$clarity_rank2[df$clarity == 'FL'] <- 2
df$clarity_rank2[df$clarity == 'VVS1' | df$clarity == 'VVS2' ] <- 3
df$clarity_rank2[df$clarity == 'VS1' | df$clarity == 'VS2' ] <- 4
df$clarity_rank2[df$clarity == 'SI1' | df$clarity == 'SI2' ] <- 5

#Plot the clarity ranks
hist(df$clarity_rank, breaks = length(unique(df$clarity_rank)))
#>seems like most diamonds are of lower quality (VVS2 and below)

clarity_gb <- group_by(ddf, clarity)
clarity_df <- summarize(clarity_gb, cnt = n(), median_price = median(price))

#general trend upward as color increases
plot(x = clarity_df$clarity, y = clarity_df$median_price)
#tends to have the most correlation with price


#color#


unique(df$color)

df$color_rank <- 99
df$color_rank[df$color == 'D'] <- 1
df$color_rank[df$color == 'E'] <- 2
df$color_rank[df$color == 'F'] <- 3
df$color_rank[df$color == 'G'] <- 4
df$color_rank[df$color == 'H'] <- 5
df$color_rank[df$color == 'I'] <- 6
df$color_rank[df$color == 'J'] <- 7

#Use the diamond grading of 
df$color_rank2 <- 99
#colorless
df$color_rank2[df$color == 'D' | df$color == 'E' | df$color == 'F'] <- 1
#nearly colorless
df$color_rank2[df$color == 'G' | df$color == 'H' | df$color == 'I' | df$color =='J'] <- 2

hist(df$color_rank, breaks = length(unique(df$color_rank)) + 2)
#>slight skew towards better color

color_gb <- group_by(ddf, color)
color_df <- summarize(color_gb, cnt = n(), median_price = median(price))

#general trend upward as color increases
plot(x = color_df$color, y = color_df$median_price)

#cut#


unique(df$cut)

df$cut_rank <- 99
df$cut_rank[df$cut == 'Astor Ideal'] <- 1
df$cut_rank[df$cut == 'Ideal'] <- 2
df$cut_rank[df$cut == 'Very Good'] <- 3
df$cut_rank[df$cut == 'Good'] <- 4

length(unique(df$cut_rank))

hist(df$cut_rank, breaks = 5)
#>skew towards ideal cuts. astor ideals are very rare

cut_gb <- group_by(ddf, cut)
cut_df <- summarize(cut_gb, cut_cnt = n(), median_price = median(price))

cut_df$cut
plot(x = cut_df$cut, y = cut_df$median_price)
#counter inuititve, perhaps the large sample size of ideal is biasing the dataset

#Box Plots of variables vs price#



#clarity
boxplot(df$price ~ df$clarity, 
        xlab = 'Clarity', ylab = 'Price')
#hard to see, try log(price)
boxplot(log10(df$price) ~ df$clarity, 
        xlab = 'Clarity', ylab = 'Price')

#cut
boxplot(df$price ~ df$cut, 
        xlab = 'Cut', ylab = 'Price')
#hard to see, try log(price)
boxplot(log10(df$price) ~ df$cut, 
        xlab = 'Cut', ylab = 'Price')

#color
boxplot(df$price ~ df$color, 
        xlab = 'Color', ylab = 'Price')
#hard to see, try log(price)
boxplot(log10(df$price) ~ df$color, 
        xlab = 'Color', ylab = 'Price')


#look at overall scatterplots

#plot relationship b/w price & carat with cut/clarity/color
#cut
cut_palette <- rev(brewer.pal(length(unique(df$cut)),'YlGnBu'))
plot(y = df$logprice, x = df$logcarat, col=cut_palette[df$cut_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Cut by Price & Carat')
cut_legend = c('Astor Ideal','Ideal','Very Good','Good')
legend("bottomright",legend = cut_legend,col=cut_palette,pch=1)
#clarity
clar_palette <- rev(brewer.pal(length(unique(df$clarity)),'YlGnBu'))
plot(y = df$logprice, x = df$logcarat, col=clar_palette[df$clarity_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Clarity by Price & Carat')
cut_legend = c('FL','IF','VVS1','VVS2','VS1','VS2','SI1','SI2')
legend("bottomright",legend = cut_legend,col=clar_palette,pch=1)

#color
col_palette <- rev(brewer.pal(length(unique(df$color)),'YlGnBu'))
plot(y = df$logprice, x = df$logcarat, col=col_palette[df$color_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Color by Price & Carat')
color_legend = c('D','E','F','G','H','I','J')
legend("bottomright",legend = color_legend,col=col_palette,pch=1)

#display.brewer.all()
# Question 1 ----

#What linear model will provide the most accurate prediction for price?

#base model, price on all regressors
par(mfrow=c(2,2))

colnames(df)

#Base model with carat and rank values for categoricals
base_mod <- lm(formula = logprice ~ carat + clarity_rank + color_rank + cut_rank, data = df)
anova(base_mod)

#one-hot encode variables
df_encoded <- one_hot(as.data.table(select(df, logprice, cut, clarity, carat, color)))

#run model with 
base_mod_encoded <- lm(formula = logprice ~ . , data = df_encoded)
anova(base_mod_encoded)

#plot residuals

#intern_s_resids_base <- stdres(base_mod)
extern_s_resids_base <- studres(base_mod)
hist(extern_s_resids_base)

qqnorm(extern_s_resids_base)
qqline(extern_s_resids_base)
#heavy skew to the left of dist


plot(fitted.values((base_mod), extern_s_resids_base))
#residuals could be predicted in an exponential fashion (assymptotic?)

avPlots(base_mod)
#avPlots(base_mod_encoded)

df$logcarat <- log10(df$carat)

#now transform 
base_mod2 <- lm(logprice ~ logcarat + clarity_rank + color_rank + cut_rank, data = df)
summary(base_mod2)
summary(base_mod)


#plot residuals
extern_s_resids_base2 <- studres(base_mod2)
hist(extern_s_resids_base2)

qqnorm(extern_s_resids_base2)
qqline(extern_s_resids_base2)

#fatter tails, but better than before
avPlots(base_mod2)

cor(df$price, df$carat)

cor(df$logprice, log(df$carat))

#try with interaction terms
base_mod3 <-lm(logprice ~ logcarat + color_rank + clarity_rank + cut_rank + logcarat*color_rank, data = df  )
summary(base_mod3)
summary(base_mod2)

anova(base_mod2, base_mod3)

#plot residuals, check for normality
extern_s_resids_base3 <- studres(base_mod3)
hist(extern_s_resids_base3)

qqnorm(extern_s_resids_base3)
qqline(extern_s_resids_base3)

#check for homoskedacitity
plot(fitted.values(base_mod3), extern_s_resids_base3)

plot(df$cut_rank, extern_s_resids_base3)


summary(base_mod)$adj.r.squared
summary(base_mod2)$adj.r.squared
summary(base_mod3)$adj.r.squared

#throw a bunch of spaghetti at the wall, see what sticks
base_mod_intall <- lm(logprice ~ (logcarat + color_rank + clarity_rank + cut_rank)^2, data = df)

anova(base_mod_intall)

#logcarat:color_rank and color_rank:clarity_rank seem promising
base_mod4 <- lm(logprice ~ logcarat + color_rank + clarity_rank + cut_rank + logcarat*color_rank + color_rank*clarity_rank, data = df)
anova(base_mod4)
summary(base_mod4)$adj.r.squared

plot(base_mod4)

#look at resides of base_mod4
extern_s_resids_base4 <- studres(base_mod4)
hist(extern_s_resids_base4)

plot(fitted.values(base_mod4), extern_s_resids_base4)

qqnorm(extern_s_resids_base4)
qqline(extern_s_resids_base4)

avPlot(base_mod4, variable = 'color_rank:clarity_rank')

#Test if adding logcarat*color is significantly better
anova(base_mod2, base_mod3)
#>seems to be so

#Test if adding color*clarity is significantly better
anova(base_mod3, base_mod4)


summary(base_mod4)


plot(x = df$cut_rank, y = extern_s_resids_base4)
#>seems to be heterscedacity for resids as cut_rank increases. let's try transforming
#

#let's see how else we could transform some of these predictors
#https://stats.stackexchange.com/questions/61217/transforming-variables-for-multiple-regression-in-r
#boxTidwell(logprice ~ clarity_rank + color_rank + carat + clarity_rank*color_rank + color_rank * carat, data = df, max.iter = 100)

base_mod5 <- lm(logprice ~ I(carat^(1/100000)) + cut_rank + clarity_rank**2 + color_rank**2 + color_rank*logcarat + color_rank*clarity_rank, data = df)
#anova(base_mod5)
summary(base_mod5)$adj.r.squared
summary(base_mod4)$adj.r.squared
#
par(mfrow = c(1,1))
plot(df$logcarat, df$logprice)
plot(df$carat^(1/100000), df$logprice)

base_mod6 <- lm(logprice ~ I(carat^(1/100000)) + cut + clarity + color + color*clarity + color*logcarat, data = df)

summary(base_mod6)

?I

plot(base_mod5)
#lets test a few of these models to see if there is overfitting

## 75% of the sample size
smp_size <- floor(0.8 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]


train_mod4 <- lm(base_mod4$call[[2]], data = train)
mod4_mspe <- mean((test$logprice - predict.lm(train_mod4, test)) ^ 2)

train_mod5 <- lm(base_mod5$call[[2]], data = train)
mod5_mspe <- mean((test$logprice - predict.lm(train_mod5, test)) ^ 2)

train_mod6 <- lm(base_mod6$call[[2]], data = train)
mod6_mspe <- mean((test$logprice - predict.lm(train_mod6, test)) ^ 2)

mod4_mspe
mod5_mspe
mod6_mspe

cv.lm(data = df, form.lm = base_mod4$call[[2]], m = 5)
#>0.00519
cv.lm(data = df, form.lm = base_mod5$call[[2]], m = 5)
#>0.00509
cv.lm(data = df, form.lm = base_mod6$call[[2]], m = 5)
#>0.00454