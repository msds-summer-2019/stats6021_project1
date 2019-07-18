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


install.packages('DAAG')
library(DAAG)

library(RColorBrewer)

setwd('C:\\Users\\cmp2c\\Desktop\\Summer2019\\STAT6021\\Project1')
#EDA ----
#Read in data
df <- read.csv('clean_diamond_data.csv')


#stringsAsFactors = (TRUE/FALSE) (default is TRUE)
#lm will treat Factors differently

str(df)

ddf <- tbl_df(df)




# #cut
# cut_palette <- rev(brewer.pal(length(unique(df$cut)),'YlGnBu'))
# plot(y = df$logprice, x = df$logcarat, col=cut_palette[df$cut_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Cut by Price & Carat')
# cut_legend = c('Astor Ideal','Ideal','Very Good','Good')
# legend("bottomright",legend = cut_legend,col=cut_palette,pch=1)
# #clarity
# clar_palette <- rev(brewer.pal(length(unique(df$clarity)),'YlGnBu'))
# plot(y = df$logprice, x = df$logcarat, col=clar_palette[df$clarity_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Clarity by Price & Carat')
# cut_legend = c('FL','IF','VVS1','VVS2','VS1','VS2','SI1','SI2')
# legend("bottomright",legend = cut_legend,col=clar_palette,pch=1)
# 
# #color
# col_palette <- rev(brewer.pal(length(unique(df$color)),'YlGnBu'))
# plot(y = df$logprice, x = df$logcarat, col=col_palette[df$color_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Color by Price & Carat')
# color_legend = c('D','E','F','G','H','I','J')
# legend("bottomright",legend = color_legend,col=col_palette,pch=1)

# Feature Engineering ----
df$logprice <- log10(df$price)
df$logcarat <- log10(df$carat)
#display.brewer.all()
df$clarity_grouped <- 99
df$clarity_grouped[df$clarity == 'FL' | df$clarity == 'IF'] <- 'FL/IF' 
df$clarity_grouped[df$clarity == 'VVS1' | df$clarity == 'VVS2'] <- 'VVS1/2' 
df$clarity_grouped[df$clarity == 'VS1' | df$clarity == 'VS2'] <- 'VS1/2' 
df$clarity_grouped[df$clarity == 'SI1' | df$clarity == 'SI2'] <- 'SI1/2' 
df$clarity_grouped <- as.factor(df$clarity_grouped)

#For plotting purposes only
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




df$cut_grouped <- 99
df$cut_grouped[df$cut == 'Astor Ideal' | df$cut == 'Ideal'] <- 'Astor Ideal/Ideal'
df$cut_grouped[df$cut == 'Very Good' | df$cut == 'Good'] <- 'Very Good/Good'
df$cut_grouped <- as.factor(df$cut_grouped)

#For plotting purposes only
df$cut_rank <- 99
df$cut_rank[df$cut == 'Astor Ideal'] <- 1
df$cut_rank[df$cut == 'Ideal'] <- 2
df$cut_rank[df$cut == 'Very Good'] <- 3
df$cut_rank[df$cut == 'Good'] <- 4

#carat_g1: True if greater than 1 carat, False if less than
df$carat_g1 <- 99
df$carat_g1[df$carat >= 1] <- TRUE
df$carat_g1[df$carat <1] <- FALSE
df$carat_g1 <- as.factor(df$carat_g1)

#carat_g2: Exention of _g1, looks at more bins
#https://www.creditdonkey.com/diamond-prices.html
#Tells me that 0.5, 0.75, 1, 1.5, and 2 are useful levels for carat
df$carat_g2 <- 99
df$carat_g2[df$carat >= 5] <- '5 or more'
df$carat_g2[df$carat < 5] <- '4-4.99'
df$carat_g2[df$carat < 4] <- '3-3.99'
df$carat_g2[df$carat < 3] <- '2.5-2.99'
df$carat_g2[df$carat < 2.5] <- '2-2.49'
df$carat_g2[df$carat < 2] <- '1.5-1.99'
df$carat_g2[df$carat < 1.5] <- '1-1.49'
df$carat_g2[df$carat < 1] <- '0.75-0.99'
df$carat_g2[df$carat < 0.75] <- '0.5-0.74'
df$carat_g2[df$carat < 0.5] <- 'Less then 0.5'
df$carat_g2 <- as.factor(df$carat_g2)

#carat_g3: Extension of g_2, Step function implementation
df$carat_g3 <- 99
df$carat_g3[df$carat >= 5] <- floor(df$carat[df$carat >=5])
df$carat_g3[df$carat < 5] <- 4
df$carat_g3[df$carat < 4] <- 3
df$carat_g3[df$carat < 3] <- 2.5
df$carat_g3[df$carat < 2.5] <- 2
df$carat_g3[df$carat < 2] <- 1.5
df$carat_g3[df$carat < 1.5] <- 1
df$carat_g3[df$carat < 1] <- 0.75
df$carat_g3[df$carat < 0.75] <- 0.5
df$carat_g3[df$carat < 0.5] <- 0.25

#carat_g4: Exentions of _g2, add more bins for higher values of carat (break down 5 or more)
df$carat_g4 <- 99
df$carat_g4[df$carat >= 3] <- paste(as.character(floor(df$carat[df$carat >=3])), '-',as.character(floor(df$carat[df$carat >=3])+0.99))
df$carat_g4[df$carat < 3] <- '2.5-2.99'
df$carat_g4[df$carat < 2.5] <- '2-2.49'
df$carat_g4[df$carat < 2] <- '1.5-1.99'
df$carat_g4[df$carat < 1.5] <- '1-1.49'
df$carat_g4[df$carat < 1] <- '0.75-0.99'
df$carat_g4[df$carat < 0.75] <- '0.5-0.74'
df$carat_g4[df$carat < 0.5] <- 'Less then 0.5'
df$carat_g4 <- as.factor(df$carat_g4)

#carat_g5: extension of carat_g1, just 3 bins : < 1 carat, 1 carat, > 1.25 carat
df$carat_g5 <- '> 1.25 carats'
df$carat_g5[df$carat < 1.25 ] <- '1-1.24 carats'
df$carat_g5[df$carat < 1] <- '<1 carat'


#Plotting ----

hist(log10(df$carat), breaks = 200)

bc <- boxcox(price ~ log(carat) + color + clarity + cut, data = df)

best_lambda <- bc$x[bc$y == max(bc$y)]
best_lambda


png('carat_price.png')
plot(df$carat, df$price, xlab = 'Carat',ylab = 'Price', main = 'Diamond Carat vs. Price')
dev.off()

png('carat_logprice.png')
plot(df$carat, log10(df$price), xlab = 'Carat',ylab = 'log(Price)', main = 'Diamond Carat vs. log(Price)')
dev.off()

png('logcarat_logprice.png')
plot(log10(df$carat), log10(df$price), xlab = 'log(Carat)',ylab = 'log(Price)', main = 'Diamond log(Carat) vs. log(Price)')
dev.off()

# Modeling ----

#What linear model will provide the most accurate prediction for price?

#base model, price on all regressors

#Simple models ----
simple_mod <- lm(formula = price ~ carat + clarity + color + cut, data =df)
summary(simple_mod) #0.58
vif(simple_mod) #high for clarity and cut

sres_simple <- studres(simple_mod)

#Not normal
png(file = 'simple_mod_qq.png')
qqnorm(sres_simple)
qqline(sres_simple)
dev.off()


#Transform price
simple_mod2 <- lm(formula = logprice ~ carat + clarity + color + cut, data = df)
summary(simple_mod2) # 0.74, better
vif(simple_mod2) #clarity and cut still high

sres_simple2 <- studres(simple_mod2)

#Not normal
png(file = 'simple_mod2_qq.png')
qqnorm(sres_simple2)
qqline(sres_simple2)
dev.off()

#Transform carat too
simple_mod3 <- lm(formula = logprice ~ logcarat + clarity + color + cut, data = df)
summary(simple_mod3) # 0.981, very nice!

sres_simple3 <- studres(simple_mod3)

#Pretty close to normal
png(file = 'simple_mod3_qq.png')
qqnorm(sres_simple3)
qqline(sres_simple3)
dev.off()

#Homoskedastic
png(file = 'simple_mod3_homo.png')
plot(fitted.values(simple_mod3), sres_simple3)
dev.off()

#Some multi-collinearity in Clarity and Cut
vif(simple_mod3) #clarity and cut still high


#Advanced models ----

#Base model with log carat, grouped color and clarity, 

base_mod <- lm(formula = logprice ~ logcarat + clarity_grouped + color + cut_grouped, data = df)

summary(base_mod) #lowered to 0.9796

sres <- studres(base_mod)

#Normality is pretty good
png('base_mod_qq.png')
qqnorm(sres)
qqline(sres)
dev.off()

#Homoskedacity
png('base_mod_homo.png')
plot(fitted.values(base_mod), sres)
dev.off()

#No variance inflation
vif(base_mod) #great VIFs


#add interaction term for logcarat and color
base_mod2 <-  lm(formula = logprice ~ logcarat + clarity_grouped + color + cut_grouped + logcarat * color, data = df)
summary(base_mod2) #.98

#adding interaction term proved significant
anova(base_mod, base_mod2) 

sres2 <- studres(base_mod2)

#Normality is pretty good
png('base_mod2_qq.png')
qqnorm(sres2)
qqline(sres2)
dev.off()

#Homoskedacity
png('base_mod2_homo.png')
plot(fitted.values(base_mod2), sres2)
dev.off()

#No variance inflation
vif(base_mod) #great VIFs
vif(base_mod2) #looking good too

avPlots(base_mod2)
# 
# 
# 
# #change logcarat to carat_g2 based off research on creditdonkey about pricing tables
# base_mod3 <- lm(formula = price ~ logcarat + clarity_grouped + color + cut_grouped + logcarat*color, data =df)
# summary(base_mod3) #.983
# vif(base_mod3) #logcarat is very high
# 
# #change interaction term to carat_g2 as well to reduce VIF of carat
# base_mod4 <- lm(formula = logprice ~ carat_g5 + clarity_grouped + cut_grouped + logcarat*color, data =df)
# summary(base_mod4) #.95
# vif(base_mod4) 
