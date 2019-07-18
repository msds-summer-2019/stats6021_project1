#Project 1

#import statements
#install.packages('reshape2')
library("reshape2")
#install.packages('plyr')
library("plyr")
#install.packages('ggplot2')
library("ggplot2")
#install.packages('dplyr')
library("dplyr")
#install.packages('data.table')
library(data.table)
#install.packages('mltools')
library(mltools)
#install.packages('MASS')
library(MASS)
#install.packages('car')
library(car)
#fix issue b/w dplyer and MASS with select
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)
#install.packages('DAAG')
library(DAAG)
#install.packages('xtable')
library(xtable)
#Set options for xtable
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
#install.packages('RColorBrewer')
library(RColorBrewer)

#setwd('C:\\Users\\cmp2c\\Desktop\\Summer2019\\STAT6021\\Project1')
#EDA ----
#Read in data
df <- read.csv('clean_diamond_data.csv')

#Get sense of format of data
str(df)

#create a table dataframe of df for plotting purposes
ddf <- tbl_df(df)

# Feature Engineering ----
df$logprice <- log10(df$price)
df$logcarat <- log10(df$carat)
#display.brewer.all()

#Grouping clarity into 4 groups
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

#Grouping cut into 2 groups
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


#cut
#Use ColorBrewer's palette to assign values of cut to the palette
cut_palette <- rev(brewer.pal(length(unique(df$cut)),'YlGnBu'))
#plot log carat, log price, and cut, use cut_rank to imply order
plot(y = df$logprice, x = df$logcarat, col=cut_palette[df$cut_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Cut by Price & Carat')
#add the legend
cut_legend = c('Astor Ideal','Ideal','Very Good','Good')
legend("bottomright",legend = cut_legend,col=cut_palette,pch=1)

#clarity
#assign palette to clarity values
clar_palette <- rev(brewer.pal(length(unique(df$clarity)),'YlGnBu'))
#plot log carat, log price, and clarity, use clarity_rank to imply order
plot(y = df$logprice, x = df$logcarat, col=clar_palette[df$clarity_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Clarity by Price & Carat')
#add the legend to the plot
cut_legend = c('FL','IF','VVS1','VVS2','VS1','VS2','SI1','SI2')
legend("bottomright",legend = cut_legend,col=clar_palette,pch=1)

#color
#assign palette to color values (ironic)
col_palette <- rev(brewer.pal(length(unique(df$color)),'YlGnBu'))
#plot log carat, log price, and color
plot(y = df$logprice, x = df$logcarat, col=col_palette[df$color_rank], xlab = 'log(carat)', ylab = 'log(price)', main = 'Color by Price & Carat')
color_legend = c('D','E','F','G','H','I','J')
legend("bottomright",legend = color_legend,col=col_palette,pch=1)

# Modeling ----

#What linear model will provide the most accurate prediction for price?

#base model, price on all regressors

#Simple models ----

#Model 1----
#start with main effects, no transformations
simple_mod <- lm(formula = price ~ carat + clarity + color + cut, data =df)
summary(simple_mod) #0.58
vif(simple_mod) #high for clarity and cut

#Get externally studentized residuals
sres_simple <- studres(simple_mod)

#Q-Q plot to test normality of residuals
png(file = 'simple_mod_qq.png')
qqnorm(sres_simple, main = 'Model 1 - Normality of Residuals')
qqline(sres_simple)#Not normally distributed 
dev.off()

#Model 2----
#Transform price since residuals were not normal
simple_mod2 <- lm(formula = logprice ~ carat + clarity + color + cut, data = df)
summary(simple_mod2) # 0.74, better
vif(simple_mod2) #clarity and cut still high

#Get externally studentized residuals
sres_simple2 <- studres(simple_mod2)

#Q-Q plot to test normality of residuals
png(file = 'simple_mod2_qq.png')
qqnorm(sres_simple2, main = 'Model 2 - Normality of Residuals')
qqline(sres_simple2)#Not normal

dev.off()

#Model 3----
#Transform carat too in hopes of achieving normally distributed residuals
simple_mod3 <- lm(formula = logprice ~ logcarat + clarity + color + cut, data = df)
summary(simple_mod3) # 0.981 R-squared, very nice!

#Get externally studentized residuals
sres_simple3 <- studres(simple_mod3)

#Q-Q plot to test normality of residuals
png(file = 'simple_mod3_qq.png')
qqnorm(sres_simple3, main = 'Model 3 - Normality of Residuals')
qqline(sres_simple3) #Pretty close to normal
dev.off()

#Plot fitted values vs. residuals to test for homoscedasticity
png(file = 'simple_mod3_homo.png')
plot(fitted.values(simple_mod3), sres_simple3, xlab = 'Predicted Price', ylab = 'Residuals', main = 'Model 3 - Homoscedasticity') #homoscedastic
dev.off()

#Test multi-collinearity in the model with Variance Inflation Factors
vif(simple_mod3) #clarity and cut are high (>10)

#Write output of VIF to a csv for external plotting purposes
write.csv((data.frame(vif(simple_mod3))), 'simple_mod3_vifs.csv')

#Model 4----

#Keep log carat and log price, but we group clarity and cut to try and reduce their VIFs
base_mod <- lm(formula = logprice ~ logcarat + clarity_grouped + color + cut_grouped, data = df)

summary(base_mod) # R-squared lowered very slightly to 0.9796

#Get externally studentized residuals
sres <- studres(base_mod)

#Q-Q plot to test normality of residuals
png('base_mod_qq.png')
qqnorm(sres, main = 'Model 4 - Normality of Residuals')
qqline(sres) #Normality is pretty good
dev.off()

#Plot fitted values vs. residuals to test for homoscedasticity
png('base_mod_homo.png')
plot(fitted.values(base_mod), sres, xlab = 'Predicted Price', ylab = 'Residuals',
     main = 'Model 4 - Homoscedasticity') #homoscedastic
dev.off()

#Test multi-collinearity in the model with Variance Inflation Factors
vif(base_mod) #No major variance inflation

#Write output of VIF to a csv for external plotting purposes
write.csv((data.frame(vif(base_mod))), 'base_mod_vifs.csv')

#Model 5 ----

#Based off of research, color and logcarat seem to have an interaction effect. This model will test that hypothesis
base_mod2 <-  lm(formula = logprice ~ logcarat + clarity_grouped + color + cut_grouped + logcarat * color, data = df)
summary(base_mod2) #R-squared is 0.98

#Running partial F-test shows significance of adding a new interaction term to the model
xtable(anova(base_mod, base_mod2))#adding interaction term proved significant

#Get externally studentized residuals
sres2 <- studres(base_mod2)

#Q-Q plot to test normality of residuals
png('base_mod2_qq.png')
qqnorm(sres2, main = 'Model 5 - Normality of Residuals')
qqline(sres2)#Normality is pretty good
dev.off()

#Plot fitted values vs. residuals to test for homoscedasticity
png('base_mod2_homo.png')
plot(fitted.values(base_mod2), sres2, xlab = 'Predicted Price', ylab = 'Residuals',
     main = 'Model 5 - Homoscedasticity') #Homoskedac
dev.off()

#Test multi-collinearity in the model with Variance Inflation Factors
vif(base_mod2) #no major variance inflation

#Write output of VIF to a csv for external plotting purposes
write.csv((data.frame(vif(base_mod2))), 'base_mod2_vifs.csv')

#Generate added value plots for Model 5
png('base_mod2_avPlots.png')
avPlots(base_mod2, main = 'Model 5 - Added Variable Plots') #all variables seem to have an effect on the model, since their slopes are all non-zero. Interestingly, the interaction between color and logcarat shows that as color "worsens" the impact on price increases (severity of slope increases)
dev.off()

#Generate leverage plots to identify  highly influential points
png('base_mod2_leverage.png')
leveragePlots(base_mod2, main = 'Model 5 - Leverage Plots')

#Cooks distance, not used in our final presentation or report
cd_cutoff <- 4/((nrow(df)-length(base_mod2$coefficients)-2)) 

#Plotting the residuals with cooks distance, not used in final pres or report
plot(base_mod2, which = 4, cooks.levels = cd_cutoff)

#Ploting leverage with cooks distance super imposed, not used in final pres or report
plot(base_mod2, which = 6, cooks.levels = cd_cutoff)



#measure prediction interval length for a sample diamond of 1.5 carats, VS2 Clarity, very good cut, and J color (for models 4 and 5)
df_pred1 <- data.frame(logcarat = log10(1.50), clarity_grouped = 'VS1/2', cut_grouped = 'Very Good/Good', color = 'J')

#inputs used for  model 3
df_pred2 <- data.frame(logcarat = log10(1.50), clarity = 'VS2', cut = 'Very Good', color = 'J')

#Examinging prediction interval lengths between model 4 and 5 after adding interaction term
pred4_simple_mod3 <- 10^predict(simple_mod3, df_pred2, interval = 'prediction')
pred4_base_mod <- 10^predict(base_mod, df_pred1, interval = 'prediction')
pred4_base_mod2 <- 10^predict(base_mod2, df_pred1, interval = 'prediction')

#Also look at confidence interval out of curiosity
conf4_base_mod2 <- 10^predict(base_mod2, df_pred1, interval = 'confidence')

#Calculate prediction interval lengths for Models 3,4, and 5
pred_int_length_simple_mod3 <- pred4_simple_mod3[1,3] - pred4_simple_mod3[1,2]
pred_int_length_base_mod <- pred4_base_mod[1,3] - pred4_base_mod[1,2]
pred_int_length_base_mod2 <- pred4_base_mod2[1,3] - pred4_base_mod2[1,2]

#Calcualte the MSres for each model to demonstrate the reduction in prediction interval length
base_mod_msres <- anova(base_mod)$'Mean Sq'[5]
base_mod2_msres <- anova(base_mod2)$'Mean Sq'[6]

base_mod_msres
base_mod2_msres

#### - Unused code ----
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
