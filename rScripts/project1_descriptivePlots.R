library(haven)
library(ggplot2)

dd <- read.csv("/Users/Bradley/Box Sync/Data Science/STAT6021/Project1/clean_diamond_data.csv")


summary(dd)

#basic distrobutions:
logPrice <- ggplot(dd, aes(x=log10(price))) + geom_histogram(binwidth=0.025) + xlab('Log(Price)') + ylab('Number of Diamonds in Data') +ggtitle("Log Price of Diamonds in Dataset\n(N=210,638)") +  theme(plot.title = element_text(hjust = 0.5))


dd$carCat <- 99
dd$carCat[dd$carat < 0.25] <- "<0.25"
dd$carCat[0.25<=dd$carat & dd$carat<0.5] <- "0.25-0.49"
dd$carCat[0.5 <= dd$carat & dd$carat < 0.75] <- "0.5-0.74"
dd$carCat[0.75 <= dd$carat & dd$carat < 1] <- "0.75-0.99"
dd$carCat[1 <= dd$carat & dd$carat < 1.5] <- "1-1.49"
dd$carCat[1.5 <= dd$carat & dd$carat < 2] <- "1.5-1.99"
dd$carCat[2 <= dd$carat & dd$carat < 2.5] <- "2-2.49"
dd$carCat[2.5 <= dd$carat & dd$carat < 3] <- "2.5-2.99"
dd$carCat[3 <= dd$carat & dd$carat < 3.5] <- "3-3.49"
dd$carCat[3.5<= dd$carat & dd$carat < 4] <- "3.5-3.99"
dd$carCat[dd$carat >= 4] <- ">= 4"

dc <- subset(dd, select = carCat)
dc$carCat <- factor(dc$carCat,levels = c("<0.25", "0.25-0.49", "0.5-0.74", "0.75-0.99", "1-1.49", "1.5-1.99", "2-2.49", "2.5-2.99", "3-3.49", "3.5-3.99", ">= 4"))


ggplot(data.frame(dc), aes(x=carCat)) + geom_bar() + xlab('Carat Range') + ylab('Number of Diamonds in Data') +ggtitle("Range of Carats of Diamonds in Dataset\n(N=210,638)") +  theme(plot.title = element_text(hjust = 0.5))



#prediction 1:

df_pred_1 <- data.frame(logcarat=log10(0.30), clarity_grouped = "SI1/2", cut_grouped = 'Very Good/Good', color= "E")
pred_1 <- predict(base_mod2, df_pred_1, interval = 'prediction')
fit_1 <- 10^pred_1[1,1]
lower_1 <- 10^pred_1[1,2]
higher_1 <- 10^pred_1[1,3]

#prediction 2:

df_pred_2 <- data.frame(logcarat=log10(0.58), clarity_grouped = "SI1/2", cut_grouped = 'Very Good/Good', color= "J")
pred_2 <- predict(base_mod2, df_pred_2, interval = 'prediction')
fit_2 <- 10^pred_2[1,1]
lower_2 <- 10^pred_2[1,2]
higher_2 <- 10^pred_2[1,3]


#prediction 3:

df_pred_3 <- data.frame(logcarat=log10(0.85), clarity_grouped = "SI1/2", cut_grouped = 'Very Good/Good', color= "F")
pred_3 <- predict(base_mod2, df_pred_3, interval = 'prediction')
fit_3 <- 10^pred_3[1,1]
lower_3 <- 10^pred_3[1,2]
higher_3 <- 10^pred_3[1,3]


#prediction 4:

df_pred_4 <- data.frame(logcarat=log10(1.50), clarity_grouped = "VS1/2", cut_grouped = 'Very Good/Good', color= "J")
pred_4 <- 10^predict(base_mod2, df_pred_4, interval = 'prediction')


