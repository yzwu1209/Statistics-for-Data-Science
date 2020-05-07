# install stargazer package
install.packages("stargazer", repos="http://cran.us.r-project.org")
library(stargazer)

getwd()
setwd("/Users/cati/Documents/Study/UCB/W203-Statistics_for_Data_Science/HomeWork/unit_10")

load("bwght.RData")
summary(data$bwght)
summary(data$cigs)

hist(data$bwght)
hist(data$cigs, breaks = 15)

plot(data$cigs, data$bwght, 
     xlab = "number of cigarettes smoked by the mother each day during pregnacy",
     ylab = "infant birth weight in ounces")

m1 <- lm(bwght ~ cigs, data = data)
abline(m1)

m1$coefficients

?options

plot(data)

str(data)

plot(data$faminc, data$bwght,
     xlab = "1988 family income, $1000s",
     ylab = "infant birth weight in ounces")

plot(data$fatheduc, data$bwght,
     xlab = "father's yrs of educ",
     ylab = "infant birth weight in ounces")

plot(data$motheduc, data$bwght,
     xlab = "mother's yrs of educ",
     ylab = "infant birth weight in ounces")

plot(data$parity, data$bwght,
     xlab = "birth order of child",
     ylab = "infant birth weight in ounces")

plot(data$packs, data$bwght,
     xlab = "packs smked per day while preg",
     ylab = "infant birth weight in ounces")

plot(data$lfaminc, data$bwght,
     xlab = "log(faminc)",
     ylab = "infant birth weight in ounces")

cor.test(data$faminc, data$bwght)
cor.test(data$fatheduc, data$bwght)
cor.test(data$motheduc, data$bwght)
cor.test(data$parity, data$bwght)
cor.test(data$cigs, data$bwght)
cor.test(data$packs, data$bwght)
cor.test(data$lfaminc, data$bwght)

model2 <- lm(bwght ~ faminc + fatheduc + motheduc + parity + cigs, data = data)
model3 <- lm(bwght ~ faminc + parity + cigs, data = data)
model4 <- lm(bwght ~ faminc + cigs, data = data)


stargazer(model2, type = "text", title = "Model 2 Results",
          ci.levels = .95, align = T, single.row = T)

stargazer(m1, model2, model3, model4,
          type="text", keep.stat=c("n", "adj.rsq"))

AIC(m1)
AIC(model2)
AIC(model3)
AIC(model4)

cor.test(data$cigtax, data$bwght)
cor.test(data$cigtax, data$cigs)
cor.test(data$cigprice, data$cigs)

?par
