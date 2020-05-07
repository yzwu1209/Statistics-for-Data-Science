
#####################################################################
## setting up
getwd()
setwd("/Users/cati/Documents/Study/UCB/W203-Statistics_for_Data_Science/HomeWork/lab_2")
A = read.csv("anes_pilot_2018.csv")

install.packages("car", dependencies = TRUE)
library(car)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
install.packages('plyr')
library(plyr)
install.packages("effsize", dependencies = TRUE)
library(effsize)
install.packages("gmodels")
library(gmodels)
install.packages("rcompanion")
library(rcompanion)
library(dplyr) 

#####################################################################
#### Question 1 ###
# basic EDA
# 1. Quick summary
summary(A$ftpolice)
summary(A$ftjournal)

# 2. Subset data
# Get a count table for [honest] variable
count(A, "honest")

# Remove cases where [honest] == 1
A_honest = subset(A, honest != 1)

# Remove cases where ftjournal is -7
A_q1 = subset(A_honest, 0 <= ftjournal)

# 3. Histogram
hist(A_q1$ftpolice, main = "Distribution of Rating", xlab = "Rating", 
     col = "#0000FF75", border = "white", breaks = 20)
hist(A_q1$ftjournal, add = T, col = "#B2222275", border = "white", breaks = 20)
legend("top", title = "Rating for", c("Police", "Journalist"), 
       fill = c("#0000FF75", "#B2222275"))

# 3. Boxplot
boxplot(A_q1$ftpolice, A_q1$ftjournal, main = "Distribution of Rating",
        ylab = "Rating Score", names = c("Rating for Police", "Rating for Journalist"))

# 4. Correlation
cor(A_q1$ftpolice, A_q1$ftjournal)

# Basic data check
summary(A_q1$ftpolice)
summary(A_q1$ftjournal)

# pull up a qq-plot to see how normal this variable looks
qqnorm(A_q1$ftpolice, main = "Normal Q-Q plot for ftpolice Variable")
qqnorm(A_q1$ftjournal, main = "Normal Q-Q plot for ftjournal Variable")
#####################################################################
# To be discarded
ggplot(A) + 
  geom_histogram(aes(x=A$ftpolice, y=..density..),  fill="lightblue", bins = 25) + 
  geom_histogram(aes(x=A$ftjournal, y=..density..),  fill="indianred1", alpha = 0.5, 
                 bins = 25, position = "dodge") +
  labs(title ="Histogram for Rating of Police and Journalist", x = "Rating", y = "Count") 
  
?geom_histogram
?labs

neg_rating_cases = subset(A, ftjournal < 0)
summary(neg_rating_cases$ftjournal)
nrow(neg_rating_cases)
str(neg_rating_cases)

#####################################################################
t.test(A_q1$ftpolice, A_q1$ftjournal, paired = T)

cohen.d(A_q1$ftpolice, A_q1$ftjournal)

#####################################################################
# For Kaiqi

summary(A$pid7)
length(A$pid7)
summary(A$pid7x)
length(A$pid7x)

#####################################################################
#### Question 3 ####
# basic EDA
# 1. Basic data check and visualization
summary(A_honest$pid7x)
summary(A_honest$russia16)
summary(A_honest$muellerinv)
count(A_honest, "pid7x")
count(A_honest, "russia16")
count(A_honest, "muellerinv")
cor(A_honest$russia16, A_honest$muellerinv)
hist(A_honest$pid7x, breaks = -7:8 - 0.5, xlim = c(-7,7))

# 2. Subset data
# Select cases where pid7x in (3,4,5) AND russia16 == 2 AND muellerinv not in (1,2,3)
A_q3 = subset(A_honest, pid7x %in% c(3, 4, 5) & russia16 != -7)
# A_mueller = subset(A_honest, !(muellerinv %in% c(1, 2, 3)))
# A_russia = subset(A_honest, russia16 == 2)
A_ind = subset(A_honest, pid7x %in% c(3, 4, 5))
# hist(A_ind$russia16)
# hist(A_ind$muellerinv)
A_contradict = subset(A_honest, (russia16 == 2 & muellerinv %in% c(1, 2)) | 
                      (russia16 == 1 & muellerinv %in% c(6, 7)))

# 3. Data peek through visualization
# Histogram
ggplot(A_q3,aes(x=russia16,group=pid7x,fill=pid7x)) +
  geom_histogram(position="dodge", binwidth = 0.5) + theme_bw() 

# boxplot(A_q3$russia16, main = "Distribution of Russia16",
#         ylab = "Russia16 Value")

# Calculate correlation


# ?ggplot
# hist(A_q3$russia16)
# ?wilcox.test
CrossTable(A_q3$russia16, A_q3$pid7x,
           prop.r=F, prop.c=F, expected=T,
           chisq=T, fisher=T,
           prop.chisq=F, prop.t=T)

CrossTable(A_q3$muellerinv, A_q3$pid7x,
           prop.r=F, prop.c=F, expected=T,
           chisq=T,
           prop.chisq=F, prop.t=T)

wilcox.test(A_q3$pid7x, A_q3$russia16)

# This is the test to use
wilcox.test(A_q3$russia16, mu = 1.5)

# Calculate effect size
mean_sample = mean(A_q3$russia16)
A_q3$difference <- 1.5 - A_q3$russia16
sd_diff = sd(A_q3$difference)
effectsize = abs(1.5 - mean_sample)/sd_diff

baseless = subset(A_q3, russia16 == 2)
base = subset(A_q3, russia16 == 1)
cohen.d(baseless$russia16, base$russia16)

CrossTable(A_honest$wall, A_honest$pid7x,
           prop.r=F, prop.c=F, expected=T,
           chisq=T,
           prop.chisq=F, prop.t=T)

#####################################################################
#### Question 5 ####
# Remove non-response
A_q5 = subset(A_honest, pid7x != -7)

# Add a new column "party" to categorize cases into three political standing
A_q5$party[A_q5$pid7x %in% c(1,2)] <- "Democ"
A_q5$party[A_q5$pid7x %in% c(6,7)] <- "Repub"
A_q5$party[A_q5$pid7x %in% c(3,4,5)] <- "Indep"

# Add a new column "partyn" to categorize cases into three political standing with numeric values
A_q5$partyn[A_q5$pid7x %in% c(1,2)] <- 1
A_q5$partyn[A_q5$pid7x %in% c(6,7)] <- 3
A_q5$partyn[A_q5$pid7x %in% c(3,4,5)] <- 2

# Add a new column "wallx" to categorize cases into three views towards border wall
A_q5$wallx[A_q5$wall %in% c(1,2,3)] <- "Favor"
A_q5$wallx[A_q5$wall == 4] <- "Neutral"
A_q5$wallx[A_q5$wall %in% c(5,6,7)] <- "Oppose"

# Add a new column "walln" to categorize cases into three views towards border wall with numeric values
A_q5$walln[A_q5$wall %in% c(1,2,3)] <- 1
A_q5$walln[A_q5$wall == 4] <- 2
A_q5$walln[A_q5$wall %in% c(5,6,7)] <- 3

count(A_q5$party)
count(A_q5$wallx)

# Histogram
ggplot(A_q5,aes(x=wall,group=party,fill=party)) +
  geom_histogram(position="dodge", binwidth = 0.5) + theme_bw() 

# Manipulate data for grouped bar plot
df <- A_q5 %>%
  group_by(party, wallx) %>%
  summarise(counts = n()) 

ggplot(df, aes(x = wallx, y = counts)) +
  geom_bar(
    aes(color = party, fill = party),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + theme_bw() + ggtitle("Distribution of Voter Opinion towards Border Wall by Party") +
  xlab("Voter Opinion on Border Wall") 

CrossTable(A_q5$party, A_q5$wallx,
           prop.r=F, prop.c=F, expected=T,
           chisq=T,
           prop.chisq=F, prop.t=T)

cohen.d(A_q5$partyn, A_q5$walln)
