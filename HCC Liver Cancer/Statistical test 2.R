#' ---

#' ---
#' 
#' **Q1 ---------------------------------------------------------------------**\
#' **Experimental Design**: Repeated Measure\
data1 <- read.csv("Q1.csv")
data_matrix1 <- matrix(data1[,1], nrow = 6, ncol = 3, 
                dimnames = list(seq(1,6), c(25,30,35)))
#' **Assumptions**:\
#' 1 - Assuming Normally Distributed Populations.\
#' 2 - All populations have a common variance.\
bartlett.test(RESPONSE ~ TEMP, data = data1)
#' The null hypothesis that the variances are equal is not rejected.\
#' **Pre-ANOVA**:\
#' 1 - Box plots:\
op <- par(mfrow = c(1, 3))
for (i in seq(ncol(data_matrix1))) {
  boxplot(data_matrix1[,i], ylim = c(min(data1[,1]), max(data1[,1])), 
          main = colnames(data_matrix1)[i], ylab = "Response")
}


#' **NULL HYPOTHESIS H0**: Populations means are equal.\
#' **ALTERNATIVE HYPOTHESIS Ha**: At least one of the samples is from a different population.\
#'
Response <- data1$RESPONSE
Temperature <-factor(data1$TEMP)
Subject <- factor(data1$SUBJ) 
res.aov <- aov(Response ~ Temperature + Subject, data = data1)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypothesis that the population means are equal is rejected as P-values for 
    both Temperature and Subject collumns are less than alpha = 0.05)")

##' **Q2 --------------------------------------------------------------------**\
#' **Experimental Design**: Repeated Measure\
#' 
data2 <- read.csv("Q2.csv")

Day0 <- data2[c(1,2,3)]
Day7 <- data2[c(1,2,4)]
Week1 <- data2[c(1,2,5)]
Week4 <- data2[c(1,2,6)]
Week8 <- data2[c(1,2,7)]
Week12 <- data2[c(1,2,8)]

Day0_matrix <- matrix(Day0[,3], nrow = 11, ncol = 2, 
                       dimnames = list(seq(1,11), c(1,2)))
Day7_matrix <- matrix(Day7[,3], nrow = 11, ncol = 2, 
                      dimnames = list(seq(1,11), c(1,2)))
Week1_matrix <- matrix(Week1[,3], nrow = 11, ncol = 2, 
                      dimnames = list(seq(1,11), c(1,2)))
Week4_matrix <- matrix(Week4[,3], nrow = 11, ncol = 2, 
                      dimnames = list(seq(1,11), c(1,2)))
Week8_matrix <- matrix(Week8[,3], nrow = 11, ncol = 2, 
                      dimnames = list(seq(1,11), c(1,2)))
Week12_matrix <- matrix(Week12[,3], nrow = 11, ncol = 2, 
                      dimnames = list(seq(1,11), c(1,2)))

#' **Assumptions**:\
#' 1 - Assuming Normally Distributed Populations.\
#' 2 - All populations have a common variance.\
bartlett.test(DAY0 ~ TREAT, data = Day0)
bartlett.test(DAY7 ~ TREAT, data = Day7)
bartlett.test(WEEK1 ~ TREAT, data = Week1)
bartlett.test(WEEK4 ~ TREAT, data = Week4)
bartlett.test(WEEK8 ~ TREAT, data = Week8)
bartlett.test(WEEK12 ~ TREAT, data = Week12)
#' The null hypothesis that the variances are equal is not rejected.\

#' **Pre-ANOVA**:\
#' 1 - Box plots:\
#' For **Day 0**:\
boxplot(DAY0 ~ TREAT ,data = Day0, main = "Day 0",
        xlab = "Treatment", ylab = "Serum Concentration")
#' For **Day 7**:\
boxplot(DAY7 ~ TREAT ,data = Day7, main = "Day 7",
        xlab = "Treatment", ylab = "Serum Concentration")
  #' For **Week 1**:\
boxplot(WEEK1 ~ TREAT ,data = Week1, main = "Week 1",
        xlab = "Treatment", ylab = "Serum Concentration")
#' For **Week 4**:\
boxplot(WEEK4 ~ TREAT ,data = Week4, main = "Week 4",
        xlab = "Treatment", ylab = "Serum Concentration")
#' For **Week 8**:\
boxplot(WEEK8 ~ TREAT ,data = Week8, main = "Week 8",
        xlab = "Treatment", ylab = "Serum Concentration")
#' For **Week 12**:\
boxplot(WEEK12 ~ TREAT ,data = Week12, main = "Week 12",
        xlab = "Treatment", ylab = "Serum Concentration")

#' **NULL HYPOTHESIS H0**: Populations means are equal .\
#' **ALTERNATIVE HYPOTHESIS Ha**: At least one of the samples is from a different population.\
#'
Response1 <- Day0$DAY0
Response2 <- Day7$DAY7
Response3 <- Week1$WEEK1
Response4 <- Week4$WEEK4
Response5 <- Week8$WEEK8
Response6 <- Week12$WEEK12

Treatment <-factor(Day0$TREAT)
Patient <- factor(Day0$PATIENT)

res.aov <- aov(Response1 ~ Treatment + Patient, data = Day0)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is not rejected for 2 treatments in Day 0,
    therefore the treatments are equal in this case. But for different 
    patients the treatments were biased.")
res.aov <- aov(Response2 ~ Treatment + Patient, data = Day7)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is rejected for 2 treatments in Day 7,
    therefore the treatments are not equal in this case. But for different 
    patients the treatment are equal.")
res.aov <- aov(Response3 ~ Treatment + Patient, data = Week1)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is not rejected for 2 treatments in Day 0,
    therefore the treatments are equal in this case. Also for different 
    patients the treatment are equal.")
res.aov <- aov(Response4 ~ Treatment + Patient, data = Week4)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is not rejected for 2 treatments in Day 0,
    therefore the treatments are equal in this case. But for different 
    patients the treatments were biased.")
res.aov <- aov(Response5 ~ Treatment + Patient, data = Week8)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is not rejected for 2 treatments in Day 0,
    therefore the treatments are equal in this case. Also for different 
    patients the treatment are equal.")
res.aov <- aov(Response6 ~ Treatment + Patient, data = Week12)
summary(res.aov)
TukeyHSD(res.aov)
cat("The null hypthesis is not rejected for 2 treatments in Day 0,
    therefore the treatments are equal in this case. But for different 
    patients the treatments were biased.")
##' **Q3 --------------------------------------------------------------------**\
#' **Experimental Design**: Completely Randomized\
data3 <- read.csv("Q3.csv")

BW_data <- data3[c(1,2,3)]
HW_data <- data3[c(1,2,4)]
LW_data <- data3[c(1,2,5)]
KW_data <- data3[c(1,2,6)]
SW_data <- data3[c(1,2,7)]
CERUL_data <- data3[c(1,2,8)]


#' **Pre-ANOVA**:\
#' 1 - Box plots:\
boxplot(BW ~ DIET ,data = BW_data, main = "Body Weight",
        xlab = "Diet", ylab = "BW")
boxplot(HW ~ DIET ,data = HW_data, main = "Heart Weight",
        xlab = "Diet", ylab = "HW")
boxplot(LW ~ DIET ,data = LW_data, main = "Liver Weight",
        xlab = "Diet", ylab = "LW")
boxplot(KW ~ DIET ,data = KW_data, main = "Kidney Weight",
        xlab = "Diet", ylab = "KW")
boxplot(SW ~ DIET ,data = SW_data, main = "Spleen Weight",
        xlab = "Diet", ylab = "SW")
boxplot(CERUL ~ DIET ,data = CERUL_data, main = "Ceruloplasmin",
        xlab = "Diet", ylab = "CERUL")

#' **Assumptions**:\
#' 1 - Assuming Normally Distributed Populations.\
#' 2 - All populations have a common variance.\
bartlett.test(BW ~ DIET, data = BW_data) #
bartlett.test(HW ~ DIET, data = HW_data) #
bartlett.test(LW ~ DIET, data = LW_data)
bartlett.test(KW ~ DIET, data = KW_data)
bartlett.test(SW ~ DIET, data = SW_data) #
bartlett.test(CERUL ~ DIET, data = CERUL_data) #
#' Based on Bartlett's test results, given alpha = 0.05, the p-values are less than alpha,\
#' hance ANOVA cannot be used for these data.


#' **NULL HYPOTHESIS H0**: Populations means are equal.\
#' **ALTERNATIVE HYPOTHESIS Ha**: At least one of the samples is from a different population.\
#'
res.aov <- aov(LW_data$LW ~ factor(LW_data$DIET), data = LW_data)
summary(res.aov)
cat("P-value is smaller than 0.05: The null hypothesis is rejected and the mean of Liver weights for different groups is not similar.")
res.aov <- aov(KW_data$KW ~ factor(KW_data$DIET), data = KW_data)
summary(res.aov)
cat("P-value is larger than 0.05: The null hypothesis is not rejected and the mean of Liver weights for different groups is similar.")

##' **Q4 --------------------------------------------------------------------**\
#' **Experimental Design**: Completely Randomized\
data4 <- read.csv("Q4.csv")

data4A <- data4[,c(1,3)]
data4B <- data4[,c(2,3)]

#' **Pre-ANOVA**:\
#' 1 - Box plots:\
boxplot(Count ~ Sens ,data = data4A, main = "Cell Count",
        xlab = "Senstization Status", ylab = "Count")

boxplot(Count ~ Treat ,data = data4B, main = "Cell Count",
        xlab = "Exposure Status", ylab = "Count")

#' **Assumptions**:\
#' 1 - Assuming Normally Distributed Populations.\
#' 2 - All populations have a common variance.\
bartlett.test(Count ~ Sens, data = data4A)
bartlett.test(Count ~ Treat, data = data4B)
#' Although the null hypothesis of similar variances is rejected in first factor,\
#' I continue to apply ANOVA.

#' **a)**\
res.aov <- aov(data4A$Count ~ factor(data4A$Sens), data = data4A)
TukeyHSD(res.aov)
cat("P-value is smaller than 0.05: The population means of two sensetization outcomes are different")
#' **b)**\
res.aov <- aov(data4B$Count ~ factor(data4B$Treat), data = data4B)
summary(res.aov)
TukeyHSD(res.aov)
cat("P-value is smaller than 0.05: The population means of three exposure groups are different")
#' **c)**\
res.aov <- aov(data4B$Count ~ factor(data4B$Treat) + factor(data4A$Sens) + factor(data4A$Sens)*factor(data4B$Treat), data = data4)
summary(res.aov)
TukeyHSD(res.aov)
cat("P-value of combined effect is larger than 0.05: The population means ofcombined effects are equal.")