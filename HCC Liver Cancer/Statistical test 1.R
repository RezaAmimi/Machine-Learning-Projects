#' ---

#' date: "Oct 23 2021"
#' ---
#' 

#'\
#' **STEP 1:** Null and Alternative Hypothesis\
#' We assume that data follows a normal distribution.\
#' Null Hypothesis --> **H0: p0 = 40%**\
#' Alternative Hypothesis --> **Ha: pa != 40%**\
p0 <- 40/100
#' Significance level is determined by the problem statement.\
alpha <- 0.05

#' **STEP 2:** Test Statistics\
pa <- 249/529
#' z-score is used for population proportion testing\
z_score <- (pa - p0)/sqrt(p0*(1 - p0)/529)
cat("Sample proportion: ", pa, ", z-score: ", z_score)
cat("Sample proportion is ", z_score, " standard deviations over 40%.")

#' **STEP 3:** P-value\
#' Based on normal distribution assumption:\
p_value <- 2*(1 - pnorm(z_score))
cat("p-value = ", p_value)

#' **STEP 4:*** Conclusion\
cat("p-value = ", p_value, " is less than significance level = ",
    alpha, " thus there is enough evidence to reject the null hypothesis.")


##' **PART 2)**\
#'\
#' **STEP 1:** Null and Alternative Hypothesis\
#' We assume that data follows a normal distribution.\
#' Null Hypothesis --> **H0: p0 = 40%**\
#' Alternative Hypothesis --> **Ha: pa > 40%**\
p0 <- 40/100
#' Significance level is determined by the problem statement.\
alpha <- 0.05

#' **STEP 2:** Test Statistics\
pa <- 134/326
#' z-score is used for population proportion testing\
z_score <- (pa - p0)/sqrt(p0*(1 - p0)/326)
cat("Sample proportion: ", pa, ", z-score: ", z_score)
cat("Sample proportion is ", z_score, " standard deviations over 40%.")

#' **STEP 3:** P-value\
#' Based on normal distribution assumption:\
p_value <- 2*(1 - pnorm(z_score))
cat("p-value = ", p_value)

#' **STEP 4:*** Conclusion\
cat("p-value = ", p_value, " is greater than significance level = ",
    alpha, " thus there is not enough evidence to reject the null hypothesis.")


##' **QUESTION 2)** --------------------------------------------------\
#'\
Pre_GDS <- c(12,10,16,2,12,18,11,16,16,10,14,21,9,19,20)
Post_GDS <- c(11,10,11,3,9,13,8,14,16,10,12,22,9,16,18)
#' **STEP 1:** Null and Alternative Hypothesis\
#' In this case, we have two samples and we want to 
#' check whether we can reject the null hypothesis that
#' they are being sampled from a same population (or in
#' other words, the medication had no effect.)\
#' We follow the assumption that the variances are unknown 
#' and their are not necessarily equal, therefore we use
#' t-statistic for p-value computation.

#' **STEP 2:** Test Statistics\
t_statistic <- (mean(Pre_GDS) - mean(Post_GDS))/
  sqrt(var(Pre_GDS)/length(Pre_GDS) + var(Post_GDS)/length(Post_GDS))

df <- (var(Pre_GDS)/length(Pre_GDS) + var(Post_GDS)/length(Post_GDS))^2/
  ((var(Pre_GDS)/length(Pre_GDS))^2/(length(Pre_GDS) - 1) +
   (var(Post_GDS)/length(Post_GDS))^2/(length(Post_GDS) - 1))

#' **STEP 3:** P-value\
#' Significance level is determined by the problem statement.\
alpha <- 0.05
p_value <- 1 - pt(t_statistic, df)

#' **STEP 4:*** Conclusion\
cat("p-value = ", p_value, " is greater than significance level = ", alpha, "thus there is not enough evidence to reject null hypothesis.")
cat("In other words, we cannot conclude that reminiscence therapy had any effect on GDS depression score of patients.")

##' **QUESTION 3)** --------------------------------------------------\
##' **PART 1)**\
#'\
#' Importing Data and Extracting Creatinine Phosphokinase concentration:\
data <- read.csv('heart_failure_clinical_records_dataset.csv')
CP <- data$creatinine_phosphokinase
#' **STEP 1:** Null and Alternative Hypothesis\
#' CP0: CP concentration in alive patients\
#' CP1: CP concentration in deceased patients\
CP0 <- CP[data$DEATH_EVENT == 0]
CP1 <- CP[data$DEATH_EVENT == 1]
#' Comparing sample variances to see if we can assume equal variances for testing.\
cat("Variance of CP0: ", var(CP0))
cat("Variance of CP1: ", var(CP1))
cat("Relative difference ", (var(CP0) - var(CP1))/var(CP1), " is high enough to assume non-equal variances.")
#' Hypothesis Testing: Two sample test with unknown variances with no assumptions:\
#' Null Hypothesis --> Averages of two alive and deceased patient groups are statistically the same.\
#' Alternative Hypothesis --> Averages significantly differ.\

#' **STEP 2:** Test Statistics\
t_statistic <- (mean(CP0) - mean(CP1))/
  sqrt(var(CP0)/length(CP0) + var(CP1)/length(CP1))

df <- (var(CP0)/length(CP0) + var(CP1)/length(CP1))^2/
  ((var(CP0)/length(CP0))^2/(length(CP0) - 1) +
     (var(CP1)/length(CP1))^2/(length(CP1) - 1))

#' **STEP 3:** P-value\
#' Significance level is determined by the problem statement.\
alpha <- 0.05
p_value <- 1 - pt(abs(t_statistic), df)

#' **STEP 4:*** Conclusion\
cat("p-value = ", p_value, " is greater than significance level = ", alpha, "thus there is not enough evidence to reject null hypothesis.")
cat("In other words, we cannot conclude that Creatinine Phosphokinase level in alive and deceased patients were significantly different.")

##' **PART 2)**\
#'\
#' Extracting diabetes data
D <- data$diabetes
#' **STEP 1:** Null and Alternative Hypothesis\
#' D0: Alive patients with diabetes\
#' D1: Deceased patients with diabetes\
D0 <- D[data$DEATH_EVENT == 0]
D1 <- D[data$DEATH_EVENT == 1]

#' **STEP 2:** Test Statistics\
n0 <- length(D0)
n1 <- length(D1)

p0 <- sum(D0)/n0
p1 <- sum(D1)/n1
p <- n0/(n0 + n1)*p0 + n1/(n0 + n1)*p1

z_score <- (p0 - p1)/sqrt(p*(1 - p)/n0 + p*(1 - p)/n1)

#' **STEP 3:** P-value\
#' Significance level is determined by the problem statement.\
alpha <- 0.05
p_value <- 2*(1 - pnorm(z_score))
cat("p-value = ", p_value)

#' **STEP 4:*** Conclusion\
cat("p-value = ", p_value, " is greater than significance level = ",
    alpha, " thus there is not enough evidence to reject the null hypothesis.")
cat("In other words, it is unlikely that the average number of patients with diabetes in alive and deceased groups significantly differ.")
