#' ---

#' ---
#' <font size = "5"> **TASK1: Data Analysis** </font>\
#' **------------------------------------------------------------------------**\
#' <font size = "3"> **a) Importing Required Libraries** </font>\
#' **------------------------------------------------------------------------**\

library(ggplot2)
library(psych)
library(ggfortify)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(pca3d)
library(scatterplot3d)
library(patchwork)
library(cluster)
library(factoextra)
library(philentropy)
library(ggdendro)
library(dendextend)
library(colorspace)
library(class)
library(caTools) 
library(caret)
library(e1071)
library(rgl)
library(nnet)
library(mlbench)
library(FSinR)
library(gtools)
library(naivebayes)

#' **------------------------------------------------------------------------**\
#' <font size = "3"> **b) Importing & Describing Data** </font>\
#' **------------------------------------------------------------------------**\
#' Reading data:\
data <- read.table('hcc-data.txt', sep = ',')
#' Changing the column names:\
#' Orig.titles: Literal column titles\
#' titles: Shortened column titles for convenience\

Orig.titles <- c("Gender",
                 "Symptoms",
                 "Alcohol",
                 "Hepatitis B Surface Antigen",
                 "Hepatitis B e Antigen",
                 "Hepatitis B Core Antibody",
                 "Hepatitis C Virus Antibody",
                 "Cirrhosis",
                 "Endemic Countries",
                 "Smoking",
                 "Diabetes",
                 "Obesity",
                 "Hemochromatosis",
                 "Arterial Hypertension",
                 "Chronic Renal Insufficiency",
                 "Human Immunodeficiency Virus",
                 "Nonalcoholic Steatohepatitis",
                 "Esophageal Varices",
                 "Splenomegaly",
                 "Portal Hypertension",
                 "Portal Vein Thrombosis",
                 "Liver Metastasis",
                 "Radiological Hallmark",
                 "Age at diagnosis",
                 "Grams of Alcohol per day",
                 "Packs of cigarets per year",
                 "Performance Status",
                 "Encefalopathy degree",
                 "Ascites degree",
                 "International Normalised Ratio",
                 "Alpha-Fetoprotein",
                 "Haemoglobin",
                 "Mean Corpuscular Volume",
                 "Leukocytes",
                 "Platelets",
                 "Albumin",
                 "Total Bilirubin",
                 "Alanine transaminase",
                 "Aspartate transaminase",
                 "Gamma glutamyl transferase",
                 "Alkaline phosphatase",
                 "Total Proteins",
                 "Creatinine",
                 "Number of Nodules",
                 "Major dimension of nodule",
                 "Direct Bilirubin",
                 "Iron",
                 "Oxygen Saturation",
                 "Ferritin",
                 "Death Status")

titles <- c("G", "Sym", "Alch", "HBSA", "HBEA", 
            "HBCA", "HCVA", "C", "EC",
            "Smoke", "Diab", "Obes", "HC",
            "AH", "CRI", "HIV", "NS", "EV",
            "Splen","PH", "PVT", "LM", "RH",
            "AaD", "GoApD", "PoCpY", "PS", "ED",
            "AD", "INR", "AF", "HG", "MCV", "Leuk",
            "Plate", "Alb", "TB", "AlaT", "AspT", "GGT",
            "AP", "TP", "Creat","NoN", "MDoN",
            "DB", "Iron", "OS", "Ferr", "Stat")

title.dict <- c("G" = "Gender", 
                "Sym" = "Symptoms", 
                "Alch" = "Alcohol", 
                "HBSA" = "Hepatitis B Surface Antigen", 
                "HBEA" = "Hepatitis B e Antigen", 
                "HBCA" = "Hepatitis B Core Antibody", 
                "HCVA" = "Hepatitis C Virus Antibody", 
                "C" = "Cirrhosis", 
                "EC" = "Endemic Countries",
                "Smoke" = "Smoking", 
                "Diab" = "Diabetes", 
                "Obes" = "Obesity", 
                "HC" = "Hemochromatosis",
                "AH" = "Arterial Hypertension", 
                "CRI" = "Chronic Renal Insufficiency", 
                "HIV" = "Human Immunodeficiency Virus", 
                "NS" = "Nonalcoholic Steatohepatitis", 
                "EV" = "Esophageal Varices",
                "Splen" = "Splenomegaly",
                "PH" = "Portal Hypertension", 
                "PVT" = "Portal Vein Thrombosis", 
                "LM" = "Liver Metastasis", 
                "RH" = "Radiological Hallmark",
                "AaD" = "Age at diagnosis", 
                "GoApD" = "Grams of Alcohol per day", 
                "PoCpY" = "Packs of cigarets per year", 
                "PS" = "Performance Status", 
                "ED" = "Encefalopathy degree",
                "AD" = "Ascites degree", 
                "INR" = "International Normalised Ratio", 
                "AF" = "Alpha-Fetoprotein", 
                "HG" = "Haemoglobin", 
                "MCV" = "Mean Corpuscular Volume", 
                "Leuk" = "Leukocytes",
                "Plate" = "Platelets", 
                "Alb" = "Albumin", 
                "TB" = "Total Bilirubin",
                "AlaT" = "Alanine transaminase", 
                "AspT" = "Aspartate transaminase", 
                "GGT" = "Gamma glutamyl transferase",
                "AP" = "Alkaline phosphatase", 
                "TP" = "Total Proteins", 
                "Creat" = "Creatinine",
                "NoN" = "Number of Nodules", 
                "MDoN" = "Major dimension of nodule",
                "DB" = "Direct Bilirubin", 
                "Iron" = "Iron", 
                "OS" = "Oxygen Saturation", 
                "Ferr" = "Ferritin", 
                "Stat" = "Death Status")


names(data) <- titles

#' Replacing "?" in data with NA:\
data[data == "?"] = NA
#' Separating discrete (nominal and ordinal) and continuous variables:\

discrete.vars.ind <- c(1:23,27,28,29,44,50)
continuous.vars.ind <- c(24,25,26,30:43,45:49)

discrete.vars <- titles[discrete.vars.ind]
continuous.vars <- titles[continuous.vars.ind]

discrete.vars.orig <- titles[discrete.vars.ind]
continuous.vars.orig <- titles[continuous.vars.ind]
#' Converting categorical data to factor type:\
data[discrete.vars] <- lapply(data[discrete.vars], as.numeric)
data[discrete.vars] <- lapply(data[discrete.vars], as.factor)
#' Converting continuous data to numeric type:\
data[continuous.vars] <- lapply(data[continuous.vars], as.numeric)


#' Data summary before pre-processing:\
summary(data)


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **c) Pre-processing** </font>\
#' **------------------------------------------------------------------------**\
#'
#' **1) Analyzing the number of NA in each column:**\
NA.counts <- data.frame(titles, colSums(is.na(data))/nrow(data))
names(NA.counts) <- c("Variables", "Frequency")
#' We assume that the NA frequency (relative number of NAs with respect to number of rows) of 
#' larger than 0.05 is not acceptable for discrete variable and therefore the corresponding columns can be ignored.\
#' Same goes for continuous variables with NA frequency of 0.2, as NA in continuous variables have the chance
#' to be replaced with total mean value of the corresponding column.\
NA.threshold <- ifelse((NA.counts$Frequency > 0.05) & (titles %in% discrete.vars), TRUE, FALSE)
NA.threshold <- NA.threshold | ifelse((NA.counts$Frequency > 0.2) & (titles %in% continuous.vars), TRUE, FALSE)

NA.color <- ifelse(NA.threshold == TRUE, 2, 4)

#' Columns designated with red markers in the plot are assumed redundant and will be dropped.\
ggplot(NA.counts, aes(x = Variables, y = Frequency)) + 
  geom_point(color = NA.color, shape = 7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("NA Frequency Plot for All Columns") +
  xlab("Variables") + ylab("Frequency")

#' Dropping columns and updating dataset:\
removed.vars <- titles[NA.threshold]
remained.vars <- titles[!NA.threshold]
data <- data[,!NA.threshold]

continuous.vars.r <- continuous.vars[continuous.vars %in% remained.vars]
discrete.vars.r <- discrete.vars[discrete.vars %in% remained.vars]


#' **2) Processing remaining NAs:**\
#' Replacing NA in continuous columns with mean values of the column:\
for(i in continuous.vars.r){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
#' Omit the rows with any remaining NAs:\
data <- na.omit(data)



#' **3) Analyzing thw number of outliers in each column:**\
#' Plotting boxplots to demonstrate the scope of the outlier problem:
par(mfrow = c(4,4), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
for (i in seq(1:length(continuous.vars.r))) boxplot(data[,continuous.vars.r[i]]~data$Stat,
                                                    main = title.dict[continuous.vars.r[i]],
                                                    xlab = continuous.vars.r[i], ylab = NULL, 
                                                    col = sample(2:8, 1))

#' Saving the number outliers using the 
OUT.list <- list(rep(NA, length(continuous.vars.r)))
OUT.counts <- data.frame(continuous.vars.r, rep(length(continuous.vars.r)))
names(OUT.counts) <- c("Variables", "Frequency")

for (i in seq(1:length(continuous.vars.r))) {
  
  
  OUT.list[i] <- list(boxplot.stats(data[,continuous.vars.r[i]])$out)
  OUT.counts[i,2] <- length(OUT.list[[i]])/nrow(data)
}


OUT.threshold <- ifelse(OUT.counts$Frequency > 0.1, FALSE, TRUE)
OUT.color <- ifelse(OUT.threshold == FALSE, 2, 4)

ggplot(OUT.counts, aes(x = Variables, y = Frequency)) + 
  geom_point(color = OUT.color, shape = 7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("NA Frequency Plot for All Columns") +
  xlab("Variables") + ylab("Frequency") + ylim(c(0,1))


for (i in seq(1:length(continuous.vars.r))) {

  data[(data[,continuous.vars.r[i]] %in% OUT.list[[i]]), continuous.vars.r[i]] <- 
    mean(data[!(data[,continuous.vars.r[i]] %in% OUT.list[[i]]), continuous.vars.r[i]])
}

data <- data[, c(continuous.vars.r[OUT.threshold], discrete.vars.r)]
continuous.vars.r <- continuous.vars.r[OUT.threshold]

par(mfrow = c(4,4), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
for (i in seq(1:length(continuous.vars.r))) boxplot(data[,continuous.vars.r[i]]~data$Stat,
                                                    main = title.dict[continuous.vars.r[i]],
                                                    xlab = continuous.vars.r[i], ylab = NULL, 
                                                    col = sample(2:8, 1))


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **d) Distribution Analysis** </font>\
#' **------------------------------------------------------------------------**\
#' 
#' Distribution of each selected variable is plotted:\
par(mfrow = c(4,4), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
for (i in seq(1:length(continuous.vars.r))) {
  hist(data[data$Stat == 0,continuous.vars.r[i]], prob = TRUE, 20, 
       main = title.dict[continuous.vars.r[i]], xlab = continuous.vars.r[i], ylab = "density",
       col = sample(3:8, 1))

  lines(density(data[,continuous.vars.r[i]]),
       main = continuous.vars.r[i],
       xlab = continuous.vars.r[i], ylab = "density", 
       col = 1, lwd = 3)
}



#' QQ-plots to investigate the normality of the variables is plotted:\
par(mfrow = c(4,4), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
for (i in seq(1:length(continuous.vars.r))) {
  qqnorm(data[,continuous.vars.r[i]], main = title.dict[continuous.vars.r[i]])
  qqline(data[,continuous.vars.r[i]], col = 2)
}


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **e) Pairwise Correlation*\ </font>\
#' **------------------------------------------------------------------------**\
#' 

par(mfrow = c(1,1), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
corrplot(cor(data[,continuous.vars.r]), method="number", type="upper", col = brewer.pal(n=10, name="RdBu"))

pairs.panels(data[,continuous.vars.r], smooth = TRUE, density = TRUE,
             ellipses = TRUE)


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **f) Dimension Reduction*\ </font>\
#' **------------------------------------------------------------------------**\
#' 
#' To use prcomp function for PCA analysis, we have to convert all remaining variables to numeric type.\

data.alt <- data

levels(data.alt$Stat)[levels(data.alt$Stat) == "1"] <- "Survived"   
levels(data.alt$Stat)[levels(data.alt$Stat) == "0"] <- "Deceased"   


for (i in 1:(length(discrete.vars.r) - 1)) data.alt[,discrete.vars.r[i]] <- as.numeric(data.alt[,discrete.vars.r[i]])

data.pca <- prcomp(data.alt[-ncol(data.alt)], center = TRUE, scale = TRUE)

data.pca

summary(data.pca)

head(data.pca$x)

plot(data.pca, type = 'l', col = 2, ylim = c(0, 4), main = "Variances of Principal Components")


par(mfrow = c(1,1), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
autoplot(data.pca, data = data.alt, colour = discrete.vars.r[14],
         loadings = TRUE, loadings.colour = 'green',
         loadings.label = TRUE, loadings.label.size = 3.5)

colors <- c("red", "green3", "blue3")
colors <- colors[as.numeric(data$Stat)]


scp3 <- scatterplot3d(data.pca$x[,1:3], color = colors,pch = 16, lwd = 2,
              main="3D Scatter Plot",
              xlab = "PC1",
              ylab = "PC2",
              zlab = "PC3",
              angle = 50)
legend(scp3$xyz.convert(4.5, 0, 0), col= c("green","red"), bg="white", lty=c(1,1), lwd=2, yjust=0, legend = c("Survived", "Deceased"), cex = 1.1)


#' <font size = "5"> **TASK2: CLUSTERING** </font>\
#' **------------------------------------------------------------------------**\
#' <font size = "3"> **a) Hierarchical Clustering*\ </font>\
#' **------------------------------------------------------------------------**\
#' 

h.clust.func <- function(data, Target, dist.method, cluster.method) {
  
dist.data <- get_dist(data, method = dist.method)

hc.data <- hclust(dist.data, method = cluster.method)


groups <- cutree(hc.data, k = 2)

co_x <- cophenetic(hc.data)

cat("------------------------------------------\n")
cat("Distance Matrix Visualization\n")
cat("------------------------------------------\n")
plot(fviz_dist(dist.data))

dend <- as.dendrogram(hc.data)

dend <- rotate(dend, 1:nrow(data))

dend <- color_branches(dend, k = 2)


labels_colors(dend) <-
  rainbow_hcl(2)[sort_levels_values(
    as.numeric(Target)[order.dendrogram(dend)]
  )]


labels(dend) <- paste(as.character(data[,ncol(data)])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")

dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- set(dend, "labels_cex", 0.5)

par(mar = c(3,3,3,7))


temp_plot<- plot(dend, 
                 main = "Clustered Data", 
                 horiz =  TRUE,  nodePar = list(cex = .007))


cat("------------------------------------------\n")
cat("Cophenetic Coefficient:", cor(dist.data, co_x), "\n")
cat("------------------------------------------\n")
}

h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "complete")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "complete")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "complete")

h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "single")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "single")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "single")

h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "average")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "average")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "average")

h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "mcquitty")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "mcquitty")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "mcquitty")

h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "median")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "median")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "median")


h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "euclidean", cluster.method = "centroid")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "manhattan", cluster.method = "centroid")
h.clust.func(data.alt[-ncol(data.alt)], data.alt$Stat, dist.method = "pearson", cluster.method = "centroid")


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **b) K-Means Clustering*\ </font>\
#' **------------------------------------------------------------------------**\
#' 


kmean.clust <- function(data) {
set.seed(1111)
k = 2
  
data.kmeans <- kmeans(data, k)

colors <- 2:(k + 1)

plot(fviz_cluster(data.kmeans, data = data,
             palette = colors, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
))


fviz_nbclust(data, kmeans, method = 'silhouette')
}

#' Input: Whole processed data
kmean.clust(data.alt[-ncol(data.alt)])
#' Input: First two principal components
kmean.clust(data.pca$x[,1:2])
#' Input: First three principal components
kmean.clust(data.pca$x[,1:3])
#' Input: First four principal components
kmean.clust(data.pca$x[,1:4])
#' Input: First five principal components
kmean.clust(data.pca$x[,1:5])

#' <font size = "5"> **TASK3: CLASSIFICATION** </font>\
#' **------------------------------------------------------------------------**\
#' <font size = "3"> **a) KNN CLASSIFICATION*\ </font>\
#' **------------------------------------------------------------------------**\
#' 
#' Setting pseudo-random generation seed:
set.seed(1111)
#' Splitting data into train and test:
split <- sample.split(data.alt$Stat, SplitRatio = 0.8) 
data_train <- subset(data.alt, split == "TRUE") 
data_test <- subset(data.alt, split == "FALSE")
#' Find the optimal number of neighbors (with highest accuracy):
Acc <- c()
Sens <- c()
Spec <- c()
for (neighbor in 1:30) {
  knn_train <- knn(train = data_train[,-ncol(data_train)], test = data_test[,-ncol(data_test)], cl = data_train$Stat, k = neighbor)
  CM = confusionMatrix(table(data_test$Stat, knn_train))
  Acc <- c(Acc, CM$overall["Accuracy"])
  Sens <- c(Sens, CM$byClass["Sensitivity"])
  Spec <- c(Spec, CM$byClass["Specificity"])
}

a <- plot(1:30, Acc, type = "l", col = "red", main = "Accuracy", ylim = c(0.2,1),
          xlab = "Number of designated classes")
lines(1:30, Sens, col = "blue")
lines(1:30, Spec, col = "green")


legend(24, 0.3, legend = c("Accuracy", "Sensitivity", "Specificy"), col = c("red","blue", "green"),
       bg="white", lty=c(1,1), lwd=2, yjust=0)

opt.neighbor <- which.max(Acc)

#' Train and test model with train data (model 1):
knn_train <- knn(train = data_train[,-ncol(data_train)], test = data_train[,-ncol(data_train)], cl = data_train$Stat, k = opt.neighbor)
#' Model 1 results:
confusionMatrix(table(data_train$Stat, knn_train)) 
#' Train model with train data and test with test data (model 2):
knn_train <- knn(train = data_train[,-ncol(data_train)], test = data_test[,-ncol(data_test)], cl = data_train$Stat, k = opt.neighbor)
#' Model 2 results:
confusionMatrix(table(data_test$Stat, knn_train))


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **b) NAIVE-BAYES CLASSIFICATION*\ </font>\
#' **------------------------------------------------------------------------**\
#'
#'Change continuous variables to different percentiles:
data.alt2 <- data
for (i in 1:length(continuous.vars.r)) {
  data.alt2[,continuous.vars.r[i]] <- quantcut(data[,continuous.vars.r[i]])
}

#' Setting pseudo-random generation seed:
set.seed(1111)
#' Splitting data into train and test:
split <- sample.split(data.alt2$Stat, SplitRatio = 0.8) 
data_train <- subset(data.alt2, split == "TRUE") 
data_test <- subset(data.alt2, split == "FALSE")

#' Train Naive-Bayes Classifier:

model_NB <- naive_bayes(Stat ~ ., data = data_train, laplace = 0.01)

model_NB

#' Predict Train data:
NB_pred <- predict(model_NB, data_train, type = "class") 

confusionMatrix(table(data_train$Stat, NB_pred))
#' Predict Test data:
NB_pred <- predict(model_NB, newdata = data_test) 

confusionMatrix(table(data_test$Stat, NB_pred))

#' Conditional Probability plots:
par(mfrow = c(5,6), mar = c(3.5, 3, 2, 0.5), mgp = c(1.5, 0.2, 0), tck = -0.01,
    oma = c(0, 0, 1, 0))
for (name in names(model_NB$tables)) {
  plot(model_NB, which = name)
}



#' **------------------------------------------------------------------------**\
#' <font size = "3"> **c) LOGISTIC REGRESSION*\ </font>\
#' **------------------------------------------------------------------------**\
#'
#' Setting pseudo-random generation seed:
set.seed(1111)
#' Splitting data into train and test:
split <- sample.split(data.alt$Stat, SplitRatio = 0.8) 
data_train <- subset(data.alt, split == "TRUE") 
data_test <- subset(data.alt, split == "FALSE")

# Train Logistic Regression Model:
model_LGR <- glm(Stat ~ . , family = "binomial", data = data_train)

summary(model_LGR)

pred <- predict(model_LGR, newdata = data_test, type = "response")
y_pred <- ifelse(pred > 0.5, "Survived", "Deceased")

confusionMatrix(table(y_pred, data_test$Stat))

#' Selecting variables with P-value smaller than 0.05 to classify again:
lgr.selected <- names(data.alt)[coef(summary(model_LGR))[,4] < 0.05]

data.lgr.selected <- data.alt[, c(lgr.selected, "Stat")]



set.seed(1111)
split <- sample.split(data.alt$Stat, SplitRatio = 0.8) 
data_train <- subset(data.lgr.selected, split == "TRUE") 
data_test <- subset(data.lgr.selected, split == "FALSE")

model_LGR <- glm(Stat ~ . , family = "binomial", data = data_train)

pred <- predict(model_LGR, newdata = data_test, type = "response")
y_pred <- ifelse(pred > 0.6, "Survived", "Deceased")
table(y_pred, data_test$Stat)

sum(diag(table(y_pred, data_test$Stat)))/length(data_test$Stat)

confusionMatrix(table(data_test$Stat, y_pred))

plot(model_LGR, 1, ask = FALSE)
plot(model_LGR, 2, ask = FALSE)
plot(model_LGR, 3, ask = FALSE)
plot(model_LGR, 4, ask = FALSE)
plot(model_LGR, 5, ask = FALSE)
plot(model_LGR, 6, ask = FALSE)

#' **------------------------------------------------------------------------**\
#' <font size = "3"> **d) MULTINOMIAL LOGISTIC REGRESSION*\ </font>\
#' **------------------------------------------------------------------------**\
#'

set.seed(1111)
#' Splitting data into train and test:
split <- sample.split(data.alt$Stat, SplitRatio = 0.8) 
data_train <- subset(data.alt, split == "TRUE") 
data_test <- subset(data.alt, split == "FALSE")

model_mul <- multinom(Stat ~ . ,data = data_train)

summary(model_mul)$coefficients/summary(model_mul)$standard.errors
data.predicted <- predict(model_mul,data_test,type = "class")
table(data.predicted,data_test$Stat)
sum(diag(table(data.predicted,data_test$Stat))/length(data_test$Stat))

confusionMatrix(table(data.predicted,data_test$Stat))


#' **------------------------------------------------------------------------**\
#' <font size = "3"> **e) FEATURE SELECTION*\ </font>\
#' **------------------------------------------------------------------------**\
#'

#' No features are selected by sequential forward selection
data.alt.fs <- data.alt

data.alt.fs$Stat <- as.numeric(data.alt.fs$Stat)


evaluator <- filterEvaluator('MDLC')
searcher <- searchAlgorithm('sequentialForwardSelection')
results <- featureSelection(data.alt.fs, 'Stat', searcher, evaluator)
