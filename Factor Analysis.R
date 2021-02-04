library(tidyverse) # metapackage of all tidyverse packages
library(scales) # axis formatting
library(egg) # ggarragne

library(readr)
library(ROCR)
library(PerformanceAnalytics)
library(e1071)
library(caret)
library(gbm)
library(corrplot)
library(ggcorrplot)
library(MASS)
library(rpart)
library(caTools)
library(naivebayes)
library(class)
library(ISLR)
library(glmnet)
library(Hmisc)
library(funModeling)
library(pROC)
library(randomForest)
library(klaR)
library(scales)
library(cluster)
library(factoextra)
library(DataExplorer)
library(ClustOfVar)
library(GGally)
library(reticulate)

options(warn=-1)


df <- read_csv("C:/Users/shitt/Desktop/Heart Failure Clinical Records/heart_failure_clinical_records_dataset (1).csv")

# Checking out the structure

head(df, n = 20)

str(df)

summary (df)

# descripive statistics of dataframe in R 



install.packages("pastecs")  

library(pastecs)
stat.desc(df)

# Summary statistics of dataframe in R 

install.packages("Hmisc")

library(Hmisc)

describe(df)




# perfoming pca analysis

df.pca <- prcomp(df[c(7:11)], center = TRUE, scale. = TRUE)

library(ggplot2)
library(ggbiplot)

ggbiplot(df.pca)

fviz_eig(df.pca)

fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(df.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(df.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


str(df.pca)
summary(df.pca)
install.packages("factoextra")
library(factoextra)

ggbiplot(df.pca, labels=names(df))

library(devtools)
install_github("vqv/ggbiplot")
library("grid")


library(plyr)
library(scales)

githubinstall("ggbiplot")

install.packages("grid")
library(ggbiplot)


library(ggfortify)

# Find missing data

apply(df, 2, function(col){
  sum(is.na(col))
})




