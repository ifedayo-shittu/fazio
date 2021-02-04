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


library(tidyverse)
library(skimr)      # skimming data frames
library(ggthemes)
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)

library(rsample)    # initial_split()
library(DescTools)  # PseudoR2()
library(sjPlot)     # tab_model(), print regression models as HTML table
library(caret)      # confusionMatrix()
library(mlr)        # Machine Learning in R (for SVM)
library(rpart)      # Recursive Partitioning and Regression Trees
library(rpart.plot)
library(ranger)     # Random forest
library(lightgbm)   # LightGBM (GBDT: gradient boosting decision


install.packages("factoextra")
library(factoextra)



library(devtools)
install_github("vqv/ggbiplot")
library("grid")


palette_ro = c("#ee2f35", "#fa7211", "#fbd600", "#75c731", "#1fb86e", "#0488cf", "#7b44ab")

df <- read_csv("C:/Users/shitt/Desktop/Heart Failure Clinical Records/heart_failure_clinical_records_dataset (1).csv")

# Checking out the structure

head(df, n = 20)

str(df)

summary (df)



# descripive statistics of dataframe in R 

skim(df)

install.packages("pastecs")  

library(pastecs)
stat.desc(df)

# Summary statistics of dataframe in R 

install.packages("Hmisc")

library(Hmisc)

describe(df)



# Simple exploratory techniques

library(ggplot2)
library(ggbiplot)
library(ggbiplot)





f_features = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")

df_n <- df
df <- df %>%
  mutate_at(f_features, as.factor)

p1 <- ggplot(df, aes(x = anaemia, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)"))+
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p2 <- ggplot(df, aes(x = diabetes, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p3 <- ggplot(df, aes(x = high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p4 <- ggplot(df, aes(x = sex, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)")) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p5 <- ggplot(df, aes(x = smoking, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p6 <- ggplot(df, aes(x = DEATH_EVENT, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

((p1 + p2 + p3) / (p4 + p5 + p6)) +
  plot_annotation(title = "Distribution of the binary features and DEATH_EVENT")

#EXploratorary analysis of  numeric varaibles

p1 <- ggplot(df, aes(x = age)) + 
  geom_histogram(binwidth = 5, colour = "white", fill = palette_ro[3], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 5))), colour = palette_ro[3], fill = palette_ro[3], alpha = 0.25) +
  # 5 is binwidth of geom_histogram()
  # binwidth can be calculated from "diff(range(df$age))/20"
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  geom_vline(xintercept = median(df$age), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$age)-5, y = 50,
           label = str_c("Min.     : ", min(df$age),
                         "\nMedian : ", median(df$age),
                         "\nMean    : ", round(mean(df$age), 1),
                         "\nMax.    : ", max(df$age))) +
  labs(title = "age distribution") +
  theme_minimal(base_size = 12)


p2 <- ggplot(df, aes(x = age, fill = DEATH_EVENT)) + 
  # geom_histogram(aes(y=..density..), binwidth = 5, colour = "white", position = "identity", alpha = 0.5) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$age), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$age), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$age)-10, y = 0.03,
           label = str_c("Survived median: ", median(filter(df, DEATH_EVENT == 0)$age),
                         "\nDead median: ", median(filter(df, DEATH_EVENT == 1)$age))) +
  
  labs(title = "Relationship between age and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")
p1 / p2

#another Exploratory analysis

p1 <- ggplot(df, aes(x = creatinine_phosphokinase)) + 
  geom_histogram(binwidth = 100, colour = "white", fill = palette_ro[3], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 100))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-1000, y = 75,
           label = str_c("Min.     : ", min(df$creatinine_phosphokinase),
                         "\nMedian : ", median(df$creatinine_phosphokinase),
                         "\nMean    : ", round(mean(df$creatinine_phosphokinase), 1),
                         "\nMax.    : ", max(df$creatinine_phosphokinase))) +
  labs(title = "creatinine_phosphokinase distribution") +
  theme_minimal(base_size = 12)

p2 <- ggplot(df, aes(x = creatinine_phosphokinase, fill = DEATH_EVENT)) +
  # geom_histogram(binwidth = 100, colour = "white", position = "identity", alpha = 0.5) +
  # geom_density(eval(bquote(aes(y = ..count.. * 100))), alpha = 0.25) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-1400, y = 0.0015,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase))) +
  
  labs(title = "Relationship between creatinine_phosphokinase and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p3 <- ggplot(df, aes(x = creatinine_phosphokinase, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[5], palette_ro[3]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-4500, y = 0.7,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase))) +
  
  labs(title = "Relationship between creatinine_phosphokinase and DEATH_EVENT (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_log10() +
  annotation_logticks()

p1 / p2 / p3

#correllation maytrix

cor(df_n) %>%
  corrplot(method = "color", type = "lower", tl.col = "blue", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(df_n)$p,
           sig.level = 0.05)

#reference

cor(df_n) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           p.mat = cor.mtest(df_n)$p,
           insig = "p-value", sig.level = -1)

# perfoming pca analysis


df.pca <- prcomp(df[c(7:13)], center = TRUE, scale. = TRUE)





fviz_eig(df.pca)

fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
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


