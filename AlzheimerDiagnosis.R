# set the working directory
setwd("C:/Users/HP/OneDrive/Desktop/UNI_SUBJ/Modeling MA335/final proj")

# import required libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)
library(colorRamps)
library(factoextra)
library(cluster)
library(tidymodels)
library(caret)
library(randomForest)
library(knitr)
library(modelsummary)

# read the csv data into data frame
data_1 <- read.csv("project data.csv")

kable(data_1[1:3, ], caption = "Project data") %>%
  kableExtra::kable_styling(bootstrap_options = "bordered")

# Data cleaning

proj_data = filter(data_1, !data_1$Group=="Converted")

proj_data$Group <- factor(proj_data$Group,
                          levels = c("Demented", "Nondemented"),
                          labels = c(2, 1))

proj_data$M.F <- factor(proj_data$M.F,
                        levels = c("M","F"),
                        labels = c(1,0))

# Replacing missing values
proj_data$SES[is.na(proj_data$SES)] <- median(proj_data$SES, na.rm = TRUE)
proj_data$MMSE[is.na(proj_data$MMSE)] <- median(proj_data$MMSE, na.rm = TRUE)

# Standardizing the values
proj_data$eTIV <- (proj_data$eTIV-mean(proj_data$eTIV))/ sd(proj_data$eTIV)
proj_data$nWBV <- (proj_data$nWBV-mean(proj_data$nWBV))/ sd(proj_data$nWBV)
proj_data$ASF <- (proj_data$ASF-mean(proj_data$ASF))/ sd(proj_data$ASF)

kable(proj_data[1:3, ], caption = "Pre-processed data") %>%
  kableExtra::kable_styling(bootstrap_options = "bordered")

# Descriptive Analysis

# Count of patients grouped by SES on Gender
proj_data %>% select(Age, M.F, SES) %>%
  group_by(SES, M.F) %>% count() %>%
  ggplot(aes(x = M.F, y = n, fill = as.factor(SES))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels= c("Male", "Female")) +  
  scale_fill_manual(values = colorRamps::green2red(5)) +
  coord_flip() +
  theme_classic() +
  labs(title = "SES Based on Gender", x = "Gender", y ="Count", fill = "SES")

# Plot for correlation
pairs.panels(
  proj_data[, -c(1,2,7)],
  gap=0,
  lm=TRUE,
  stars = TRUE,
  hist.col = 4
)

#Hierarchical clustering

# Vector containing linkage methods to choose from
linkage_methods <- c("average","single","complete","ward")
names(linkage_methods) <- c("average","single","complete","ward")

# Function to calculate agglomerative clusters
ag_coefficient <- function(x){
  agnes(proj_data, method = x)$ac
}

# Print the cluster values for each method
kable(sapply(linkage_methods, ag_coefficient)) %>%
  kableExtra::kable_styling(bootstrap_options = "bordered")

# Perform agglomerative clustering using the method with the highest score
cluster <- agnes(proj_data, method = "ward")

# Visualize the dendogram for the clustering performed
pltree(cluster, cex = 0.6, hang = -1, main = "Dendrogram")


# Calculate the gap value to find the optimal number of clusters to use
gap_statistic <- clusGap(data.matrix(proj_data), FUNcluster = hcut, nstart = 10, K.max = 4)

# visualize the optimal number of clusters
fviz_gap_stat(gap_statistic)


# Create the distance matrix for each data point using the euclidean method
dist_matrix <- dist(scale(data.matrix(proj_data)), method = "euclidean")

# Perform hierarchical clustering
hclust_model <- eclust(dist_matrix, 
                       "hclust", 
                       k = 2, 
                       stand = T, 
                       hc_method = "ward.D2")

# Segment the hierarchical clustering tree to the required number of clusters
group_final <- cutree(hclust_model, k=2)

# Print the cluster output
kable(table(group_final, proj_data$Group), caption = "Clustering groups") %>%
  kableExtra::kable_styling(bootstrap_options = "bordered")

# Visualize the clusters
fviz_cluster(
  object = hclust_model,
  geom = "point",
  ellipse.type = "norm",
  show.clust.cent = T, 
  repel = T,
  main = "Clusters of Group (Demented, Non-Demented)",
  ggtheme = theme_classic()
)


# Logistic regression

# Split into train and test data
split_data <- initial_split(proj_data, prop = 0.8, strata = Group)

train_data <- split_data %>% training()
test_data <- split_data %>% testing()

# Training logistic regression model
model_training <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Group ~ ., data = train_data)

kable(tidy(model_training)) %>%
  kableExtra::kable_styling(bootstrap_options = "bordered")

# Predicting the class on test data
pred_class <- predict(model_training, 
                      new_data = test_data,
                      type = "class")

# Combine the actual and predicted response variable (Group)
results <- test_data %>% select(Group) %>% bind_cols(pred_class)

# Plot the confusion matrix for the test data
autoplot(conf_mat(results, truth = Group, estimate = .pred_class), type = "heatmap")

# Feature selection

# Vector containing the number of features wanted
subsets <- c(3,4,5)

# Function specifying the cross validation options to be used for feature selection 
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 10,
                   verbose = F)

# Calculate the significance and return the top features
lmdata <- rfe(x=proj_data[2:10], y=proj_data$Group,
              sizes = subsets,
              rfeControl = ctrl)

# Print the selected top features
lmdata

