#install.packages("recommederlab")
#install.packages("reshape2")
#install.packages("ggplot2")
# If not installed, first install following three packages in R
library(recommenderlab)
library(ggplot2)
library(reshape2)

# Set data path as per your data file (for example: "c://abc//" )
setwd("/home/ratulr/Documents/data_mining/DataMining_Recommender_Mehregan/project_reco")


# Read training file along with header
data <- read.csv("dataset/ml-latest-small/ratings.csv", header = T)
head(data)

# Setting the seed for reproducible random data.
set.seed(12)

# Counting the total number of rows
n <- nrow(data)
n

# shuffling the dataset 
data <- data[sample(n),]
head(data)

# Remove 'timestamp' column as it is not necessary for the analysis.
data <- data[,-c(4)]
head(data)


# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
data_acast <- acast(data, userId ~ movieId)
head(data)

# Convert it as a matrix
data_mat <- as.matrix(data_acast)
head(data_mat)
class(data_mat)


# Convert R into realRatingMatrix data structure
# realRatingMatrix is a recommenderlab sparse-matrix like data-structure
data_rrm <- as(data_mat, "realRatingMatrix")
head(data_rrm)
class(data_rrm)

# View it as a data-frame
head(as(data_rrm, "data.frame"))
tail(as(data_rrm, "data.frame"))

# Normalizing the rating matrix
# Training set
data_norm <- normalize(data_rrm)
head(data_norm)
head(as(data_norm, "list"))



# Preparing the split of datasets 
eval_sets <- evaluationScheme(data = data_rrm, method = "split", train = 0.8, given=20, k = 1) 
eval_sets

# Saving the datasets
train_data <- getData(eval_sets, "train")
train_validation_data <- getData(eval_sets, "known")
test_data <- getData(eval_sets, "unknown")


# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
# Train is using the training set.
# Cosine Method
train_rec_ubcf_cosine <- Recommender(train_data, method="UBCF", param=list(normalize = "Z-score", method="Cosine",nn=5, minRating=1))
# Jaccard Method
train_rec_ubcf_jaccard <- Recommender(train_data, method="UBCF", param=list(normalize = "Z-score", method="Jaccard",nn=5, minRating=1))
# Pearson Method
train_rec_ubcf_pearson <- Recommender(train_data, method="UBCF", param=list(normalize = "Z-score", method="Pearson",nn=5, minRating=1))

# Depending upon your selection, examine what you got
print(train_rec_ubcf_cosine)
names(getModel(train_rec_ubcf_cosine))
getModel(train_rec_ubcf_cosine)$nn
head(train_rec_ubcf_cosine)
model_details <- getModel(train_rec_ubcf_cosine)
model_details$data

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(train_validation_data) shows there are 135 userId (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, train_real[1:nrow(train_real)], type="topNList", n=10)

# UBCF Cosine Predictions
eval_predictions_ubcf_cosine <- predict(train_rec_ubcf_cosine, train_validation_data, type="ratings")
# UBCF Jaccard Predictions
eval_predictions_ubcf_jaccard <- predict(train_rec_ubcf_jaccard, train_validation_data, type="ratings")
# UBCF Pearson Predictions
eval_predictions_ubcf_pearson <- predict(train_rec_ubcf_pearson, train_validation_data, type="ratings")


### Evaluating prediction accuracy.
# UBCF Cosine Predictions
eval_accuracy_ubcf_cosine <- calcPredictionAccuracy(eval_predictions_ubcf_cosine, test_data)
eval_accuracy_ubcf_cosine
# UBCF Jaccard Predictions
eval_accuracy_ubcf_jaccard <- calcPredictionAccuracy(eval_predictions_ubcf_jaccard, test_data)
eval_accuracy_ubcf_jaccard
# UBCF Pearson Predictions
eval_accuracy_ubcf_pearson <- calcPredictionAccuracy(eval_predictions_ubcf_pearson, test_data)
eval_accuracy_ubcf_pearson

