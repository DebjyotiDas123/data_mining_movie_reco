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
tr <- read.csv("dataset/ml-latest-small/ratings.csv", header = T)
head(tr)

# Setting the seed for reproducible random data.
set.seed(12)

# Counting the total number of rows
n <- nrow(tr)
n

# shuffling the dataset 
tr <- tr[sample(n),]
head(tr)

# Remove 'timestamp' column as it is not necessary for the analysis.
tr <- tr[,-c(4)]
head(tr)


# Splitting the data into train and test
train <- tr[1:round(0.8*n),]
test  <- tr[(round(0.8*n)+1):n,]

str(train)
str(test)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
# Training set
train_acast <- acast(train, userId ~ movieId)
head(train_acast)
# Test set
test_acast <- acast(test, userId ~ movieId)
head(test_acast)


# Check the class of train_acast
class(train_acast)
class(test_acast)

# Convert it as a matrix
# Training set
train_R <- as.matrix(train_acast)
head(train_R)
class(train_R)
# Test set
test_R <- as.matrix(test_acast)
head(test_R)
class(test_R)




# Convert R into realRatingMatrix data structure
# realRatingMatrix is a recommenderlab sparse-matrix like data-structure
# Training set
train_real <- as(train_R, "realRatingMatrix")
head(train_real)
class(train_real)
# Test set
test_real <- as(train_R, "realRatingMatrix")
head(test_real)
class(test_real)

# View it as a data-frame
# Training set
head(as(train_real, "data.frame"))
tail(as(train_real, "data.frame"))
# Test set
head(as(test_real, "data.frame"))
tail(as(test_real, "data.frame"))


# Normalizing the rating matrix
# Training set
train_real_norm <- normalize(train_real)
head(train_real_norm)
head(as(train_real_norm, "list"))
# Test set
test_real_norm <- normalize(test_real)
head(test_real_norm )
head(as(test_real_norm , "list"))


# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(train_real, main = "Raw Ratings")
image(train_real_norm, main = "Normalized Ratings")

# Can also turn the matrix into a 0-1 binary matrix
train_bin <- binarize(train_real, minRating=1)
train_bin_mat <-as(train_bin, "matrix")
train_bin_mat[1:10,1:10]


# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
# Train is using the training set.
train_rec_ubcf_cosine <- Recommender(train_real[1:nrow(train_real)], method="UBCF", param=list(normalize = "Z-score", method="Cosine",nn=5, minRating=1))
#OR
#train_rec_ubcf_cosine2 <- Recommender(train_real, method="UBCF", param=list(normalize = "Z-score", method="Cosine",nn=5, minRating=1))

#train_rec_ubcf_jaccard <- Recommender(r[1:nrow(r)], method="UBCF", param=list(normalize = "Z-score", method="Jaccard",nn=5, minRating=1))
#train_rec_ibcf_jaccard <- Recommender(r[1:nrow(r)], method="IBCF", param=list(normalize = "Z-score", method="Jaccard",minRating=1))
#train_rec_popular <- Recommender(r[1:nrow(r)], method="POPULAR")

# Depending upon your selection, examine what you got
print(train_rec_ubcf_cosine)
names(getModel(train_rec_ubcf_cosine))
getModel(train_rec_ubcf_cosine)$nn
head(train_rec_ubcf_cosine)


############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(test_real) shows there are 671 userId (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, train_real[1:nrow(train_real)], type="topNList", n=10)

train_predicted_recom <- predict(train_rec_ubcf_cosine, train_real[1:nrow(train_real)], type="ratings")
#train_predicted_recom2 <- predict(train_rec_ubcf_cosine, train_real, type="ratings")

# Predicting on the test set using the model we created with the training set.
test_predicted_recom <- predict(train_rec_ubcf_cosine, test_real, type="ratings")


########## Examination of model & experimentation  #############
########## This section can be skipped #########################
# Convert prediction into list, user-wise
as(train_predicted_recom, "list")
# Study and Compare the following:
as(train_real, "matrix")     # Has lots of NAs. 'train_real' is the original matrix
as(train_predicted_recom, "matrix") # Is full of ratings. NAs disappear
as(train_predicted_recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(train_predicted_recom, "matrix")[0:5,0:3]   # Rating for user 5 for item at index 3
as.integer(as(train_predicted_recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(train_predicted_recom, "matrix")[671,])) # Just get the correct integer value
as.integer(round(as(train_predicted_recom, "matrix")[368,3717]))


# Convert all your recommendations to list structure
train_rec_list <-as(train_predicted_recom,"list")
head(summary(train_rec_list))
# Access this list. User 2, item at index 2
train_rec_list[[2]][2]
# Convert to data frame all recommendations for user 1
u1 <-as.data.frame(train_rec_list[[1]])
attributes(u1)
class(u1)
# Create a column by name of id in data frame u1 and populate it with row names
u1$id<-row.names(u1)
# Check movie ratings are in column 1 of u1
u1
# Now access movie ratings in column 1 for u1
u1[u1$id==3952,1]






########## Create submission File from model #######################
# Read test file
test<-read.csv("test_v2.csv",header=TRUE)
head(test)
# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,2]
  movieid<-test[u,3]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')
# Submit now this csv file to kaggle
########################################


