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
tr <- read.csv("dataset/predict_movie_ratings_kaggle/train_v2.csv", header = T)
head(tr)

# Remove 'id' column. We do not need it
tr2 <- tr[,-c(1)]
head(tr2)

# Check, if removed
tr2[tr2$user==1,]

g_melt <- melt(tr2, id=c("user", "movie"), na.rm=T)
head(g_melt)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g <- acast(tr2, user ~ movie)

# Check the class of g
class(g)

# Redundant if class of g is already a matrix, not sure why this is used - ratul notes
# Convert it as a matrix
R <- as.matrix(g)
head(R)

# Convert R into realRatingMatrix data structure
# realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
head(r)

# view r in other possible ways
r_list <- as(r, "list")     # A list
r_matrix <- as(r, "matrix")   # A sparse matrix

# I can turn it into data-frame
head(as(r, "data.frame"))


# normalize the rating matrix
r_m <- normalize(r)
head(r_m)
head(as(r_m, "list"))

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")
bin_mat <-as(r_b, "matrix")
bin_mat[1:5,1:5]

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec_ubcf_cosine <- Recommender(r[1:nrow(r)], method="UBCF", param=list(normalize = "Z-score", method="Cosine",nn=5, minRating=1))
#rec_ubcf_jaccard <- Recommender(r[1:nrow(r)], method="UBCF", param=list(normalize = "Z-score", method="Jaccard",nn=5, minRating=1))
#rec_ibcf_jaccard <- Recommender(r[1:nrow(r)], method="IBCF", param=list(normalize = "Z-score", method="Jaccard",minRating=1))
#rec_popular <- Recommender(r[1:nrow(r)], method="POPULAR")

# Depending upon your selection, examine what you got
print(rec_ubcf_cosine)
names(getModel(rec_ubcf_cosine))
getModel(rec_ubcf_cosine)$nn


############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec_ubcf_cosine, r[1:nrow(r)], type="ratings")
recom


########## Examination of model & experimentation  #############
########## This section can be skipped #########################

# Convert prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(r, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(recom, "matrix")[6039,8])) # Just get the correct integer value
as.integer(round(as(recom, "matrix")[368,3717])) 

# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))
# Access this list. User 2, item at index 2
rec_list[[2]][2]
# Convert to data frame all recommendations for user 1
u1<-as.data.frame(rec_list[[1]])
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

