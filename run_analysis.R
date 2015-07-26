# Week 3 Project:
# Getting and Cleaning Data
# run_analysis.R

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, 
#      creates a second, independent tidy data set 
#      with the average of each variable for each activity and each subject.

# read the activity label 
# by taking the second column of activity_labels.txt file
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# read the features 
# by takeing the second column of features.txt file
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

#############
# TEST DATA #
#############
# Load test data
# Load and process X_test & y_test data.
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Change the header of test data
# Add 2nd column by referring to activity_labels using 1st column as index
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("ACTIVITY_ID", "ACTIVITY_LABEL")
names(X_test) = features
names(subject_test) = "SUBJECT"


#################
# TRAINING DATA #
#################
# Load train data
# Load and process X_train & y_train data.
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Change the header of train data
# Add 2nd column by referring to activity_labels using 1st column as index
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("ACTIVITY_ID", "ACTIVITY_LABEL")
names(X_train) = features
names(subject_train) = "SUBJECT"

# Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_features <- grepl("mean|std", features)
X_test = X_test[,mean_std_features]
X_train = X_train[,mean_std_features]

# Combine test data (X_test and y_test)
test_data <- cbind(data.table::as.data.table(subject_test), y_test, X_test)

# Combine train data (X_train and y_train)
train_data <- cbind(data.table::as.data.table(subject_train), y_train, X_train)

# Merge test and train data
merge_data <- rbind(test_data, train_data)

# Creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
tidy_labels <- c("SUBJECT", "ACTIVITY_ID", "ACTIVITY_LABEL")
merge_data_labels <- setdiff(colnames(merge_data), tidy_labels)
melt_merge_data <- reshape2::melt(merge_data, id = tidy_labels, measure.vars = merge_data_labels)
tidy_data <- reshape2::dcast(melt_merge_data, SUBJECT + ACTIVITY_LABEL ~ variable, mean)

# Write the tidy data to file
write.table(tidy_data, file = "./tidy_data.txt", row.names = FALSE)
