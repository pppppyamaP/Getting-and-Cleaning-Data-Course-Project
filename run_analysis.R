# Load necessary packages. Install them if they are not already installed.
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("readr")) {
  install.packages("readr")
}
if (!require("stringr")) {
  install.packages("stringr")
}

library(dplyr)
library(readr)
library(stringr)

# 1. データをダウンロードし、読み込みます。
# このコードは、プロジェクトのデータが現在の作業ディレクトリに解凍されていることを前提としています。
# ----------------------------------------------------------------------------------------------------------------
# ファイルのパスを定義
data_path <- "./UCI HAR Dataset"

# Read the feature names and activity labels.
# features.txt and activity_labels.txt use a space as a delimiter.
features <- read_delim(file.path(data_path, "features.txt"), delim = " ", col_names = c("feature_id", "feature_name"), show_col_types = FALSE)
activities <- read_delim(file.path(data_path, "activity_labels.txt"), delim = " ", col_names = c("activity_id", "activity_name"), show_col_types = FALSE)

# Read the training data.
# X_train.txt is a space-separated file with many columns, so read_table is a good choice.
x_train <- read_table(file.path(data_path, "train", "X_train.txt"), col_names = FALSE)
# y_train.txt and subject_train.txt are also space-separated, but only have one column.
y_train <- read_delim(file.path(data_path, "train", "y_train.txt"), delim = " ", col_names = "activity_id", show_col_types = FALSE)
subject_train <- read_delim(file.path(data_path, "train", "subject_train.txt"), delim = " ", col_names = "subject_id", show_col_types = FALSE)

# Read the test data.
# X_test.txt is a space-separated file with many columns.
x_test <- read_table(file.path(data_path, "test", "X_test.txt"), col_names = FALSE)
# y_test.txt and subject_test.txt are also space-separated, but only have one column.
y_test <- read_delim(file.path(data_path, "test", "y_test.txt"), delim = " ", col_names = "activity_id", show_col_types = FALSE)
subject_test <- read_delim(file.path(data_path, "test", "subject_test.txt"), delim = " ", col_names = "subject_id", show_col_types = FALSE)

# 2. Merge the training and test sets to create one data set.
# ----------------------------------------------------------------------------------------------------------------
train_data <- bind_cols(subject_train, y_train, x_train)
test_data <- bind_cols(subject_test, y_test, x_test)
combined_data <- bind_rows(train_data, test_data)

# 3. Label the data set with descriptive variable names.
# ----------------------------------------------------------------------------------------------------------------
# Assign the feature names as column names
colnames(combined_data)[3:ncol(combined_data)] <- features$feature_name

# 4. Extract only the measurements on the mean and standard deviation.
# ----------------------------------------------------------------------------------------------------------------
# Identify columns with 'mean()' or 'std()' in their names
mean_std_cols <- grep("mean\\(\\)|std\\(\\)", features$feature_name, value = TRUE)
# Also keep subject_id and activity_id
selected_cols <- c("subject_id", "activity_id", mean_std_cols)
mean_std_data <- combined_data %>% 
  select(all_of(selected_cols))

# 5. Use descriptive activity names to name the activities in the data set.
# ----------------------------------------------------------------------------------------------------------------
# Convert activity IDs to descriptive names
mean_std_data$activity_id <- factor(mean_std_data$activity_id, levels = activities$activity_id, labels = activities$activity_name)

# 6. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# ----------------------------------------------------------------------------------------------------------------
tidy_data <- mean_std_data %>%
  group_by(subject_id, activity_id) %>%
  summarise(across(everything(), mean), .groups = 'drop')

# Write the final tidy data set to a file.
write_csv(tidy_data, "tidy_data.csv")

# Print the first few rows of the final data set to verify the result.
print(head(tidy_data))

