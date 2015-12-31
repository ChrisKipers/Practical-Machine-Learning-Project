library(dplyr)
library(tidyr)
library(caret)

# Get data for training and testing
dir.create("data")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "data/training.csv",
              method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "data/testing.csv",
              method = "curl")

training.data <- read.csv("data/training.csv")

# To pick the features to use in training, we will calculate the variance
# of column means grouped by classe
training.data.means.per.classe <- training.data %>%
  select(-starts_with("raw_timestamp"), -X) %>%
  group_by(classe) %>%
  summarize_each(funs(mean))

variance.of.column.averages <- as.data.frame(lapply(training.data.means.per.classe, FUN=var))

# Create a data frame with a field and variance column
field.and.variance <- gather(variance.of.column.averages, field, variance) %>%
  filter(!is.na(variance)) %>%
  arrange(desc(variance))

# Get the top 20 features based on variance between means
top.20.features <- field.and.variance[0:20, 1]

# Train the model
formula <- paste("classe ~", paste(top.20.features, collapse = " + "))
model <- train(as.formula(formula), data = training.data, method = "rf")

testing.data <- read.csv("data/testing.csv")

# Get predicted values
predicted.values <- predict(model, testing.data)

# Write predicted results to files for submission
dir.create("results")
for(i in 1:length(predicted.values)){
  filename = paste("results/problem_id_",i,".txt")
  write.table(predicted.values[i]
              ,file=filename,
              quote=FALSE,
              row.names=FALSE,
              col.names=FALSE)
}