setwd("C:/Users/K-Dawg/Documents/Git/PML_Project")
if(!exists("RESET")) RESET = F

library("caret")
library("doParallel")

# Set up variables.
url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
set.seed(666)

# Download the data files, if they don't exist in the working directory.
if(!exists("training.data", inherits=F)) {
   message("Downloading and reading training csv file...")
   training.data = read.csv(url(url, "r"))
}

if(!exists("test.data", inherits=F)) {
   message("Downloading and reading test csv file...")
   test.data = read.csv(url(testurl, "r"))
}

####################
## Pre-processing ##
####################

# Some of the test.data columns are empty.
# Create a list of the empty columns.
if(RESET) {
keepers = rep(NA, ncol(test.data))
for(i in 1:ncol(test.data)) {
   keepers[i] = sum(complete.cases(test.data[,i])) == nrow(test.data)
}

# Remove columns from which the test set has no data for.
test.data = test.data[, keepers]
training.data = train = training.data[, keepers]

# Drop some seemingly unnecessary columns.
drops = c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
train = training.data[, !names(training.data) %in% drops]
test.data = test.data[, !names(test.data) %in% drops]

# PREDICTION TIME!!! 
# This is very time-consuming, so don't execute if I haven't flagged RSTUDIO to reset.

   RESET = F
   
   # Create partitions.
   intrain = createDataPartition(train$classe, p=0.6, list=F)
   
   # Split training data into training and testing datasets.
   test  = train[-intrain,]
   train = train[intrain,]
   
   # Set up parallel processors.
   ignorecores = 0
   cl<-makeCluster(detectCores() - ignorecores)
   registerDoParallel(cl)
   
   # Create model fit on training data.
   message("Training model...")
   begin = Sys.time()
   modelfit = train(classe ~ ., data=train, method="rf")
   end = Sys.time()
   
   message(sprintf("Time to fit model, with %d cores: %d minutes %d seconds.", 
                detectCores() - ignorecores, 
                floor(as.numeric(end-begin, units="mins")), 
                floor(as.numeric(end-begin, units="secs")) %% 60))
}

# Create prediction for training test data.
prediction = predict(modelfit, test)

# Print results.
print(table(prediction, actual=test[complete.cases(test), "classe"]))

# Find estimated error rate.
confusionMatrix(test$classe, prediction) 

# Create prediction for test data.
testprediction = predict(modelfit, test.data)

# Write predictions to files.
for(i in 1:length(testprediction)){
   filename = paste0("problem_id_",i,".txt")
   write.table(testprediction[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE, append=F)
}
