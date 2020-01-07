##### Sampling data #####
##### Read in the data #####
loans <- read.csv('~/Documents/Code/R_Programming/Data_Science/Classification_Trees/loans.csv')

# Create a random sample of row IDs
sample_rows <- sample(nrow(loans), size = 8484)

# Create the training dataset
loans_train <- loans[sample_rows,]

# Create the test dataset
loans_test <- loans[-sample_rows,]