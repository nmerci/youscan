source("R/data_processing.R")

process_text <- function(mention_title, mention_text, spam)
{
  #process text
  text <- paste(as.vector(mention_title), as.vector(mention_text))
  text <- strsplit(text, " ")
  text <- lapply(text, gsub, pattern="[^0-9A-Za-zА-Яа-я]", replacement="") # HERE IS A BUG
  text <- lapply(text, tolower)
  
  #create dictionary
  dict <- sort(unique(unlist(text[as.vector(spam) == 1])))
  dict <- dict[nchar(dict) < 15]
  
  #create feature matrix
  m <- matrix(0, length(text), length(dict))
  colnames(m) <- paste(dict, "_d")
  
  for(i in 1:length(text))
    m[i, which(dict %in% text[[i]])] <- 1
  
  m
}

separate_train_test_data <- function(data, test_fraction)
{
  n <- nrow(data)
  idx <- rep(T, n)
  idx[sample(1:n, round(n * test_fraction), F)] <- F
  
  list(train=data[idx, ], test=data[!idx, ])
}



############################################################################
library(e1071)
library(randomForest)
library(class)


raw_data <- read.csv("data/maggi.csv", encoding = "UTF-8")
raw_data <- rbind(raw_data, read.csv("data/raffaello.csv", encoding = "UTF-8"))
raw_data <- rbind(raw_data, read.csv("data/nutrilon.csv", encoding = "UTF-8"))

#reduce text feature dimension
text_pca <- prcomp(process_text(raw_data$mention_title, raw_data$mention_text, raw_data$author_excluded))

#process data
data <- process_data(raw_data)


data <- cbind(data, text_pca$x[, 1:10])

#separate data
s_data <- separate_train_test_data(data, 0.3)

#k-nearest neighbour
prediction <- knn(s_data$train[, -1], s_data$test[, -1], s_data$train[, 1], k=1)
tab <- table(true=s_data$test$spam, pred=prediction)
tab

#SVM
model_svm <- svm(x=s_data$train[, -1], y=s_data$train[, 1], type="C", cost=100, gamma=0.1)
prediction <- predict(model_svm, s_data$test[, -1])
tab <- table(true=s_data$test$spam, pred=prediction)
tab

#random forest
model_frst <- randomForest(x=s_data$train[, -1], y=as.factor(s_data$train[, 1]))
prediction <- predict(model_frst, s_data$test[, -1])
tab <- table(true=s_data$test$spam, pred=prediction)
tab
 


#naive Bayes
model_nb <- naiveBayes(x=s_data$train[, -1], y=as.factor(1 - s_data$train[, 1]))
prediction <- predict(model_nb, s_data$test[, -1])
tab <- table(true=s_data$test$spam, pred=prediction)
tab




