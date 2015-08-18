library(e1071)
library(randomForest)
library(class)

#data encoding functions
process_mention_sentiment <- function(mention_sentiment)
{
  #one-hot encoding
  ms <- matrix(0, length(mention_sentiment), 3)
  colnames(ms) <- c("negative", "neutral", "positive")
  for(i in 1:length(mention_sentiment))
    if(mention_sentiment[i] > -2)
      ms[i, mention_sentiment[i] + 2] <- 1
  
  ms
}

get_source_url_list <- function(source_url, negl_sources)
{
  sources <- as.data.frame(table(source_url))
  
  #group factors with less than @negl_sources entries in "other_source" factor
  c(as.vector(sources[sources[, 2] > negl_sources, 1]), "other_source")
}

process_source_url <- function(source_url, source_url_list)
{
  #one-hot encoding
  su <- matrix(0, length(source_url), length(source_url_list))
  colnames(su) <- source_url_list
  for(i in 1:length(source_url))
    su[i, match(source_url[i], source_url_list, length(source_url_list))] <- 1
  
  su
}

process_author_type <- function(author_type)
{
  #one-hot encoding
  at <- matrix(0, length(author_type), 3)
  colnames(at) <- c("person", "siteadmin", "community")
  for(i in 1:length(author_type))
    at[i, log2(author_type[i])] <- 1
  
  at
}

process_author_mention_image <- function(author_image, mention_image)
{
  #coercing
  author_image <- as.vector(author_image)
  mention_image <- as.vector(mention_image)
  
  #one-hot encoding ("1" corresponds to existence of an URL, "0" stays for absence)
  author_image[!is.na(author_image)] <- 1
  author_image[is.na(author_image)] <- 0
  
  mention_image[!is.na(mention_image)] <- 1
  mention_image[is.na(mention_image)] <- 0
  
  data.frame(author_image=as.numeric(author_image), mention_image=as.numeric(mention_image))
}

process_author_readers <- function(author_readers)
{
  #prevents negative values in logarithm
  author_readers[author_readers < 1] <- 1
  
  log(author_readers)
}

process_date_time <- function(date_time)
{
  as.numeric(strptime(date_time, "%Y-%m-%dT%H:%M:%S"))
}

process_text <- function(mention_title, mention_text, dictionary=numeric())
{
  keywords <- c("http", "goo", "insta")
  
  #number of characters in title and text
  mtt <- nchar(cbind(as.vector(mention_title), as.vector(mention_text)))
  
  #indicator of keyword inclusion
  text <- paste(as.vector(mention_title), as.vector(mention_text))
  mtt <- cbind(mtt, 0)
  mtt[grepl(paste(keywords, collapse="|"), text), ncol(mtt)] <- 1
  
  #text cleansing
  text <- strsplit(text, " ")
  text <- lapply(text, gsub, pattern="[^0-9A-Za-zА-Яа-я]", replacement="")
  text <- lapply(text, tolower)
  
  #create dictionary if not passed through argument @dictionary
  if(length(dictionary) == 0)
    dictionary <- unique(unlist(text))
  
  #count spam words
  mtt <- cbind(mtt, unlist(lapply(lapply(text, "%in%", dictionary), sum)))
  colnames(mtt) <- c("title_length", "text_length", "keywords", "spam_words")
  
  #return both encoded data and dictionary
  list(data=mtt, dictionary=dictionary)
}

process_data <- function(raw_data, supp_data=list(source_url_list=numeric(), dictionary=numeric()))
{
  #create source ULR list and spam dictionary for train set
  negl_sources <- 50
  if(length(supp_data$source_url_list) == 0)
    supp_data$source_url_list <- get_source_url_list(raw_data$source_url, negl_sources)
  
  processed_text <- process_text(raw_data$mention_title, raw_data$mention_text, supp_data$dictionary)
  if(length(supp_data$dictionary) == 0)
    supp_data$dictionary <- processed_text$dictionary
  
  #create feature matrix
  data <- data.frame(spam=raw_data$author_excluded)
  data <- cbind(data, processed_text$data)
  data <- cbind(data, process_mention_sentiment(raw_data$mention_sentiment))
  data <- cbind(data, deleted=raw_data$mention_deleted)
  data <- cbind(data, process_source_url(raw_data$source_url, supp_data$source_url_list))
  data <- cbind(data, process_author_type(raw_data$author_type))
  data <- cbind(data, process_author_mention_image(raw_data$author_image, raw_data$mention_image))
  data <- cbind(data, log_author_readers=process_author_readers(raw_data$author_readers))
  data <- cbind(data, creation_date=process_date_time(raw_data$mention_creationdate))
  data <- cbind(data, modification_date=process_date_time(raw_data$author_modificationdate))

  #return feature matrix and supplementary data
  list(data=data, supp_data=supp_data)
}

separate_train_test_data <- function(data, validation_frac, test_frac)
{
  idx <- sample(1:3, nrow(data), T, c(1 - validation_frac - test_frac, validation_frac, test_frac))
  list(train=data[idx==1, ], validation=data[idx==2, ], test=data[idx==3, ])
}

#read raw data from a file
raw_data <- read.csv("data/maggi.csv", encoding = "UTF-8")
#raw_data <- rbind(raw_data, read.csv("data/raffaello.csv", encoding = "UTF-8"))
#raw_data <- rbind(raw_data, read.csv("data/nutrilon.csv", encoding = "UTF-8"))

#separate data for train (80%) and test (20%) sets 
data <- separate_train_test_data(raw_data, 0, 0.2)

#process data
train <- process_data(data$train)
test <- process_data(data$test, train$supp_data)$data
train <- train$data

#tuning parameters
#Optimal: k = 1
tune_knn <- function(train)
{
  k_range <- 1:15
  max_iter <- 100
  
  f_score <- array(0, c(length(k_range), max_iter))
  for(iter in 1:max_iter)
  {
    data <- separate_train_test_data(train, 0.25, 0)
    
    for(i in 1:length(k_range))
    {
      pred <- knn(data$train[, -1], data$validation[, -1], data$train[, 1], k=k_range[i])
      tab <- table(true=data$validation[, 1], pred=pred)
      f_score[i, iter] <- 2 * tab[2, 2] / (2 * tab[2, 2] + tab[1, 2] + tab[2, 1])
    }
  }
    
  apply(f_score, 1, mean)   
}

#Optimal: kernel = "radial", cost = 2048; gamma = 0.00390625
tune_svm <- function(train)
{
  cost_range <- 2^(11)
  gamma_range <- 2^(-8)
  max_iter <- 50
  
  f_score <- array(0, c(length(cost_range), length(gamma_range), max_iter))
  for(iter in 1:max_iter)
  {
    data <- separate_train_test_data(train, 0.25, 0)
    
    for(i in 1:length(cost_range))
      for(j in 1:length(gamma_range))
      {
        model <- svm(x=data$train[, -1], y=data$train[, 1], type="C", kernel = "radial", cost=cost_range[i], gamma=gamma_range[j])
        pred <- predict(model, data$validation[, -1])
        tab <- table(true=data$validation$spam, pred=pred)
        f_score[i, j, iter] <- 2 * tab[2, 2] / (2 * tab[2, 2] + tab[1, 2] + tab[2, 1])
      }
  }
  
  apply(f_score, 1:2, mean) 
}

#Optimal: mtry = 8
tune_rf <- function(train)
{
  mtry_range <- 1:10
  max_iter <- 20
  
  f_score <- array(0, c(length(mtry_range), max_iter))
  for(iter in 1:max_iter)
  {
    data <- separate_train_test_data(train, 0.25, 0)
    
    for(i in 1:length(mtry_range))
    {
      model <- randomForest(x=data$train[, -1], y=as.factor(data$train[, 1]), mtry=mtry_range[i])
      pred <- predict(model, data$validation[, -1])
      tab <- table(true=data$validation$spam, pred=pred)
      f_score[i, iter] <- 2 * tab[2, 2] / (2 * tab[2, 2] + tab[1, 2] + tab[2, 1])
    }
  }
  
  apply(f_score, 1, mean)
}

#test algorithms
#k-nearest neighbour
pred <- knn(train[, -1], test[, -1], train[, 1], k=1)
tab <- table(true=test$spam, pred=pred)
print(tab)

#SVM
model_svm <- svm(x=train[, -1], y=train[, 1], type="C", cost=2048, gamma=0.00390625, probability=T)
pred <- predict(model_svm, test[, -1], probability=T)
tab <- table(true=test$spam, pred=pred)
print(tab)

#random forest
model_frst <- randomForest(x=train[, -1], y=as.factor(train[, 1]), ntree=500, nodesize = 1, mtry=8)
pred <- predict(model_frst, test[, -1])
tab <- table(true=test$spam, pred=pred)
print(tab)




