# YouScan Task

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

process_source_url <- function(source_url)
{
  #group factors with less than @negl_sources entries
  #in "other" factor
  negl_sources <- 100
  
  sources <- as.data.frame(table(source_url))
  sources <- c(as.vector(sources[sources[, 2] > negl_sources, 1]), "other")
  
  #one-hot encoding
  su <- matrix(0, length(source_url), length(sources))
  colnames(su) <- sources
  for(i in 1:length(source_url))
    su[i, match(source_url[i], sources, length(sources))] <- 1
  
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
  
  #one-hot encoding ("1" corresponds to existence of 
  #an URL, "0" stays for absence)
  author_image[!is.na(author_image)] <- 1
  author_image[is.na(author_image)] <- 0
  
  mention_image[!is.na(mention_image)] <- 1
  mention_image[is.na(mention_image)] <- 0
  
  data.frame(author_image=as.numeric(author_image), 
             mention_image=as.numeric(mention_image))
}

process_author_readers <- function(author_readers)
{
  #prevents negative values in logarithm
  author_readers[author_readers < 1] <- 1
  
  log(author_readers)
}

process_date_time <- function(date_time)
{
  date_time <- strptime(date_time, "%Y-%m-%dT%H:%M:%S")
}

process_text_length <- function(mention_title, mention_text)
{
  mtt <- nchar(cbind(as.vector(mention_title), as.vector(mention_text)))
  colnames(mtt) <- c("title_length", "text_length")
  
  mtt
}

process_text <- function(mention_title, mention_text, spam)
{
  #process text
  text <- paste(as.vector(mention_title), as.vector(mention_text))
  text <- strsplit(text, " ")
  text <- lapply(text, gsub, pattern="[^0-9A-Za-zА-Яа-я]", replacement="")
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





process_data <- function(raw_data)
{
  data <- data.frame(spam=raw_data$author_excluded)
  data <- cbind(data, process_mention_sentiment(raw_data$mention_sentiment))
  data <- cbind(data, deleted=raw_data$mention_deleted)
  data <- cbind(data, process_source_url(raw_data$source_url))
  data <- cbind(data, process_author_type(raw_data$author_type))
  data <- cbind(data, process_author_mention_image(raw_data$author_image, raw_data$mention_image))
  data <- cbind(data, log_author_readers=process_author_readers(raw_data$author_readers))
  data <- cbind(data, creation_date=as.numeric(process_date_time(raw_data$mention_creationdate)))
  data <- cbind(data, modification_date=as.numeric(process_date_time(raw_data$author_modificationdate)))
  data <- cbind(data, process_text_length(raw_data$mention_title, raw_data$mention_text))
  data <- cbind(data, process_text(raw_data$mention_title, raw_data$mention_text, raw_data$author_excluded))
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


raw_data <- read.csv("data/maggi.csv", encoding = "UTF-8")
#raw_data <- rbind(raw_data, read.csv("data/raffaello.csv", encoding = "UTF-8"))
#raw_data <- rbind(raw_data, read.csv("data/nutrilon.csv", encoding = "UTF-8"))

data <- process_data(raw_data)
s_data <- separate_train_test_data(data, 0.3)


#model <- naiveBayes(as.factor(spam)~., data=s_data$train)
model <- svm(spam~., data=s_data$train, type="C", cost=200, gamma=0.05263158)
prediction <- predict(model, s_data$test[, -1])
tab <- table(pred=prediction, true=s_data$test$spam)

acc <- c(acc, (tab[1, 1] + tab[2, 2]) / 1306)
type1 <- c(type1, tab[2, 1] / 1306)




