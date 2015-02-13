#' Finds the best value of k for the nearest neighbor classifier, judged by holdout accuracy
#'@param data A data frame or a matrix where rows are observations and 
#' columns are features
#'@param trueClasses A vector with labels for each row in \code{data} 
#'@param p Distance metric the k-NN Classifier should use, the value can be 
#' either 1 (Manhattan Distance), 2(Euclidean Distance) or Inf (Chebyshev Distance). 
#'@param k_seq a vector with the values for k to be evaluated
#'@param holdout the relative size of the holdout validation set - must be between 0 and 1
#'@param seed - optional; used for random shuffling, set for reproducible behavior 
#'@return a list with following objects: dataframe containing holdout and training accuracy, 
#'the actual best value for k judged by holdout accuracy and a plot of K vs. Training/Holdout Accuracy
#'@export
#'@import assertthat
#'@import ggplot2
#'@examples
#' # create artificial dataset
#' inputs  <- matrix(rnorm(200), ncol=2)
#' classes <- c(rep(0, 50), rep(1, 50))
#' k_seq <- c(1,3,5)
#' results <- find_best_k(inputs, classes,2, k_seq)
#' results$plot
#'
#'
find_best_k <- function(data, trueClasses, p=2, k_seq = c(1,2), holdout = 0.3, seed = NA){
  #source("knn.R")
  library(assertthat)
  not_empty(data);not_empty(k_seq);not_empty(trueClasses);
  if (is.na(seed) == F) {
      assert_that(typeof(k_seq) == "double")
  }
  assert_that(typeof(k_seq) == "double")
  #assert_that(typeof(seed) == "double")
  assert_that(holdout < 1 & holdout > 0)
  
  assert_that(p %in% c(1, 2, Inf))
  #shuffle data
  data_shuffled <- cbind(trueClasses, data)
  if(is.na(seed) == F){
    set.seed(seed)
  }
  d = ncol(data)
  data_shuffled <- data_shuffled[sample(nrow(data_shuffled)),]
  data <- data_shuffled[,2:(d+1)]
  trueClasses <- data_shuffled[,1]
  data_shuffled <- NA
  n <- nrow(data)
  train <- data[1:floor(n*(1-holdout)),]
  test <- data[floor(n*(1-holdout)+1):n, ]
  y_train = trueClasses[1:floor(n*(1-holdout))]
  y_test = trueClasses[(floor(n*(1-holdout))+1):n]
  holdout_accuracy <- rep(NA, length(k_seq))
  training_accuracy <- rep(NA, length(k_seq))
  for (i in 1:length(k_seq)){
    if (i == 1){
      results = kNN_classifier(data = test, memory = train, k = k_seq[i], trueClasses = y_train, type = "predict")
      holdout_accuracy[i] <- mean(results$predictedClasses == y_test)
      training_accuracy[i] = mean(kNN_classifier(data = train, k = k_seq[i], trueClasses = y_train, type = "train")$predictedClasses==y_train)
      neighbor_out = results$neighbor_out
    }
    #make use of the precomputed neighbors
    if(i > 1){
      holdout_accuracy[i] <- mean(kNN_classifier(data = test, p = p, memory = train, k = k_seq[i], trueClasses = y_train, type = "predict", warm_start = T, neighbors_in = neighbor_out)$predictedClasses == y_test)
      training_accuracy[i] = kNN_classifier(data = train, p = p, k = k_seq[i], trueClasses = y_train, type = "train", warm_start = T, neighbors_in = neighbor_out)$accuracy
      
      
    }
  }
  accuracy <- round(c(holdout_accuracy, training_accuracy),3)
  partition <- c(rep("Holdout", length(holdout_accuracy)), rep("Training", length(training_accuracy)))
  results <- data.frame(cbind(c(k_seq,k_seq), accuracy, partition))
  names(results) <- c("K", "Accuracy", "Partition")
  library(ggplot2)
  accuracy_plot <-  ggplot(data = results, aes(x=K, y=Accuracy, colour = Partition, group = Partition)) + geom_line() + theme_bw()
  best_k = k_seq[which.max(holdout_accuracy)]
  return(list(accuracy_results=results, plot = accuracy_plot, best_k = best_k))
}