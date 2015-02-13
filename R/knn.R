#' kNN classifier 
#' 
#' Classify the input with a k nearest neighbors classifier.
#'
#' @param data A data frame or a matrix where rows are observations and 
#' columns are features. If \code{type} is "train" this is training 
#' dataset, and if it is "predict" it is test dataset.
#' @param trueClasses A vector with labels for each row in \code{data} 
#' if \code{type} is "train", and with labels for each row in 
#' \code{memory} if \code{type} is "predict".
#' @param memory A data frame or a matrix where rows are observations 
#' and columns are features. If \code{type} is "train" this argument 
#' is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has 
#' to be an odd number.
#' @param warm_start indicates whether previosuly computed neighbors should be used. This
#' is useful to speed up cross-validation to search for a  suitable value of k
#' @param neighbors_in specifies the previously computed matrix of neighbors
#' @param p Distance metric the classifier should use, the value can be 
#' either 1, 2 or Inf. 
#' @param type Whether the goal is to train the classifier or predict 
#' classes of new observations based on past ones. The value can be 
#' either "train" or "predict".
#' @return A list with following elements: predictedClasses, 
#' accuracy and errorCount and a matrix of nearest neighbors
#' @export
#' @import assertthat 
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(200), ncol=2)
#' inputsTrain  <- matrix(rnorm(200), ncol=2)
#' classesTrain <- c(rep(0, 50), rep(1, 50))
#' # get the kNN predictions for the test set
#' kNN_classifier(inputsTest, classesTrain, inputsTrain, 
#' k=15, p=2, type="predict")


kNN_classifier <- function(data, trueClasses, memory=NULL, 
                             k=1, p=2, type="train", warm_start = F, neighbors_in = NULL, scale = F) {
  # test the inputs
  library(assertthat)
  not_empty(data); not_empty(trueClasses); 
  if (type=="train") {
    assert_that(nrow(data)==length(trueClasses))
  }
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k); 
  assert_that(p %in% c(1, 2, Inf))
  if (type=="predict" & warm_start == F) {
    assert_that(not_empty(memory) & 
                  ncol(memory)==ncol(data) & 
                  nrow(memory)==length(trueClasses))
  }
  if (warm_start == T){
    assert_that(not_empty(neighbors_in))
  }
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  # Compute the distance between each point and all others 
  noObs <- nrow(data)
  
  # if we are making predictions on the test set based on the memory, 
  # we compute distances between each test observation and observations
  # in our memory
  if (type=="train") {
    predictionId <- 1

    
    # computing distances between and instances in the
    # training data
    if (p == 1) {
      distMatrix <- as.matrix(dist(data, method = "manhattan", diag = T, upper = T))
    } else if (p == 2) {
      distMatrix <- as.matrix(dist(data, method = "euclidean", diag = T, upper = T))
    }
    else if (p==Inf)
    {
      distMatrix <- as.matrix(dist(data, method = "maximum"), diag = T, upper = T)
    }
    
    neighbors <- apply(distMatrix, 1, order)
    }
   
  else if (type == "predict" & warm_start == F) {
    predictionId <- 0
    noMemory <- nrow(memory)
    distMatrix <- matrix(NA, noObs, noMemory)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(data[obs,])
      probeExpanded <- matrix(rep(probe, each=noMemory), nrow=noMemory)
      
      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(memory - 
                                             probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
      }  
    }
    neighbors <- apply(distMatrix, 1, order)
  } else if (warm_start == T & type == "predict"){
    predictionId <- 0
    neighbors <- neighbors_in
  }
  
  
  
  # Sort the distances in increasing numerical order and pick the first 
  # k elements
  # Compute and return the most frequent class in the k nearest neighbors
  prob <- predictedClasses <-  rep(NA, noObs)
  for (obs in 1:noObs) {
    predictedClasses[obs] <- Mode(trueClasses[neighbors[(1+predictionId):
                                                          (k+predictionId), obs]])
    
  }
  
  # examine the performance, available only if training
  if (type=="train") {
    errorCount <- table(predictedClasses, trueClasses)
    accuracy <- mean(predictedClasses==trueClasses)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  # return the results
  return(list(predictedClasses=predictedClasses, 
              prob=prob,
              accuracy=accuracy,
              errorCount=errorCount,
              neighbor_out = neighbors))
}

