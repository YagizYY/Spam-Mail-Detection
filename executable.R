library(data.table)

# All the results are going to be printed at the end.

# read datasets
x_train <- 
  fread("C:/Users/yagiz/Desktop/4-2/CS-464/NaiveBayes/Data/x_train.csv")
x_test <- 
  fread("C:/Users/yagiz/Desktop/4-2/CS-464/NaiveBayes/Data/x_test.csv")
y_train <- 
  fread("C:/Users/yagiz/Desktop/4-2/CS-464/NaiveBayes/Data/y_train.csv")
y_test <- 
  fread("C:/Users/yagiz/Desktop/4-2/CS-464/NaiveBayes/Data/y_test.csv")

#################################  PART 2.2 ###################################

P_spam = nrow(y_train[Prediction==1])/nrow(y_train)
P_normal = nrow(y_train[Prediction==0])/nrow(y_train)

x_train[, Prediction := y_train[, Prediction]]

all_spam_mail <- x_train[Prediction == 1][, Prediction := NULL]
all_normal_mail <- x_train[Prediction == 0][, Prediction := NULL]

col_sums_spam <- colSums(all_spam_mail)
spam <- data.table(words = names(col_sums_spam), 
                   sums = as.matrix(col_sums_spam))
setnames(spam, "sums.V1", "number_of_occurrences")
spam[, likelihood := number_of_occurrences/sum(number_of_occurrences)]

col_sums_normal <- colSums(all_normal_mail)
normal <- data.table(words = names(col_sums_normal), 
                     sums = as.matrix(col_sums_normal))
setnames(normal, "sums.V1", "number_of_occurrences")
normal[, likelihood := number_of_occurrences/sum(number_of_occurrences)]

normal[, log_likelihood := log(likelihood)]

# assign -10^12 to -Inf
normal[is.infinite(log_likelihood), log_likelihood := -10^12]

number_of_words <- nrow(x_test)
# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_normal <- rep(log(P_normal), nrow(x_test))


for (i in (1:number_of_words)){
  the_probs <- normal[, log_likelihood]*as.numeric(x_test[i])
  probs_normal[i] <- probs_normal[i] + sum(the_probs)
}

spam[, log_likelihood := log(likelihood)]

# assign -10^12 to -Inf
spam[is.infinite(log_likelihood), log_likelihood := -10^12]

number_of_words <- nrow(x_test)
# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_spam <- rep(log(P_spam), nrow(x_test))

for (i in (1:number_of_words)){
  the_probs <- spam[, log_likelihood]*as.numeric(x_test[i])
  probs_spam[i] <- probs_spam[i] + sum(the_probs)
}

compare22 <- data.table(for_normal = probs_normal, 
                      for_spam = probs_spam, 
                      prediction = y_test[, Prediction])

compare22[, assigned_class := ifelse(for_spam > for_normal, 1, 0)] 

confusion_matrix22 <- table(compare22$assigned_class, compare$prediction, 
                          dnn = c("Assigned Class", "Prediction"))

conf_mat22 <- data.table(confusion_matrix22)

#################################  PART 2.3 ###################################

P_spam = nrow(y_train[Prediction==1])/nrow(y_train)
P_normal = nrow(y_train[Prediction==0])/nrow(y_train)

V <- length(names(x_train))
alpha <- 5


x_train[, Prediction := y_train[, Prediction]]

all_spam_mail <- x_train[Prediction == 1][, Prediction := NULL]
all_normal_mail <- x_train[Prediction == 0][, Prediction := NULL]


col_sums_normal <- colSums(all_normal_mail)
normal <- data.table(words = names(col_sums_normal), 
                     sums = as.matrix(col_sums_normal))
setnames(normal, "sums.V1", "number_of_occurrences")
normal[, likelihood := 
         (number_of_occurrences + alpha)/(sum(number_of_occurrences)+alpha*V)]

col_sums_spam <- colSums(all_spam_mail)
spam <- data.table(words = names(col_sums_spam), 
                   sums = as.matrix(col_sums_spam))
setnames(spam, "sums.V1", "number_of_occurrences")
spam[, likelihood := 
       (number_of_occurrences + alpha)/(sum(number_of_occurrences)+alpha*V)]


normal[, log_likelihood := log(likelihood)]

# assign -10^12 to -Inf
normal[is.infinite(log_likelihood), log_likelihood := -10^12]

number_of_words <- nrow(x_test)
# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_normal <- rep(log(P_normal), nrow(x_test))


for (i in (1:number_of_words)){
  the_probs <- normal[, log_likelihood]*as.numeric(x_test[i])
  probs_normal[i] <- probs_normal[i] + sum(the_probs)
}

spam[, log_likelihood := log(likelihood)]

# assign -10^12 to -Inf
spam[is.infinite(log_likelihood), log_likelihood := -10^12]

number_of_words <- nrow(x_test)

# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_spam <- rep(log(P_spam), nrow(x_test))

for (i in (1:number_of_words)){
  the_probs <- spam[, log_likelihood]*as.numeric(x_test[i])
  probs_spam[i] <- probs_spam[i] + sum(the_probs)
}


compare23 <- data.table(for_normal = probs_normal, 
                      for_spam = probs_spam, 
                      prediction = y_test[, Prediction])

compare23[, assigned_class := ifelse(for_spam > for_normal, 1, 0)] 


confusion_matrix23 <- table(compare23$assigned_class, compare$prediction, 
                          dnn = c("Assigned Class", "Prediction"))

conf_mat23 <- data.table(confusion_matrix23)

#################################  PART 2.4 ###################################


P_spam = nrow(y_train[Prediction==1])/nrow(y_train)
P_normal = nrow(y_train[Prediction==0])/nrow(y_train)


x_train_for <- copy(x_train)
x_train_for[x_train_for != 0] <- 1

x_train_for[, Prediction := y_train[, Prediction]]

all_spam_mail <- x_train_for[Prediction == 1][, Prediction := NULL]
all_normal_mail <- x_train_for[Prediction == 0][, Prediction := NULL]

col_sums_spam <- colSums(all_spam_mail)
spam <- data.table(words = names(col_sums_spam), 
                   sums = as.matrix(col_sums_spam))
setnames(spam, "sums.V1", "number_of_appearance")
spam[, likelihood := number_of_appearance/sum(number_of_appearance)]

spam[, inv_likelihood := 1-likelihood]


col_sums_normal <- colSums(all_normal_mail)
normal <- data.table(words = names(col_sums_normal), 
                     sums = as.matrix(col_sums_normal))
setnames(normal, "sums.V1", "number_of_appearance")
normal[, likelihood := number_of_appearance/sum(number_of_appearance)]

normal[, inv_likelihood := 1-likelihood]


x_test_for <- copy(x_test)
x_test_for[x_test_for != 0] <- 1

normal[, c("log_likelihood", "log_inv_likelihood") := 
         .(log(likelihood), log(inv_likelihood))]


number_of_words <- nrow(x_test)
# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_normal <- rep(log(P_normal), nrow(x_test))


for (i in (1:number_of_words)){
  take <- as.numeric(x_test_for[i])
  ones_idx <- which(take == 1)
  zeros_idx <- which(take == 0)
  the_probs <- sum(normal[ones_idx, log_likelihood]) +
    sum(normal[zeros_idx, log_inv_likelihood])
  probs_normal[i] <- probs_normal[i] + the_probs
}

spam[, c("log_likelihood", "log_inv_likelihood") := 
       .(log(likelihood), log(inv_likelihood))]

number_of_words <- nrow(x_test)
# Create an empty vector with capacity of nrow(x_test) elements to allocate memory
probs_spam <- rep(log(P_spam), nrow(x_test))

for (i in (1:number_of_words)){
  take <- as.numeric(x_test_for[i])
  ones_idx <- which(take == 1)
  zeros_idx <- which(take == 0)
  the_probs <- sum(spam[ones_idx, log_likelihood]) +
    sum(spam[zeros_idx, log_inv_likelihood])
  probs_spam[i] <- probs_spam[i] + the_probs
}


compare24 <- data.table(for_normal = probs_normal, 
                      for_spam = probs_spam, 
                      prediction = y_test[, Prediction])

compare24[, assigned_class := ifelse(for_spam > for_normal, 1, 0)] 


confusion_matrix24 <- table(compare24$assigned_class, compare$prediction, 
                          dnn = c("Assigned Class", "Prediction"))

conf_mat24 <- data.table(confusion_matrix24)

# calculate for part 2.2
precision22 <- conf_mat22[4, N]/(conf_mat22[4,N]+conf_mat22[2,N])
recall22 <- conf_mat22[4, N]/(conf_mat22[4,N]+conf_mat22[3,N])

specificity22 <- conf_mat22[1, N]/(conf_mat22[1, N] + conf_mat22[3, N])

F_measure22 <- 2 * (precision22 * recall22) / (precision22 + recall22)

accuracy_percentage22 <- 100*(nrow(compare22[prediction==assigned_class])/nrow(compare22))

correct_predictions22 <- nrow(compare22[prediction==assigned_class])

wrong_predictions22 <- nrow(compare22[prediction!=assigned_class])

test_size22 <- nrow(compare22)

# calculate for part 2.3
precision23 <- conf_mat23[4, N]/(conf_mat23[4,N]+conf_mat23[2,N])
recall23 <- conf_mat23[4, N]/(conf_mat23[4,N]+conf_mat23[3,N])

specificity23 <- conf_mat23[1, N]/(conf_mat23[1, N] + conf_mat23[3, N])

F_measure23 <- 2 * (precision23 * recall23) / (precision23 + recall23)

accuracy_percentage23 <- 100*(nrow(compare23[prediction==assigned_class])/nrow(compare23))

correct_predictions23 <- nrow(compare23[prediction==assigned_class])

wrong_predictions23 <- nrow(compare23[prediction!=assigned_class])

test_size23 <- nrow(compare23)

# calculate for part 2.4
precision24 <- conf_mat24[4, N]/(conf_mat24[4,N]+conf_mat24[2,N])
recall24 <- conf_mat24[4, N]/(conf_mat24[4,N]+conf_mat24[3,N])

specificity24 <- conf_mat24[1, N]/(conf_mat24[1, N] + conf_mat24[3, N])

F_measure24 <- 2 * (precision24 * recall24) / (precision24 + recall24)

accuracy_percentage24 <- 100*(nrow(compare24[prediction==assigned_class])/nrow(compare24))

correct_predictions24 <- nrow(compare24[prediction==assigned_class])

wrong_predictions24 <- nrow(compare24[prediction!=assigned_class])

test_size24 <- nrow(compare24)

# Print the confusion matrix
cat("\nFor Part 2.2:\n") 
cat("\nConfusion Matrix:\n")
print(confusion_matrix22)

# Print the performance metrics
cat("\nPerformance Metrics\n")
cat(paste("Accuracy Percentage: ", round(accuracy_percentage22,4), "%\n", sep = ""))
cat(paste("Precision: ", round(precision22,4), "%\n", sep = ""))
cat(paste("Recall: ", round(recall22,4), "%\n", sep = ""))
cat(paste("F-measure: ", round(F_measure22,4), "%\n", sep = ""))
cat(paste("Specificity: ", round(specificity22,4), "%\n", sep = ""))
cat(paste("Correct Predictions: ", correct_predictions22, "\n", sep = ""))
cat(paste("Wrong Predictions: ", wrong_predictions22, "\n", sep = ""))
cat(paste("Test Size: ", test_size22, "\n", sep = ""))

# Print the confusion matrix
cat("\nFor Part 2.3:\n") 
cat("\nConfusion Matrix:\n")
print(confusion_matrix23)

# Print the performance metrics
cat("\nPerformance Metrics\n")
cat(paste("Accuracy Percentage: ", round(accuracy_percentage23,4), "%\n", sep = ""))
cat(paste("Precision: ", round(precision23,4), "%\n", sep = ""))
cat(paste("Recall: ", round(recall23,4), "%\n", sep = ""))
cat(paste("F-measure: ", round(F_measure23,4), "%\n", sep = ""))
cat(paste("Specificity: ", round(specificity23,4), "%\n", sep = ""))
cat(paste("Correct Predictions: ", correct_predictions23, "\n", sep = ""))
cat(paste("Wrong Predictions: ", wrong_predictions23, "\n", sep = ""))
cat(paste("Test Size: ", test_size23, "\n", sep = ""))

# Print the confusion matrix
cat("\nFor Part 2.4:\n") 
cat("\nConfusion Matrix:\n")
print(confusion_matrix24)

# Print the performance metrics
cat("\nPerformance Metrics\n")
cat(paste("Accuracy Percentage: ", round(accuracy_percentage24,4), "%\n", sep = ""))
cat(paste("Precision: ", round(precision24,4), "%\n", sep = ""))
cat(paste("Recall: ", round(recall24,4), "%\n", sep = ""))
cat(paste("F-measure: ", round(F_measure24,4), "%\n", sep = ""))
cat(paste("Specificity: ", round(specificity24,4), "%\n", sep = ""))
cat(paste("Correct Predictions: ", correct_predictions24, "\n", sep = ""))
cat(paste("Wrong Predictions: ", wrong_predictions24, "\n", sep = ""))
cat(paste("Test Size: ", test_size24, "\n", sep = ""))