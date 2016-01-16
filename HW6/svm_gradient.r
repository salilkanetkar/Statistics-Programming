"
Name: Salil Kanetkar
UCLA ID: 704557096
"
#install.packages("data.table")
library(data.table) # allows us to use function fread,
# which quickly reads data from csv files 

accuracy <- function(S, y) 
{
  return(mean(S*y))
}

train <- function(X, Y, Xtest, Ytest, num_iterations=4000,learning_rate=1e-1,lambda=0.01) 
{
  n = dim(X)[1] 
  p = dim(X)[2]+1
  ntest = dim(Xtest)[1]
	Y=2*Y-1
	Ytest=2*Ytest-1
  X1 = cbind(rep(1, n), X)
  Xtest1 = cbind(rep(1, ntest), Xtest)
  accuracies_train = NULL
  accuracies_test = NULL
  #sigma = .1; 
  beta = matrix(rep(0,p), nrow=p)
  for(it in 1:num_iterations)
  {
    S=X1%*%beta
    db=S*Y<1
    Y_train = sign(S)
    accuracies_train = append(accuracies_train,accuracy(Y_train,Y))
    
    Stest=Xtest1%*%beta
    dbtest=Stest*Ytest<1
    Y_test = sign(Stest)
    accuracies_test = append(accuracies_test,accuracy(Y_test,Ytest))
    #print(dim(matrix(rep(1,n),nrow=1)))
    #print(dim(matrix(db*Y,n,p)*X1))
    dbeta=matrix(rep(1,n),nrow=1) %*% (matrix(db*Y,n,p)*X1)/n
    beta=beta+learning_rate*t(dbeta)
    beta[2:p]=beta[2:p]-lambda*t(dbeta)[2:p]
    
    
    if (it %% 100 == 0)
    {
      print(c(it,accuracies_train[length(accuracies_train)],accuracies_test[length(accuracies_test)]))
    }
    #pr=1/(1+exp(-X1%*%beta))
    #dbeta=(matrix(rep(1,n),nrow=1)%*%(matrix(Y-pr,n,p)*X1))/n
    #print (Y_test)
  }
  final_result = list(beta,accuracies_train,accuracies_test)
  return(final_result)
}


# load data
load_digits <- function(subset=NULL, normalize=TRUE) {
  
#Load digits and labels from digits.csv.

#Args:
#subset: A subset of digit from 0 to 9 to return.
#If not specified, all digits will be returned.
#normalize: Whether to normalize data values to between 0 and 1.

#Returns:
#digits: Digits data matrix of the subset specified.
#The shape is (n, p), where
#n is the number of examples,
#p is the dimension of features.
#labels: Labels of the digits in an (n, ) array.
#Each of label[i] is the label for data[i, :]

# load digits.csv, adopted from sklearn.

df <- fread("digits.csv") 


df <- as.matrix(df)

## only keep the numbers we want.
if (length(subset)>0) {
  
  c <- dim(df)[2]
  l_col <- df[,c]
  index = NULL
  
  for (i in 1:length(subset)){
    
    number = subset[i]
    index = c(index,which(l_col == number))
  }
  sort(index)
  df = df[index,]
}

# convert to arrays.
digits = df[,-1]
labels = df[,c]

# Normalize digit values to 0 and 1.
if (normalize == TRUE) {
  digits = digits - min(digits)
digits = digits/max(digits)}


# Change the labels to 0 and 1.
for (i in 1:length(subset)) {
  labels[labels == subset[i]] = i-1
}

return(list(digits, labels))

}

split_samples <- function(digits,labels) {

# Split the data into a training set (70%) and a testing set (30%).

num_samples <- dim(digits)[1]
num_training <- round(num_samples*0.7)
indices = sample(1:num_samples, size = num_samples)
training_idx <- indices[1:num_training]
testing_idx <- indices[-(1:num_training)]

return (list(digits[training_idx,], labels[training_idx],
        digits[testing_idx,], labels[testing_idx]))
}


#====================================
# Load digits and labels.
result = load_digits(subset=c(1, 7), normalize=TRUE)
digits = result[[1]]
labels = result[[2]]

result = split_samples(digits,labels)
training_digits = result[[1]]
training_labels = result[[2]]
testing_digits = result[[3]]
testing_labels = result[[4]]

# print dimensions
length(training_digits)
length(testing_digits)

# Train a net and display training accuracy.

final_result = train(training_digits, training_labels, testing_digits, testing_labels)
beta=final_result[[1]]
train_accuracy = final_result[[2]]
test_accuracy = final_result[[3]]
par(mfrow=c(1,2))
plot(1:length(final_result[[2]]),final_result[[2]],xlab='Iteration',ylab='Train Accuracy')
plot(1:length(final_result[[3]]),final_result[[3]],xlab='Iteration',ylab='Test Accuarcy')