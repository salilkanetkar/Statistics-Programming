
myLogistic <- function(X, Y)
{
n <- nrow(X)
p <- ncol(X)    

beta <- matrix(rep(0, p), nrow = p)
epsilon <- 1e-6
repeat
{
  eta <- X%*%beta
  pr <- expit(eta)
  w <- pr*(1-pr)
  Z <- eta + (Y-pr)/w
  sw <- sqrt(w)
  mw <- matrix(sw, n, p)
  Xwork <- mw*X
  Ywork <- sw*Z
  beta_new <- lm(Ywork ~ 0 + Xwork)$coefficient
  err <- sum(abs(beta_new-beta))
  beta <- beta_new
  if (err<epsilon)
      break
}
return(beta)
}

expit <- function(x)
{
y <- 1/(1+exp(-x))
return(y)
}

n = 1000
p = 5
X = matrix(rnorm(n*p), nrow=n)
beta = matrix(rep(1, p), nrow = p)
Y = runif(n) < expit(X %*% beta)
myLogistic(X, Y)