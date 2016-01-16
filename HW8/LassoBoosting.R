"
Name: Salil Kanetkar 
UCLA ID: 704557096 
HW 8 Submission
"
n = 50
p = 200
s = 10
T = 10
lambda_all = (100:1)*10
L = length(lambda_all)
db = matrix(rep(0, p), nrow = p)

X = matrix(rnorm(n*p), nrow=n)
beta_true = matrix(rep(0, p), nrow = p)
beta_true[1:s] = 1:s
Y = X %*% beta_true + rnorm(n)

beta = matrix(rep(0, p), nrow = p)
beta_all = matrix(rep(0, p*L), nrow = p)
err = rep(0, L)
R = Y
ss = rep(0, p)
for (j in 1:p)
    ss[j] = sum(X[, j]^2)

for (l in 1:L)
{
    lambda = lambda_all[l]
    for (t in 1:T)
    {
      for (j in 1:p)
      {
        db = sum(R*X[, j])/ss[j]
        b = beta[j]+db
        b=sign(b)*max(0,abs(b)-lambda/ss[j])
        db=b-beta[j]
        R=R-X[,j]*db
        beta[j]=b
      }
    }
    beta_all[, l] = beta
    err[l] = sum((beta-beta_true)^2)
}
par(mfrow=c(1,3))
matplot(t(matrix(rep(1, p), nrow = 1)%*%abs(beta_all)), t(beta_all), type = 'l',main='LASSO BOOSTING',xlab='N',ylab='Beta')
plot(lambda_all, err, type = 'l',main='LASSO ERROR ESTIMATION',xlab='Lambda',ylab='Error')


T = 6000
epsilon = .0001
beta = matrix(rep(0, p), nrow = p)
db = matrix(rep(0, p), nrow = p)
beta_all = matrix(rep(0, p*T), nrow = p)

R = Y
for (t in 1:T)
{
  for (j in 1:p){
    db[j] = sum(R*X[,j])
  }
  j=which.max(abs(db))
  beta[j]=beta[j]+db[j]*epsilon
  R=R-X[,j]*db[j]*epsilon
  beta_all[,t] = beta
}
matplot(t(matrix(rep(1, p), nrow = 1)%*%abs(beta_all)), t(beta_all), type = 'l',main='STAGEWISE REGRESSION',xlab='N',ylab='Beta')