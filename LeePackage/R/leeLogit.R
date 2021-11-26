#' calculate Logistic regression model's coefficients
#' calculate logit regression model's coefficients using Newton's or gradient optimization algorithm
#' @param y lables of samples in training data
#' @param X feature of samples in training data
#' @param iter the times we iterate in the iteration algorithm
#' @return coefficients of logistic regression model
#' @examples
#' leeLogit(y=c(rep(0,50),rep(1,50)),X=matrix(rnorm(100*10),ncol=10),method="Newton",iter=1000)
#'
#'

leeLogit<-function(y,X,method="Newton",iter=1000){
  n=length(y);p=ncol(X)
  y<-matrix(y,ncol=1)
  X1<-t(as.matrix(data.frame(1,X)))
  if(method=="Newton"){
    Beta<-matrix(0,nrow=p+1,ncol=iter)
    for(k in seq(2,iter,1)){
      beta<-Beta[,k-1]
      w1<-t(X1)%*%beta
      w2<-exp(w1)
      w3<-(1+w2)
      w4<-w2/w3**2
      w<-diag(as.vector(w4))
      grad<-X1%*%(w2/w3-y)/n
      Hes<-X1%*%w%*%t(X1)/n
      beta1<-beta-solve(Hes)%*%grad
      Beta[,k]<-beta1
    }
    return(Beta[,iter])
  }

  if(method=="gradient"){
    Beta=matrix(0,nrow =p+1,ncol=iter)
    gam=0.1

    for (k in seq(2,iter,1)){
      beta=Beta[,k-1]
      w1=t(X1)%*%beta
      w2=exp(w1)
      w3=w2/(1+w2)
      w4=w3-y
      grad=X1%*%w4/n
      beta1=beta-gam*grad
      Beta[,k]=beta1
    }
    return(Beta[,iter])
  }
}
