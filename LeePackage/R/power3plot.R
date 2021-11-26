#' plot the power under different population mean and 3 different sample scale
#' @description plot the power change for different nomal population mean under 3 different sample scale
#' @param nseq a vector containing three positive integer, such as c(10,20,30)
#' @param museq a vector containing a sequnce of number, used as the mean of nomal distribution
#' @return a picture
#' @examples
#' power3plot(nseq=c(10,50,100),museq=seq(-2,2,by=0.1))
#'
#'

power3plot<-function(nseq,museq){
  if(length(nseq)!=3){return("The length of nseq must be: 3")}
  part<-function(n,mu,sd=2){
    X<-rnorm(n,mean=mu,sd=sd)
    return(sqrt(n)*mean(X)/sd(X))
  }

  full<-function(n,mu){
    Tn<-replicate(10**4,part(n,mu))
    return(mean(abs(Tn)>qt(0.975,df=n-1)))
  }

  Re<-matrix(0,nrow=length(nseq),ncol=length(museq))
  for(i in 1:nrow(Re)){
    for(j in 1:ncol(Re)){
      Re[i,j]=full(nseq[i],museq[j])
    }
  }

  word1<-paste("n=",nseq[1],sep = "")
  word2<-paste("n=",nseq[2],sep = "")
  word3<-paste("n=",nseq[3],sep = "")
  plot(museq,Re[1,],col="red",type="o",ylim=c(0,1),xlab=expression(mu),ylab="power")
  points(museq,Re[2,],type="o",col="green")
  points(museq,Re[3,],type="o",col="blue")
  legend(x="bottomright",legend=c(word1,word2,word3),col=c("red","green","blue"),pch=c("o","o","o"),lty=c(1,1,1),cex=0.6)
}
