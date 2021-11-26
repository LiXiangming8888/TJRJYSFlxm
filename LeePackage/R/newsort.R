#' sort a sequence of numbers from the smallest to largest.
#'
#' @description sort a sequence of numbers according their size.
#' @param a vector containing a sequence of numbers.
#' @return sorted vector in which the number is posited in order.
#' @examples
#' newsort(a=c(1,7,4,3,10,-4))
#'
#'

newsort<-function(a){
  for(i in seq(1,length(a)-1)){
    for(j in seq(0,length(a)-2)){
      if(a[j+1]>a[j+2]){
        x1<-a[j+1] ; x2<-a[j+2]
        a[j+1]<-x2 ; a[j+2]<-x1
      }
    }
  }
  return(a)
}
