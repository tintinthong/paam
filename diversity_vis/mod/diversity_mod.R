#reduce decimal 
library(ineq)

fix_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))

party_fun<-function(x_sort,N,p,q,R,type="notlast"){
  n<-ceiling(N*q)
  if(type=="last"){
    n<-N
  }
  
  WC<-x_sort[1:n]
  w<-p*R
  w_unit<-w/n
  n_left<-N-n
  R<-(1-p)*R
  x_upd<-x_sort[(n+1):N] #x_upd makes no sense for last
  return(list(x_upd=x_upd,WC=WC,n=n,n_left=n_left,w=w,w_unit=w_unit,R=R))
} 

#party_fun(c(1,2,3),3, 0.1,0.2,100)


diversity<-function(x_sort,WC_n1,WC_n2,WC_n3){
  rank_before<-rank(x_sort) #before
  rank_after<-rank(c(WC_n1,WC_n2,WC_n3)) #after
  return(list(x_sort=x_sort,gini_before=ineq(x_sort,type="Gini"),gini_after=ineq(c(WC_n1,WC_n2,WC_n3),type="Gini"),rank_change=sum(rank_before==rank_after)/length(c(WC_n1,WC_n2,WC_n3)) ))
  
}


