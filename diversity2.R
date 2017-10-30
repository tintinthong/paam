#this function uses Lc
#x: wealth vector
#prob: proportion of wealth
#percent: percent of reward pool
diversity2<-function(x,reward_pool,q1=0.5,q2=0.5,p1=0.6,p2=0.6,p3=0.5,quant=c(0.3,0.9),percent=0.1){
  
  print("the begining---------------------------------------------------------")
  
  #set parameters
  #par(xpd=F)
  #if(prob[2]<=prob[1]){
  #  stop("prob2<prob1")
  #}
  
  #ensure x is all positive
  if(sum(x<0)!=0){
    stop("Not every element is positive")
  }
  
  #make sure there are names
  if(is.null(names(x))){
    names(x)<-seq(1,length(x))
  }
  
  q1=0.5 #can be anything
  q2=0.83 # can be anything
  p1=0.6 #restricted
  p2=0.5 #restricted
  p3=0.11 #restricted
  R=100 #can be anything
  
  set.seed(1)
  N<-length(x)
  x<-rgamma(100,1,1)
  o<-order(x)
  x_sort<-x[o]
  
  #xq <- quantile(x) 
  #sapply(xq, function(y)which.min(abs(x - y))) 
  
  #quantiles
  #n=tail(which(x_sort<quantile(x_sort,prob=q1)),1)
  #-----------------------------------------------
  n1=ceiling(N*q1)
  WC_1<-x_sort[1:n1]
  #select p1 with condition p1>q1(this should be selected based on skew)
  print(paste("Condition 1 is",p1>q1))
  w1=p1*R
  w1_unit=w1/n1
  n2=N-n1
  r1=R
  R=(1-p1)*R
  #------------------------------------------------
  n2=ceiling(n2*q2)
  WC_2<-x_sort[(n1+1):(n2)]
  #select p2 with condition p2<(w1/n1)*(n2/R) range 
  print(paste("Condition 2 is",p2<(w1/n1)*(n2/R)))
  w2=p2*R
  w2_unit=w2/n2
  n3=N-n2-n1
  r2=R
  R=(1-p2)*R
  #-------------------------------------------------
  n3=n3
  WC_3<-x_sort[(n2+1):length(x_sort)]
  #select p3 with condition p3<(w2/n2)*(n3/R) range 
  print(paste("Condition 3 is",p3<(w2/n2)*(n3/R)))
  w3=p3*R
  w3_unit=w3/n3
  #n4=N-n3
  r3=R
  R=(1-p3)*R
  #-------------------------------------------------
  
  WC=c(WC_1,WC_2,WC_3)
  w_vec=c(w1,w2,w3)
  #w_unit_vec=c(w1_unit,w2_unit,w3_unit)
  
  #-------------------------------------------------
  wg_unit=R/length(w_vec)
  w_vec=c(w1,w2,w3)+wg_unit*c(length(WC_1),length(WC_2),length(WC_3))
  w_unit_vec=c(w1/length(WC_1),w2/length(WC_2),w3/length(WC_3))
  
  
  w_vec
  w_unit_vec
  length(WC)
  
  
  #return(list(x=x,gini_before=ineq(x,type="Gini"),WC=c(WC_1,WC_2,WC_3),gini_after=ineq(c(WC_1,WC_2,WC_3),type="Gini"),rank_change=sum(rank_before==rank_after)/length(x),quant=quant ))
  return(list(x=x,w_vec=c(w1,w2,w3),w_unit_vec=c(w1_unit,w2_unit,w3_unit)))
  
}
#reward pool just doesn

debug(diversity)
diversity2(rgamma(100,1),prob=c(0.2,0.8))
diversity(rgamma(100,1,2))
diversity(rpareto(100000,2,2))

x<-c(1,2,3,5,5)
names(x)<-seq(1:length(x))
rank(x)

y<-x+c(5,5,5,1,1)
x
y
rank(x,ties.method="first")
rank(y,ties.method="first")
sum(rank(x,ties.method="first")==rank(y,ties.method="first"))/length(x)
