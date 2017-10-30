#Diversity

#apply simulation
set.seed(13)
name<-sample(1:500,replace=FALSE) #use larger name space to avoid from comp being overflown also sometimes TRUE or FALSE needed because
#single vertex graph produced. we deal with this by making sure the name space is much larger than the number of trees(not ideal)
master_list<-create_list(name,100) 
graph_union<-create_union(master_list)
#plot(graph_union,curved=0.1,vertex.size=1*V(graph_union)$wealth_vector_trickle,layout=layout.davidson.harel)

graph_union<-delete_edge_attr(graph_union, "weight" )
score<-page_rank(graph_union,directed=TRUE)


#notes the root_wealth is 100 for all
# the branch attribution is even
#compute fairness
fair(V(graph_union)$wealth_vector_trickle,1/score$vector) #fairness is bad

#barplots
barplot(V(graph_union)$wealth_vector_trickle,names.arg=V(graph_union)$name)
barplot(score$vector)

#plothist
hist(V(graph_union)$wealth_vector_trickle)
hist(score$vector)

#quantiles
par(xpd=F)
plot(Lc(V(graph_union)$wealth_vector_trickle),xlab="percent of people",ylab="percent of wealth")
length(V(graph_union)$wealth_vector_trickle) #200 vertices
sum(Lc(V(graph_union)$wealth_vector_trickle)$L>0.5) #28 owns more than 50% of wealth
sum(Lc(V(graph_union)$wealth_vector_trickle)$L>0.6) #22 owns more than 40% of wealth
Lc(V(graph_union)$wealth_vector_trickle)$p[which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.5)[1]] #0.865 percent of ppl hold 0.5 percent of wealth
Lc(V(graph_union)$wealth_vector_trickle)$p[which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.7)[1]] #0.925 percent of ppl hold 0.7 percent of wealth
o<-order(V(graph_union)$wealth_vector_trickle)
#V(graph_union)[o][which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.5)[1]]

q3<-V(graph_union)[o]$wealth_vector_trickle[which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.9)[1]]
q2<-V(graph_union)[o]$wealth_vector_trickle[which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.6)[1]]
q1<-V(graph_union)[o]$wealth_vector_trickle[which(Lc(V(graph_union)$wealth_vector_trickle)$L>0.3)[1]]
quant<-c(q1,q2,q3)
quant

#prepare quantiles
#quant<-quantile(V(graph_union)$wealth_vector_trickle,probs=c(0.6,0.9,1))

#visualise
hist(V(graph_union)$wealth_vector_trickle)
abline(v=quant)


WC_1<-V(graph_union)[V(graph_union)$wealth_vector_trickle<=quant[1]]$wealth_vector_trickle
names(WC_1)<-V(graph_union)[V(graph_union)$wealth_vector_trickle<=quant[1]]$name
WC_2<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[2]) +(V(graph_union)$wealth_vector_trickle>quant[1]))==2 ]$wealth_vector_trickle
names(WC_2)<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[2]) +(V(graph_union)$wealth_vector_trickle>quant[1]))==2 ]$name
WC_3<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[3]) +(V(graph_union)$wealth_vector_trickle>quant[2]))==2 ]$wealth_vector_trickle
names(WC_3)<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[3]) +(V(graph_union)$wealth_vector_trickle>quant[2]))==2 ]$name



#set up
rank_before<-rank(c(WC_1,WC_2,WC_3),ties.method="first") #before
hist(c(WC_1,WC_2,WC_3)) #before
sigma1<-min(WC_2)-max(WC_1)
sigma2<-min(WC_3)-max(WC_2)
tau1<-mean(WC_2)-mean(WC_1)
tau2<-mean(WC_3)-mean(WC_2)
tau1<-quantile(WC_2)[2]-quantile(WC_1)[4]
tau2<-quantile(WC_3)[2]-quantile(WC_2)[4]
sigma1
sigma2
tau1
tau2
sum(V(graph_union)$wealth_vector_trickle)#1383.947



#hist(WC_1)
#mean(WC_1)
#hist(WC_2)
#mean(WC_2)
#hist(WC_3)
#mean(WC_3)


#WC_2<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[2]) +(V(graph_union)$wealth_vector_trickle>quant[1]))==2 ]
#WC_3<-V(graph_union)[((V(graph_union)$wealth_vector_trickle<=quant[3]) +(V(graph_union)$wealth_vector_trickle>quant[2]))==2 ]
reward_pool<-0.1*sum(V(graph_union)$wealth_vector_trickle)
reward_pool

#if(0.6*reward_pool<sigma1){
#  WC_1<-WC_1+0.6*reward_pool
#  print("good sigma1")
#}else{
#  WC_1<-WC_1+tau1
#}
#if(0.3*reward_pool<sigma2){
#  WC_2<-WC_2+0.3*reward_pool
#  print("good sigma2")
#}else{
#  WC_2<-WC_2+tau2
#}
#WC_3<-WC_3+0.1*reward_pool

#choose between using tau1 or using some percentage obviously if the percentage is less than tau 1 then it is better
tau1
tau2

0.6*reward_pool/length(WC_1)
0.3*reward_pool/length(WC_2)
0.1*reward_pool/length(WC_3)

1/(sum(WC_1)/sum(c(WC_1,WC_2,WC_3)) ) *reward_pool
1/(sum(WC_2)/sum(c(WC_1,WC_2,WC_3)) )*reward_pool
1/(sum(WC_3)/sum(c(WC_1,WC_2,WC_3)) ) *reward_pool
reward_pool

#if we use tau1
if(tau1*length(WC_1)>reward_pool){
  print("tau1*length(WC_1)>reward_pool")
  WC_1<-WC_1+reward_pool/length(WC_1)
  reward_pool<-0
}else{
  WC_1<-WC_1+tau1
  reward_pool<-reward_pool-tau1*length(WC_1)
}

if(reward_pool!=0){
  if(tau2*length(WC_2)>reward_pool){
    WC_2<-WC_2+reward_pool/length(WC_1)
  }else{
    WC_2<-WC_2+tau2
    reward_pool<-reward_pool-tau1*length(WC_2)
  }
}

if(reward_pool!=0){
  if(tau2<reward_pool/length(WC_3)){
    WC_3<-WC_3+tau2/2
    reward_pool<-reward_pool-length(WC_3)*(tau2/2)
    WC_1<-WC_1+reward_pool/length(c(WC_1,WC_2,WC_3))
    WC_2<-WC_2+reward_pool/length(c(WC_1,WC_2,WC_3))
    WC_3<-WC_3+reward_pool/length(c(WC_1,WC_2,WC_3))
  }else{
    WC_3<-reward_pool/length(WC_3)
    reward_pool<-0
  }
}

reward_pool



sigma1*length(WC_1) #amount paid if bad sigma1
sigma2*length(WC_2) #amount paid if bad sigma2
tau1*length(WC_1) 
tau2*length(WC_2) 

ineq(V(graph_union)$wealth_vector_trickle,type="Gini")
ineq(c(WC_1,WC_2,WC_3),type="Gini")
plot(Lc(V(graph_union)$wealth_vector_trickle))
plot(Lc(c(WC_1,WC_2,WC_3)))


#investigation as to whether the ranking has changed(keep in mind that it should be a function of lorenz)


rank_after<-rank(c(WC_1,WC_2,WC_3),ties.method="first") #after
rank_before==rank_after
sum(rank_before==rank_after)/length(V(graph_union)$wealth_vector_trickle)
hist(c(WC_1,WC_2,WC_3))


#this function uses Lc
#x: wealth vector
#prob: proportion of wealth
#percent: percent of reward pool
diversity<-function(x,prob=c(0.3,0.9),percent=0.1){
  
  print("the begining---------------------------------------------------------")
  
  #set parameters
  #par(xpd=F)
  if(prob[2]<=prob[1]){
    stop("prob2<prob1")
  }
  
  #ensure x is all positive
  if(sum(x<0)!=0){
    stop("Not every element is positive")
  }
  
  #make sure there are names
  if(is.null(names(x))){
    names(x)<-seq(1,length(x))
  }
  
  #set order
  o<-order(x)
  #find the corresponding quantiles from prob (proportion wealth)
  #q2<-x[o][which(Lc(x)$L>prob[2])[1]]
  #q1<-x[o][which(Lc(x)$L>prob[1])[1]]
  q1<-tail(x[o][which(Lc(x)$L<prob[1])],1)
  q2<-tail(x[o][which(Lc(x)$L<prob[2])],1)
  quant<-c(q1,q2)
  
  #visualise
  #hist(x)
  #abline(v=quant)
  
  #define wealth_classes
  WC_1<-x[x<=quant[1]]
  WC_2<-x[((x<=quant[2]) +(x>quant[1]))==2]
  WC_3<-x[ (x>quant[2]) ]

  #change ranking
  rank_before<-rank(c(WC_1,WC_2,WC_3)) #before
  
  #make taus
  tau1<-quantile(WC_2)[2]-quantile(WC_1)[4]
  tau2<-quantile(WC_3)[2]-quantile(WC_2)[4] #should re-define tau2 such that it is less than tau1
  
  #make reward pool
  reward_pool<-percent*sum(x)
  
  
  #if we use tau1
  if(tau1*length(WC_1)>reward_pool){
    print(paste("giving all of reward pool to WC_1","value per unit=",reward_pool/length(WC_1),"total=",reward_pool) )
    WC_1<-WC_1+reward_pool/length(WC_1)
    reward_pool<-0
  }else{
    print(paste("giving tau1","value per unit=",tau1,"total=",tau1*length(WC_1)) )
    WC_1<-WC_1+tau1
    reward_pool<-reward_pool-tau1*length(WC_1)
  }
  
  if(reward_pool!=0){
    
    if(tau2*length(WC_2)>reward_pool){
      
      if(reward_pool/length(WC_2)>tau1/length(WC_1)){ 
        print(paste("giving all of reward pool to WC_2","value per unit=",reward_pool/length(WC_2),"total=",reward_pool) )
        WC_2<-WC_2+0.8*tau1 #the problem with this is that tau1 may very well be worse
        reward_pool<-0
      }
      
    }else{
      #if(tau1/length(WC_1)<tau2/length(WC_2)){
        print(paste("giving tau2","value per unit=",tau2,"total=",tau2*length(WC_2)) )
        WC_2<-WC_2+tau2
        reward_pool<-reward_pool-tau2*length(WC_2)
      #}else{
      #  print(paste("giving tau1 because tau2>tau1","value per unit=",tau1,"total=",tau1*length(WC_2)) )
      #  WC_2<-WC_2+tau1
      #  reward_pool<-reward_pool-tau1*length(WC_2)
      #}
     
    }
      
    
  }
  #add this tau1>tau2 thing to 3rd
  
  if(reward_pool!=0){
    if(tau2<reward_pool/length(WC_3)){
      WC_3<-WC_3+tau2/2
      reward_pool<-reward_pool-length(WC_3)*(tau2/2)
      WC_1<-WC_1+reward_pool/length(c(WC_1,WC_2,WC_3))
      WC_2<-WC_2+reward_pool/length(c(WC_1,WC_2,WC_3))
      WC_3<-WC_3+reward_pool/length(c(WC_1,WC_2,WC_3))
    }else{
      WC_3<-WC_3+reward_pool/length(WC_3)
      reward_pool<-0
    }
  }
  
  #measure changes in ranking
  rank_after<-rank(c(WC_1,WC_2,WC_3)) #after
  #ties first still doesnt make things as tho they dont change
  
  
  #set parameters back
  #par(xpd=T)
  
  
  return(list(x=x,gini_before=ineq(x,type="Gini"),WC=c(WC_1,WC_2,WC_3),gini_after=ineq(c(WC_1,WC_2,WC_3),type="Gini"),rank_change=sum(rank_before==rank_after)/length(x),quant=quant ))
  
}
#reward pool just doesn

debug(diversity)
diversity(rgamma(100,1),prob=c(0.6,0.2))
diversity(rgamma(100,1,2))
diversity(rpareto(100000,2,2))
