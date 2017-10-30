#load library
library(compositions)

#useful functions for managing simplex
cls<-function(x){
  return(x/sum(x))
}

ait.sum<-function(x,y){
  return(cls(x*y))
}

ait.scale<-function(a,x){
  return(cls(x^a))
}

ait.dot<-function(x,y){
  n<-length(x)
  index<-expand.grid(1:n,1:n)
  
  return((1/(2*n))*sum( log(x[index[,1]]/x[index[,2]])*log(y[index[,1]]/y[index[,2]] )  ) )
}

ait.dist<-function(x,y){
  n<-length(x)
  index<-expand.grid(1:n,1:n)
  return(sqrt((1/(2*n))*sum( (log(x[index[,1]]/x[index[,2]]) -log(y[index[,1]]/y[index[,2]] ))^2  ) ))
}


u<-c(0.1,0.1,0.8)
p<-c(0.2,0.2,0.6)
p_prev<-c(0.3,0.3,0.4)
w_prev<-0.2#reliance on p
thres<-0.5
filter_attr(u,p,p_prev,w_prev,thres)




#---------------------
#original

#filtering formula
filter_attr<-function(u,p,p_prev=NULL,w_prev=0.5,thres=0.5){
  #mapping from R to (0,1)
  map<-function(a,x){
    return(exp(-a*x))
  }
  
  #u and p cannot be null at the same time 
  #w_pre  cannot be null
  #thres cannot be null
  
  if(is.null(p_prev)){
    p_prev<-p
  }
  
  #make checks
  #w_prev should be between 0 and 1
  #check whether u is reported, put in null
  #
  
  if (map(0.1,ait.dist(u,p))>thres| map(0.1,ait.dist(p,p_prev))>thres) {
    phat<-u
    w<-w_prev+0.05
  } else if (is.null(u)) {
    phat<-p
    w<-w_prev+0.05
  } else
    phat<- ait.dot(ait.scale(w,p), ait.scale((1-w),u))
  if(w>1){
    w<-1
  }else if(w<0){
    w<-0
  }
  return(list(phat=phat,w=w))
}


#new edit
filter_attr<-function(u,p,p_prev=NULL,beta=0.5,kappa1=0.7,kappa2=0.7,kappa3=0.7){
  #mapping from R to (0,1)
  map<-function(a=0.1,x){
    return(exp(-a*x))
  }
  
  if(is.null(p_prev)){
    p_prev<-p
  }
  
  pbar<-ait.sum(ait.scale(beta,p_prev),ait.scale(1-beta,u))
  lambda<-0.4
  if(map(lambda,ait.dist(u,p))<kappa3){
    print(paste("u and p similar"))
    if(map(lambda,ait.dist(u,p_prev))<kappa2 && map(lambda,ait.dist(p,p_prev))>kappa1){
      phat<-u
      beta<-beta-0.05
    }else if(map(lambda,ait.dist(u,p_prev))>kappa2 && map(lambda,ait.dist(p,p_prev))<kappa1){
      phat<-p
      beta<-beta+0.05
    }else if(map(lambda,ait.dist(u,p_prev))<kappa2 && map(lambda,ait.dist(p,p_prev))<kappa1){
      phat<-u
    }else{
      print(paste("in the first else"))
      phat<-pbar
    }
  }else{
    print(paste("u and p different"))
    if(map(lambda,ait.dist(u,p_prev))<kappa2 && map(lambda,ait.dist(p,p_prev))>kappa1){
      phat<-pbar
      beta<-beta+0.025
    }else if(map(lambda,ait.dist(u,p_prev))>kappa2 && map(lambda,ait.dist(p,p_prev))<kappa1){
      phat<-pbar
       beta<-beta+0.025
    }else{
    print(paste("in the second else"))
    phat<-pbar
   }
    
  }
  return(list(phat=phat,beta=beta))
}


build_list<-function(df){
  for(i in 1:nrow(df$p)){
    print(i)
    obj<-filter_attr(u=df$u[i,],p=df$p[i,],p_prev=df$p_prev[i-1,],beta=df$beta[i])
    #obj<-filter_attr(u=df$u[i,],p=df$p[i,],p_prev=df$p_prev,beta=df$beta[i])
    df$phat<-rbind(df$phat,obj$phat)
    df$beta<-c(df$beta,obj$beta)
    #df$p_prev<-obj$phat
    df$p_prev<-rbind(df$p_prev,obj$phat)
  }
  return(df)
}

filter_attr(u,p,p_prev)
sum(filter_attr(u,p,p_prev)$phat)

u<-c(0.8,0.1,0.1)
#p<-c(0.11,0.12,0.77)
p_prev<-NULL
beta<-0.5

n<-5
mat<-matrix(NA,nrow=n,ncol=3)
mat[1,]<-rsimplex(3)
for(i in 2:nrow(mat)){
  mat[i,]<-ait.scale(i*0.3,mat[i-1,])
}
p<-mat
mat<-t(replicate(n,u))
u<-mat
u[(n-1):n,]<-t(replicate(2,c(0.2,0.2,0.6)))


start<-list(u=u,p=p,p_prev=NULL,beta=0.5,phat=NULL)
undebug(build_list)
results<-build_list(start)


plot<-ggtern(data=data.frame(na.omit(results$phat)),aes(x=X1,y=X2,z=X3))+geom_point(size=2,fill="yellow",color="red",shape=1)+
  geom_path(color="red",linetype=2,size=0.2,arrow=arrow(type="closed",length=unit(0.15,"cm")))+ggtitle('Ternary Diagram')

plot

plot+geom_point(data=data.frame(p),fill="yellow",color="blue",size=2,shape=1)+
  geom_path(data=data.frame(p),color="blue",arrow=arrow(type="closed",length=unit(0.15,"cm")) ,linetype=2,size=0.2)+
  geom_point(data=data.frame(u),fill="yellow",color="green",size=2,shape=1)+
  geom_path(data=data.frame(u),color="green",arrow=arrow(type="closed",length=unit(0.15,"cm")),linetype=2,size=0.2)
  
results$beta
  
plot+ geom_point(data=data.frame(results$p_prev),fill="yellow",color="purple",size=2,shape=1)+geom_path(data=data.frame(results$p_prev),color="purple",arrow=arrow(type="closed",length=unit(0.15,"cm")),linetype=2,size=0.2)
  



p
u
results$phat

plot<-ggtern(data=data.frame(p),aes(x=X1,y=X2,z=X3))+geom_point(size=2,fill="yellow",color="red",shape=1) +labs(title="Ternary Plot")+
  geom_path(color="blue",linetype=2,size=1) 
plot<-ggtern(data=data.frame(u),aes(x=X1,y=X2,z=X3))+geom_point(size=2,fill="yellow",color="red",shape=1) +labs(title="Ternary Plot")+
  geom_path(color="blue",linetype=2,size=1) 
plot


u<-c(0.6,0.1,0.3)
p<-c(0.11,0.12,0.77)
p_prev<-c(0.1,0.05,0.85)
beta<-0.5
filter_attr(u,p,p_prev)
df<-data.frame(rbind(u,p,p_prev))
df<-data.frame(rbind(u))
df
colnames(df)
base<-ggtern(data=df,aes(x=X1,y=X2, z=X3))+geom_point()#geom_point()
base
#plot(acomp(df),id=TRUE,plot=TRUE,atpen=TRUE)
#plot.acomp
base+labs(x="Left",y="Top",z="Right")
#+scale_T_continuous(name = "1st", limits = NULL, breaks = waiver(), minor_breaks = waiver(), labels = waiver())
#+scale_L_continuous(name = waiver(), limits = NULL, breaks = waiver(), minor_breaks = waiver(), labels = waiver())
#+scale_R_continuous(name = waiver(), limits = NULL, breaks = waiver(), minor_breaks = waiver(), labels = waiver())

rsimplex<-function(n,alim=10){
  param<-sample(1:alim,n ,replace=TRUE)
  return(cls(sapply(param,rgamma,n=1)))
}

mat<-matrix(NA,nrow=10,ncol=3)
mat[1,]<-rsimplex(3)
for(i in 2:nrow(mat)){
  mat[i,]<-ait.scale(i*0.1,mat[i-1,])
}
df<-data.frame(mat)


for(i in 1:nrow(mat)){
  mat[i,]<-rsimplex(3)
}
df2<-data.frame(mat)

library(ggtern)

u<-c(0.2,0.3,0.5)
p<-c(0.11,0.12,0.77)
p_prev<-c(0.1,0.05,0.85)
df<-data.frame(rbind(u,p,p_prev))

plot<-ggtern(data=df,aes(x=X1,y=X2,z=X3))+geom_point(size=2,fill="yellow",color="red",shape=1) +labs(title="Ternary Plot")+
  geom_path(color="blue",linetype=2,size=1) 
plot+geom_point(data=df2)+
  geom_path(data=df2,color="red",linetype=1,size=1)+
geom_Tisoprop(value=seq(0,1,by=0.25),colour='darkred') +
  geom_Lisoprop(value=c(.5),colour='darkgreen') +
  geom_Risoprop(value=c(.5),colour='darkblue') 

u<-data.frame(u)
p<-data.frame(p)
plot<-ggtern(data=p,aes(x=X1,y=X2,z=X3))+geom_point(size=2,fill="yellow",color="red",shape=1) +labs(title="Ternary Plot")+
  geom_path(color="blue",linetype=2,size=1) 
plot+geom_point(data=u,size=2,fill="yellow",color="blue")+
  geom_path(data=u,color="red",linetype=2,size=1)
  
plot

#+ opts(legend.position = "none")
#+scale_T_continuous(limits=c(0.3,1.0)) + scale_L_continuous(limits=c(0,0.7)) + scale_R_continuous(limits=c(0,0.7))


#Most geometries can be used as-is
plot <- ggtern(data=data.frame(x=c(.7,.1,.3),
                               y=c(.1,.8,.2),
                               z=c(.2,.1,.5)),
               aes(x=x,y=y,z=z)) +geom_point(size=3,fill="yellow",color="red",shape=21) 
plot
+
  geom_polygon(alpha=0.5,color="red",size=2) +
  geom_path(color="blue",linetype=2,size=1) +
  geom_point(size=3,fill="yellow",color="red",shape=21) +
  geom_smooth(method="lm",se=F,limitarea=F,fullrange=T,
              color="magenta",size=1,linetype=3)
plot

