#----------Union of trees-------- 
#- we need a make_tree generator
#- rand_vect remove possibility of generating 0
#- extend color palette
#- add tests
#- assess code profiling

#extracting all attributes of a list
#h <- structure(42, foo=4, bar=2)
#unlist(attributes(h))

#remember to check the ifelse when performing the union 
# it may very well be that the not every vertex has root-node
#- you might be able to just add the attributes
#- make it as 0


#color palette has limited colors
#not a problem because you will not do
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector
pie(rep(1,n), col=sample(col_vector, n))
col_vector<-col_vector[-4]
pie(rep(1,5), col=col_vector[1:5])


#generate random combination of elements to obtain a sum 
#- remove possibility of 0(this will become a problem down the line)
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <-  ceiling(vec / sum(vec) * M)#round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}




#create list of trees
#name is vertex names(I think does not have to be vertex names)
create_list<-function(name,N_tree){
  
  #N_tree cant be the size of name
  #if name is length of 8 N_tree must be less than or equal 4
  #if name is length of 9 N_tree must be less than or equal 3
  
  n<-length(name)
  #n_tree_vec<-rand_vect(N_tree,n,1)
  if(N_tree>n){
    return("alert")
  }
  #if(N_tree>(as.integer(n/2)-1)){ #gotta think about this. I do not believe
  #  return("alert")
  #}
  
  n_tree_vec<-as.integer(sample(n,N_tree)/2+1)
  #while(sum(n_tree_vec)>n){   #gotta think about this. I do not believe
  #  n_tree_vec<-as.integer(sample(n,N_tree)/2+1)
  #}
  print(paste("n_tree_vec",n_tree_vec))
  #n_tree_vec<-sample(n,N_tree) this is the original
  list_tree<-lapply(n_tree_vec,generate_tree,name=name)
  
  #generate root labels that have not been used before(not particularly efficient code temporary fix)
  root_names<-c()
  for(i in 1:N_tree){
    root_names<-c(root_names,list_tree[[i]]$root_name)
  }
  
  #re-selecte root-nodes which were already used
  index<-which(duplicated(root_names))
  if(length(index)>=1){
    for(i in 1:length(index) ){
      no_set<-union(root_names,V(list_tree[[index[i]]])$name)
      comp<-as.character(name)[is.na(pmatch(as.character(name),no_set))]
      element<-sample(comp,1) #sometimes you run out of comp and all name  have been used
      list_tree[[index[i]]]$root_name<-element
      V(list_tree[[index[i]]])$name[1]<-element
      root_names[index[i]]<-element#update
    }
  }
  
  
  #comp<-as.character(name)[is.na(pmatch(as.character(name),root_names))]
  #samp<-sample(comp,sum(duplicated(root_names)),replace=FALSE)
  #for(i in 1:length(index) ){
  #  list_tree[[index[i]]]$root_name<-samp[i]
  #  V(list_tree[[index[i]]])$name[1]<-samp[i]
  #}
  
  # remove trees with 0 and 1 vert
  x<-sapply(sapply(list_tree,V),length)
  xremove<-which(x<2)
  if(length(xremove)>0){
    list_tree<-list_tree[-xremove]
  }
  
  
  #randomise attributes(color and wealth vector already created)
  for(i in 1:length(list_tree)){
    x<-rpois(1,500)
    V(list_tree[[i]])$wealth_vector[!is.na(V(list_tree[[i]])$wealth_vector)]<-100#x
    list_tree[[i]]<-wealth_trickle(list_tree[[i]],100)#x
    E(list_tree[[i]])$color<-col_vector[i]
  }
  
  return(list_tree)
}

#set root attribute of vertex in a tree
set_root<-function(treei,root_wealth){
  root= which(degree(treei, v = V(treei), mode = "in")==0, useNames = T) #you can just set root as first element
  wealth_vector<-rep(NA,length(V(treei)))
  wealth_vector[root]<-root_wealth
  treei<-set.vertex.attribute(treei,"wealth_vector",value=wealth_vector)
  treei$root_name<-names(root)
  return(treei)
}


#generate random tree using make_tree
#ni is the number of vertices in the tree

generate_tree<-function(ni,name,col="blue",root_wealth=50){
  namei<-sample(name,ni,replace=FALSE)
  #childi<-as.integer(sample(ni,1))#i changed number of children here(it works but does not give you deep graphs)
  #if(length(childi)<2){childi<-childi+1}
  childi<-as.integer(sample(ni,1)/2)+1 #+1 needed to remove possibility of 0 tihs is the original
  #childi<-ifelse(childi==0,childi+1,childi) #to ensure no error when 0 children
  treei<-make_tree(ni,children=childi)
  
  #there is no wealth vector yet
  
  #add attributes
  treei<-set.vertex.attribute(treei, "name", value=as.character(namei)) 
  treei<-set_root(treei,root_wealth)
  E(treei)$color <- col
  return(treei)
}


#create union add wealth_trickle
create_union<-function(tree_list){
  graph_union<-tree_list[[1]]
  for(i in 2:(length(tree_list))){
    graph_union<-graph.union(graph_union,tree_list[[i]],byname="auto")
    E(graph_union)$color <- ifelse(is.na(E(graph_union)$color_1),
                                  E(graph_union)$color_2,E(graph_union)$color_1)
    V(graph_union)$wealth_vector <- rep(NA,length(V(graph_union)$wealth_vector_1))
    index_1<-which(!is.na(V(graph_union)$wealth_vector_1))
    index_2<-which(!is.na(V(graph_union)$wealth_vector_2))
    V(graph_union)$wealth_vector[index_1]<-V(graph_union)$wealth_vector_1[index_1]
    V(graph_union)$wealth_vector[index_2]<-V(graph_union)$wealth_vector_2[index_2]
    
    #add weatlh
    V(graph_union)$wealth_vector_trickle_1[which(is.na(V(graph_union)$wealth_vector_trickle_1))]<-0
    V(graph_union)$wealth_vector_trickle_2[which(is.na(V(graph_union)$wealth_vector_trickle_2))]<-0
    V(graph_union)$wealth_vector_trickle<-V(graph_union)$wealth_vector_trickle_1+ V(graph_union)$wealth_vector_trickle_2
    
    #plot(graph_union,layout=layout_as_tree,weights=TRUE,edge.label=round(E(graph_union)$weight,2))
    
  }
  
  
  #add the wealth trickle
  
  return(graph_union)
}

#no of vertices, vertices of a treee and no of trees created, are all related in the create_list
undebug(create_list)
debug(create_list)
undebug(create_union)
debug(create_union)

#apply simulation
set.seed(7)
name<-sample(1:50,replace=FALSE) #use larger name space to avoid from comp being overflown also sometimes TRUE or FALSE needed because
#single vertex graph produced. we deal with this by making sure the name space is much larger than the number of trees(not ideal)
master_list<-create_list(name,10) 

master_list
debug(filter)

#apply simulation

set.seed(8)
name<-sample(1:30,replace=FALSE) #use larger name space to avoid from comp being overflown also sometimes TRUE or FALSE needed because
#single vertex graph produced. we deal with this by making sure the name space is much larger than the number of trees(not ideal)
master_list<-create_list(name,5) 

#apply simulation
set.seed(13)
name<-sample(1:30,replace=FALSE) #use larger name space to avoid from comp being overflown also sometimes TRUE or FALSE needed because
#single vertex graph produced. we deal with this by making sure the name space is much larger than the number of trees(not ideal)
master_list<-create_list(name,3) 
graph_union<-create_union(master_list)
plot(graph_union,curved=0.1,vertex.size=1*V(graph_union)$wealth_vector_trickle,layout=layout.davidson.harel)
#V(tree)$size<-2*V(tree)$wealth_vector_trickle

plot(master_list[[3]],layout=layout_as_tree,weights=TRUE,edge.label=round(E(master_list[[3]])$weight,2))

graph_union<-delete_edge_attr(graph_union, "weight" )
score<-page_rank(graph_union,directed=TRUE)

#compute fairness
fair(V(graph_union)$wealth_vector_trickle,1/score$vector) #fairness is bad
#notes the root_wealth is 100 for all

# the branch attribution is even

# plot barplots
par(mfrow=c(1,1))
barplot(V(graph_union)$wealth_vector_trickle,names.arg=V(graph_union)$name)
#k<-V(graph_union)$wealth_vector_trickle[1]/score$vector[1]
#barplot(k*score$vector)
barplot(score$vector)

#plothist
hist(V(graph_union)$wealth_vector_trickle)
hist(V(graph_union)$wealth_vector_trickle)
hist(score$vector)

#quantiles
plot(Lc(V(graph_union)$wealth_vector_trickle))
quant<-quantile(V(graph_union)$wealth_vector_trickle,probs=c(0.6,0.9,1))
V(graph_union)$wealth_vector_trickle
V(graph_union)$name
V(graph_union)$wealth_vector_trickle
Lc




WC_1<-V(graph_union)[V(graph_union)$wealth_vector_trickle<=quant[1]]
WC_2<-V(graph_union)[ ((V(graph_union)$wealth_vector_trickle<=quant[2]) +(V(graph_union)$wealth_vector_trickle>quant[1]))==2 ]
WC_3<-V(graph_union)[ ((V(graph_union)$wealth_vector_trickle<=quant[3]) +(V(graph_union)$wealth_vector_trickle>quant[2]))==2 ]
WC_1$wealth_vector_trickle
WC_2$wealth_vector_trickle
WC_3$wealth_vector_trickle
reward_pool<-5
#WC_1<-WC_1+0.6*reward_pool
#WC_2<-WC_2+0.3*reward_pool
#WC_1<-WC_1+0.1*reward_pool

ineq(V(graph_union)$wealth_vector_trickle,type="Gini")
ineq(c(WC_1,WC_2,WC_3),type="Gini")



quant[1]
quant[2]
quant[3]
length(WC_1)
length(WC_2)
length(WC_3)

#fit power law
fit<-fit_power_law(V(graph_union)$wealth_vector_trickle) #power law occurs the null hypothesis is not rejected

#inequality indexes
ineq(V(graph_union)$wealth_vector_trickle,type="Gini")
ineq(V(graph_union)$wealth_vector_trickle,type="RS")
ineq(V(graph_union)$wealth_vector_trickle,type="Atkinson")
ineq(V(graph_union)$wealth_vector_trickle,type="Theil")
ineq(V(graph_union)$wealth_vector_trickle,type="Kolm")
ineq(V(graph_union)$wealth_vector_trickle,type="var")
ineq(V(graph_union)$wealth_vector_trickle,type="entropy")

#plotting Lorenz Curve
plot(Lc(V(graph_union)$wealth_vector_trickle))

#power law to log-normal mu and sigma are (referring to normal distribution after log is applied)
trans_pareto_log<-function(x,mu,sigma,xmin,a){
  return(exp(mu+sigma*qnorm(1-(xmin/x)^a)))
}

R<-range(V(graph_union)$wealth_vector_trickle)
alpha<-5
sigma<-diff(R)/alpha
sigma<-sd(V(graph_union)$wealth_vector_trickle)


log_normals<-trans_pareto_log(V(graph_union)$wealth_vector_trickle,log(mean(V(graph_union)$wealth_vector_trickle)),log(sigma),fit$xmin,fit$alpha)
V(graph_union)$wealth_vector_trickle
log_normals
hist(V(graph_union)$wealth_vector_trickle)
hist(log_normals)
ineq(log_normals,type="Gini")
ineq(V(graph_union)$wealth_vector_trickle,type="Gini")

debug(page_rank)
graph_union

E(graph_union)$weight



g <- erdos.renyi.game(10, 1/1000)
get.edge.attribute(g)

set.seed(3)
tree<-generate_tree(20,sample(1:100))
tree<-target_branch(tree)


#occasionally get an error that the thing is not a graph object




#make union of graphs
graph_union<-create_union(master_list)
get.vertex.attribute(graph_union)

plot(create_union(master_list),edge.curved=0.1,layout=layout_as_star)
# visualise with size of vertex
plot(create_union(master_list),edge.curved=0.1,vertex.size=rep(0.1,length(V(graph_union))),vertex.label=NA, edge.arrow.size=0 )


#call wealth_trickle recursively until some condition is met
recursive_wealth_trickle<-function(tree,root_wealth=100,tau=0.2){
  wealth_trickle(tree,root_wealth=100,tau=0.2)
}


#wealth trickle edit2
#should change edit of the vertex attributes
wealth_trickle<-function(tree,root_wealth=100,tau=0.2){
  
  if( (length(V(tree))==1 ) || (length(V(tree))==0) ){
    return(tree)
  }
  
  
  #get targeted branch
  tree<-target_branch(tree) #adds attributes branch_list, branch_user, branch_index
  
  #should not go into this function
  tree<-filter(tree) #requires the attributes of branch_list, branch_user, branch_index
  
  #moving wealth attributes forward(make as class)
  #adjacencymatrix removes attributes(probably can remove effects of adjacency matrix)
  wealth_vector_place<-V(tree)$wealth_vector
  wealth_vector_trickle_place<-V(tree)$wealth_vector_trickle
  root_name_place<-tree$root_name
  branch_list_place<-tree$branch_list
  branch_user_place<-tree$branch_user
  branch_index_place<-tree$branch_index
  branch_final_place<-tree$branch_final
  
  
  #get adjacency matrix
  tree_mat<-as_adjacency_matrix(tree)
  tree_mat<-as.matrix(tree_mat)
  
  #make weights
  #1:length()is not general
  #vectorise please
  #targeting rows (which is not very general too)
  tree_mat[which(rowSums(tree_mat)!=0),]=tree_mat[which(rowSums(tree_mat)!=0),]/rowSums(tree_mat[which(rowSums(tree_mat)!=0),,drop=FALSE])
  #this is original
  #for(i in 1:sum(branch_index_place)){
  #  x<-tree_mat[i,]!=0
  #  tree_mat[i,x]<-branch_final_place[[i]][[1]]
  #}
  
  #re-assign attributes
  tree<-graph.adjacency(tree_mat,mode="directed",weighted=TRUE)
  tree<-set.vertex.attribute(tree,"wealth_vector",value=wealth_vector_place)
  tree<-set.vertex.attribute(tree,"wealth_vector_trickle",value=wealth_vector_trickle_place)
  tree$root_name<-root_name_place
  tree$branch_list<-branch_list_place
  tree$branch_user<-branch_user_place
  tree$branch_index<-branch_index_place
  tree$branch_final<-branch_final_place
  
  
  
  #find all simple paths from root to leaves
  leaves= which(degree(tree, v = V(tree), mode = "out")==0, useNames = T)
  root= which(degree(tree, v = V(tree), mode = "in")==0, useNames = T)
  paths<-all_simple_paths(tree, from = root, to = leaves)
  
  #length of each path(also can use diameter)
  len_tree<-sapply(paths,function(x) length(x)) #length of all paths from root to leaves
  max_len_tree<-max(len_tree) #longest length of all paths from root to leaves
  
  #return local-wealth of a layer
  local_wealth<-function(layer,mat,tau,root_wealth){
    out<-tau*(1-tau)^(layer)*(mat %^% (layer))[root,]
    return(out[which(out!=0)]*root_wealth)#
  }
  
  #iterate through values(replace 0.2 with tau and root_wealth should be an attribute of a vertex)
  #you can make this more elegant, also profile extending list and all
  wealth_list <- as.list(1:(max_len_tree)-1 ) #different values of layer
  #subtract<-function(x){return(x-1)}
  #wealth_list<-lapply(wealth_list,subtract)
  wealth_layers<-lapply(wealth_list[-1],FUN=local_wealth,mat=tree_mat,tau=tau,root_wealth=root_wealth)
  x<-tau*root_wealth
  names(x)<-V(tree)$name[1] #root can also use tree$root_name
  wealth_list[-1]<-wealth_layers
  wealth_list[[1]]<-x
  #tree$wealth_list<-wealth_list
  
  if(is.null(V(tree)$wealth_vector_trickle)){
    V(tree)$wealth_vector_trickle<-unlist(wealth_list)
  }else{
    V(tree)$wealth_vector_trickle<-V(tree)$wealth_vector_trickle+unlist(wealth_list)
  }
  
  tree$wealth_resid<-root_wealth-sum(V(tree)$wealth_vector_trickle) 
  
  #must carry forward other attributes from tree branch_list, branch_user, branch_index
  #V(tree)$size<-2*V(tree)$wealth_vector_trickle
  #plot(tree,layout=layout_as_tree,weights=TRUE,edge.label=round(E(tree)$weight,2))
  
  return(tree)
  
  #recycle wealth until it is used up(not particularly important)
  #target branch attributions
  
  
}

rsimplex<-function(n,alim=10){
  param<-sample(1:alim,n ,replace=TRUE)
  return(cls(sapply(param,rgamma,n=1)))
}

#creates placeholder for branch_list
#the weight are not assigned yet
target_branch<-function(tree){
  
  #breadth first search
  obj<-bfs(tree,root=tree$root_name,order=TRUE,rank=TRUE,father=TRUE,unreachable=FALSE)
  #tree_mat<-as.matrix(as_adjacency_matrix(tree))#can also just remove
  
  #vertex name which have a branch
  #branch_labels<-unique(na.omit(V(tree)[as.numeric(obj$father)[obj$order],na_ok=TRUE]))#can also remove
  
  #these are all the branches
  branch_list<-adjacent_vertices(tree,V(tree)$name,mode="out")
  branch_list<-lapply(branch_list,as.numeric)
  
  
  #name of the parents and index of vertices
  #the list are no longer parents
  
  #get user branches
  branch_user<-branch_list

  check_length<-function(x){
    if(length(x)==0){
      x<-NULL
    }else{
      x<-list(x,c(rsimplex(length(x))))
    }
    return(x)
  }
  
  #Make things null and add attribution
  branch_index<-sapply(branch_user,function(x){if(length(x)==0){x<-NULL}})
  branch_user<-lapply(branch_user,check_length)
  branch_list<-lapply(branch_list,check_length)
  
  
  #assign attributes
  tree$branch_list<-branch_list
  tree$branch_user<-branch_user
  tree$branch_index<-!unlist(lapply(branch_user,is.null))
  #tree$branch_final<-lapply(tree$branch_user,tree$branch_list,filter_attr)
  
  return(tree)
  
}




#filtering formula
filter_attr<-function(u,p,p_prev=NULL,w_prev=0.5,thres=0.5){
  
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
  map<-function(a,x){
    return(exp(-a*x))
  }
  
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

#filtering setup
filter<-function(tree){

  #dont like the use of sum
  tree$branch_final<-as.list(1:sum(tree$branch_index))
  for(i in 1:sum(tree$branch_index)){
    tree$branch_final[[i]]<-filter_attr(tree$branch_user[[i]][[2]],tree$branch_list[[i]][[2]])
    
  }
  names(tree$branch_final)<- names(tree$branch_user)[tree$branch_index]
  
  return(tree)  
}
























ptm <- proc.time()
proc.time() - ptm

attributes(branch_list)







tree$wealth_vector_trickle
tree$wealth_list[V(tree)[as.numeric(obj$father)[obj$order],na_ok=TRUE]]
tree$wealth_list
debug(wealth_trickle)

tree<-graph_from_literal(3-+5:6,6-+2,3-+4)
V(tree)
list<-list(jus1=c(1,2),jus2=20,jus3=19,jus4=13,jus5=123)
set.vertex.attribute(tree,"shish",list)
#tree<-make_tree(10)
root<- which(degree(tree, v = V(tree), mode = "in")==0, useNames = T)
obj<-graph.bfs(tree,root=names(root), neimode="out",father=TRUE,order=TRUE,unreachable = FALSE)


true_father<-V(tree)[as.numeric(obj$father)[obj$order],na_ok=TRUE] # or obj$order[as.numeric(obj$father)[obj$order],na_ok=TRUE] 

as.numeric(obj$father)[obj$order] #this is correct
as.numeric(obj$order)[obj$father] #not the same

#--- test code beginning to apply wealth trickle----
tree<-master_list[[1]]
plot(tree)
tree<-wealth_trickle(tree,100)
tree$root_name #where can I find these
tree$wealth_list
tree$wealth_resid
V(tree)$wealth_vector_trickle
tree<-wealth_trickle(tree,tree$wealth_resid)
100-sum(V(tree)$wealth_vector_trickle)
tree$wealth_resid
V(tree)$wealth_vector
V(tree)$name
V(tree)$wealth_vector_trickle


x<-c(1,2,3)
names(x)<-c(3,2,1)
x[as.character(c(2,1))]

#------test code which works-----

name<-sample(1:10,replace=FALSE)
tree1<-generate_tree(5,name)
tree2<-generate_tree(5,name)
get.vertex.attribute(tree1)
get.vertex.attribute(tree2)
graph_union<-create_union(list(tree1,tree2))
get.vertex.attribute(graph_union)



n1<-15
n2<-15
nsum<-n1+n2
x<-1:nsum
name<-sample(1:(nsum),replace=FALSE)
name1<-sample(name,n1,replace=FALSE)
name2<-sample(name,n2,replace=FALSE)
child1<-sample(n1,1)
child2<-sample(n2,1)

tree1<-make_tree(n1,children=child1)
tree2<-make_tree(n2,children=child2)
tree1<-set.vertex.attribute(tree1, "name", value=as.character(name1))
tree2<-set.vertex.attribute(tree2, "name", value=as.character(name2) )
E(tree1)$color <- 'blue'
E(tree2)$color<-"red"
plot(tree,layout=layout_as_tree)
plot(tree2,layout=layout_as_tree)

#treeunion
tree_union<-graph.union(tree1, tree2, byname=TRUE)
plot(tree1,layout=layout_as_tree)
plot(tree2,layout=layout_as_tree)
tree_union<-graph.union(tree1, tree2, byname="auto")
E(tree_union)$color <- ifelse(is.na(E(tree_union)$color_1),
                              E(tree_union)$color_2,E(tree_union)$color_1)
plot(tree_union,layout=layout_as_tree,edge.curved=0.1)



#coord<-duplicated(name1)
#name1[coord]<-sample(  x [! x %in% name1],length(name1[coord]) ,replace=FALSE)
#name1
