
#load library
#library(Rsolnp)
library(igraph)
library(expm) #matrix power
library(RColorBrewer)
#library(LCA)
#library(compositions)
library(ineq)


#------- Finding trees in a random sample graph--------

#generate full network
set.seed(1)
N<-100 # no of nodes
network<-sample_forestfire(nodes=N, fw.prob=0.37, bw.factor=0.32/0.37,directed=TRUE)
#network<-sample_pa(N,power=2)
network<-set.vertex.attribute(network, "name", value=1:N)
E(tree)$curved<-TRUE
plot(network,edge.curved=0.2) # it is actually quite connected

#randomly finding a tree within a graph
sub_N<-as.integer(N/2)
f <- function(graph, data, extra) {
  data['rank'] == sub_N
}

bfs_tree<-bfs(network,root=V(network)[sample(1:N,1)], neimode="out",father=TRUE,order=TRUE,unreachable = FALSE,callback=f)
sub_tree<-rbind( bfs_tree$father[bfs_tree$order[1:sub_N,na_ok=TRUE], na_ok = TRUE],bfs_tree$order[1:sub_N,na_ok=TRUE])[,-1] #edge_list
sub_tree<-sub_tree[,!is.na(colSums(sub_tree))] #remove NA's
sub_tree<-graph(sub_tree)
nV_sub_tree<-length(V(sub_tree))
sub_tree<-set.vertex.attribute(sub_tree, "name", value=1:nV_sub_tree) #remember to always add attributes

bfs_tree$father[bfs_tree$order[1:sub_N,na_ok=TRUE],na_ok=TRUE]
plot(sub_tree)
sub_tree<-delete.vertices(sub_tree, degree(sub_tree)==0)
plot(sub_tree,layout=layout_as_tree)

#randomly finding many trees within a network
#-note this tree is a full breadth search with transversal order reduced to sub_N
find_tree<-function(network,bench_wealth=1000){
  sub_N<-as.integer(N/2)
  N_tree<-sub_N #N_tree<N
  f <- function(graph, data, extra) {
    data['rank'] == sub_N
  }
  mylist.names <- paste("t",seq(1,N_tree), sep="" )
  master_list<-sapply(mylist.names, assign,NULL)
  rand_vert<-V(network)[sample(1:N,N_tree)]
  for(i in 1:length(rand_vert) ){
    print(i) #remove this when ready
    bfs_tree<-bfs(network,root=V(network)[rand_vert[i]], neimode="out",father=TRUE,order=TRUE,unreachable = FALSE,callback=f)
    sub_tree<-rbind( bfs_tree$father[bfs_tree$order[1:sub_N,na_ok=TRUE], na_ok = TRUE],bfs_tree$order[1:sub_N,na_ok=TRUE])[,-1] #edge_list
    if(all(is.na(sub_tree))){sub_tree<-NULL} #if all nodes are NA
    else{
      sub_tree<-sub_tree[,!is.na(colSums(sub_tree))] #remove NA's
      sub_tree<-graph(sub_tree)
      nV_sub_tree<-length(V(sub_tree))
      sub_tree<-set.vertex.attribute(sub_tree, "name", value=1:nV_sub_tree) #remember to always add attributes
      sub_tree<-delete.vertices(sub_tree, degree(sub_tree)==0)
      
    }
    master_list[[i]]<-sub_tree
    attributes(master_list[[i]])$"root_wealth"<-rpois(1,max(degree(sub_tree)))*runif(1,0,bench_wealth)
    
  }
  return(master_list)
}

#main iteration using filtering 
master_out<-find_tree(network)
for(i in 1:N_tree){
  #if(!is.null(master_out[[i]])){
   print(wealth_trickle(master_out[[i]],root_wealth=attributes(master_out[[i]])$"root_wealth"))
  #}
}











#----trying to isolate each branch-------------------------------------------------------------------
obj
obj 
obj$father[obj$order]
obj
for(i in obj$order){
  print(i)
}
obj_mat<-rbind( obj$father[obj$order],obj$order) #edge_list
obj$father
obj$order

leaves= which(degree(tree, v = V(tree), mode = "out")==0, useNames = T)
root= which(degree(tree, v = V(tree), mode = "in")==0, useNames = T)
paths<-all_simple_paths(tree, from = root, to = leaves)
all_simple_paths(tree, from = 3, to = leaves)

#-------------


#----wealth trickle----

wealth_trickle<-function(tree,root_wealth=100,tau=0.2){
  #adjacency matrix
  tree_mat<-as_adjacency_matrix(tree)
  tree_mat<-as.matrix(tree_mat)
  
  #print(tree_mat)
  #make weights
  tree_mat[which(rowSums(tree_mat)!=0),]=tree_mat[which(rowSums(tree_mat)!=0),]/rowSums(tree_mat[which(rowSums(tree_mat)!=0),,drop=FALSE])
  tree<-graph.adjacency(tree_mat,mode="directed",weighted=TRUE)
  plot(tree,layout=layout_as_tree,weights=TRUE,edge.label=round(E(tree)$weight,2))
  
  #find all simple paths from root to leaves
  leaves= which(degree(tree, v = V(tree), mode = "out")==0, useNames = T)
  root= which(degree(tree, v = V(tree), mode = "in")==0, useNames = T)
  paths<-all_simple_paths(tree, from = root, to = leaves)
  
  #length of each path(also can use diameter)
  len_tree<-sapply(paths,function(x) length(x)) #length of all paths from root to leaves
  max_len_tree<-max(len_tree) #longest length of all paths from root to leaves
  
  #return local-wealth of a layer
  local_wealth<-function(layer,mat,tau,root_wealth){
    out<-tau*(1-tau)^(layer)*(tree_mat %^% (layer))[root,]
    return(out[which(out!=0)]*root_wealth)#
  }
  
  #iterate through values(replace 0.2 with tau and root_wealth should be an attribute of a vertex)
  mylist <- as.list(1:(max_len_tree-1) ) #different values of layer
  return(sapply(mylist,FUN=local_wealth,mat=tree_mat,tau=tau,root_wealth=root_wealth))
  
  #recycle wealth until it is used up(not particularly important)

}


wealth_trickle(tree)

plot(make_tree(10),layout=layout_as_tree)


#until less than epsilon(not particularly important)
eps_stop<-function(eps){
  mylist <- as.list(1:(max_len_tree-1) )
  for(i in mylist){
    if(sum(local_wealth(layer=i,mat=tree_mat,tau=0.2,root_wealth=100))<eps*100){
      return(paste("finished at",i))
    }
  }
}


local_wealth<-function(layer,mat,tau,root_wealth){
  out<-tau*(1-tau)^(layer)*(tree_mat %^% (layer))[-1,]
  return(out[which(out!=0)]*root_wealth)#
}






#----------------------optimisation-------------------

#specify fairness function
fair<-function(x,w){
  top<-sum(w*x)^2
  bottom<-length(x)*sum((x*w)^2)
  return(top/bottom)
}
fair(c(2,1,0),c(1/2,1,0))

#specify your function
#opt_func <- function(x) {
#  10 - 5*x[1] + 2 * x[2] - x[3]
#}

#specify the equality function. The number 15 (to which the function is equal)
#is specified as an additional argument
#equal <- function(x) {
#  x[1] + x[2] + x[3] 
#}


equal<-function(M){
  p1<-M[1,1]+M[1,2]+M[1,3]
  p2<-M[2,2]
  p3<-M[3,1]+M[3,2]
  return(c(p1,p2,p3))
}

#example
#goeqfn = function(dat, n)
#{
#  x = dat[1:n]
#  y = dat[(n+1):(2*n)]
#  z = dat[(2*n+1):(3*n)]
#  apply(cbind(x^2, y^2, z^2), 1, "sum")
#}

#the optimiser - minimises by default
solnp(c(5,5,5), #starting values (random - obviously need to be positive and sum to 15)
      fair, #function to optimise
      eqfun=equal, #equality function 
      eqB=1,   #the equality constraint
      LB=c(0,0,0), #lower bound for parameters i.e. greater than zero
      UB=c(100,100,100))#upper bound for parameters (I just chose 100 randomly)
      

#-----------Curiosity------------
#what is a consensus tree
#consensus_tree(tree_union, hrg = NULL, start = FALSE, num.samples = 10000)



#--- useful list arguments----
#master_list<-vector("list", N_tree)
#c(master_list, paste("t",i,sep="")= sub_tree)
mylist.names <- paste("t",seq(1,N_tree), sep="" )
#master_list<-vector("list", length(mylist.names))
master_list<-sapply(mylist.names, assign,NULL)

#---------------


#network<-sample_smallworld(dim=1, size=100, nei=5, p=0.05, loops = FALSE, multiple = FALSE)
#network<-simplify(network) #remove loops and multiple edges
#plot(rewire(graph.lattice(10, nei=2, dir=TRUE, mutual=TRUE),each_edge(8/100)))







#E(h)
#E(h) %>% rev() #reversing sequence of edge 

#wealth trickle edit
#should change edit of the vertex attributes
wealth_trickle<-function(tree,root_wealth=100,tau=0.2){
  
  #moving wealth attributes forward(make as class)
  #adjacencymatrix removes attributes(probably can remove effects of adjacency matrix)
  wealth_vector_place<-V(tree)$wealth_vector
  wealth_vector_trickle_place<-V(tree)$wealth_vector_trickle
  root_name_place<-tree$root_name
  #V(tree)$wealth_vector_trickle move forward
  
  
  #adjacency matrix
  tree_mat<-as_adjacency_matrix(tree)
  tree_mat<-as.matrix(tree_mat)
  
  #print(tree_mat)
  #make weights
  tree_mat[which(rowSums(tree_mat)!=0),]=tree_mat[which(rowSums(tree_mat)!=0),]/rowSums(tree_mat[which(rowSums(tree_mat)!=0),,drop=FALSE])
  tree<-graph.adjacency(tree_mat,mode="directed",weighted=TRUE)
  tree<-set.vertex.attribute(tree,"wealth_vector",value=wealth_vector_place)
  tree<-set.vertex.attribute(tree,"wealth_vector_trickle",value=wealth_vector_trickle_place)
  tree$root_name<-root_name_place
  V(tree)$size<-V(tree)$wealth_vector_trickle
  plot(tree,layout=layout_as_tree,weights=TRUE,edge.label=round(E(tree)$weight,2))
  
  
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
  wealth_layers<-sapply(wealth_list[-1],FUN=local_wealth,mat=tree_mat,tau=tau,root_wealth=root_wealth)
  x<-tau*root_wealth
  names(x)<-V(tree)$name[1] #root can also use tree$root_name
  wealth_list[-1]<-wealth_layers
  wealth_list[[1]]<-x
  tree$wealth_list<-wealth_list
  
  if(is.null(V(tree)$wealth_vector_trickle)){
    V(tree)$wealth_vector_trickle<-unlist(wealth_list)
  }else{
    V(tree)$wealth_vector_trickle<-V(tree)$wealth_vector_trickle+unlist(wealth_list)
  }
  
  tree$wealth_resid<-root_wealth-sum(V(tree)$wealth_vector_trickle)
  return(tree)
  
  #recycle wealth until it is used up(not particularly important)
  #target branch attributions
  
  
}


