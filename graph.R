
#import igraph
library(igraph)
library(igraphdata)

#generate erdos renyli
g <- sample_gnp(20, 5/20, directed=TRUE)

#generate forest fire
g <- sample_forestfire(10, fw.prob=0.37, bw.factor=0.32/0.37)

#generate citation graph
g<-make_full_citation_graph(100, directed = TRUE)

#generate small world graph
g <- sample_smallworld(1, 100, 5, 0.05)

#generate barbasi alber
g<-sample_pa(100)

#write graph as adjacency matrix
M<-as_adjacency_matrix(g)

#labelling vertices
V(g)$name <- letters[1:vcount(g)]


#adding weights
#E(g)$weight <- runif(ecount(g))

#write graph as adjacency matrix with weight attributes
#M<-as_adjacency_matrix(g, attr="weight")


N<-length(V(g))
M<-as.matrix(M)


divide.row.sum<-function(row){
  row.sum<-sum(row)
  if(row.sum!=0){
    return(row/row.sum)
  }else{
    return(row)
  }
}


prop<-function(col){
  col[col!=0]<-log(length(col)/col[col!=0])
  col[col!=0]<-col[col!=0]/sum(col[col!=0])
  return(col)
}

#tf-idf implemented in function prop and divide.row.sum

prop.flat<-function(col){
  if(sum(col)==0){
    return(NULL)
  }
  col.names<-colnames(col)
  col<-col[col!=0]
  col.names<-col.names[col!=0]
  col<-col/sum(col)
  return(col)
}

M<-apply(M,2,divide.row.sum)
M<-apply(M,1,prop)
g<-graph_from_adjacency_matrix(as.matrix(M),mode="directed",weighted=TRUE)
plot(g,edge.label=round(E(g)$weight,2))


#degree 
deg<-degree(g,mode="in")
hist(deg, breaks=1:vcount(g)-1, main="Histogram of node degree")

#degree distribution
degree_distribution(g)
degree_distribution(g,cumulative=TRUE)
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
                                                                     xlab="Degree", ylab="Cumulative Frequency")

#centrality
degree(g, mode="in")
centr_degree(g, mode="in", normalized=T)

#closeness
closeness(g, mode="all", weights=NA) 
centr_clo(g, mode="all", normalized=T)

#eigenvector(small world no warning)
eigen_centrality(g, directed=T, weights=NA)
centr_eigen(g, directed=T, normalized=T)




#------------depth-seearch first-------

# %du% make_tree(10)
h<-make_tree(10)
plot(h)
E(h)
V(h)
is.weighted(h)
plot(h)
bfs(h, root=2, "out",
    unreachable=FALSE, restricted=TRUE, order=TRUE, rank=TRUE, father=TRUE)


bfs(g, root=11, "out",
    TRUE, TRUE, TRUE, TRUE, father=TRUE)

g <- graph(c(1,2,2,6,1,4,4,6,5,6,1,5,5,3,3,4), directed=FALSE)
plot(g)
r <- graph.bfs(g, root=1, neimode='all', order=TRUE, father=TRUE)

g <- sample_smallworld(1, 5, 5, 0.05)
plot(g)
r <- graph.bfs(g, root=1, neimode='all', order=TRUE, father=TRUE,unreachable=TRUE)

g <- graph(c(1,2,2,6,1,4,4,6,5,6,1,5,5,3,3,4), directed=FALSE)
r <- graph.bfs(g, root=1, neimode='all', order=TRUE, father=TRUE)
h <- graph( rbind(r$order, r$father[r$order, na_ok = TRUE])[,-1], directed=FALSE )
plot(h)



#----example-----
h<-make_tree(10)

#with normal vertex attributes
graph.bfs(h, root="1", neimode='out', order=TRUE, father=TRUE,unreachable=FALSE) #father output seems correct
plot(h,layout=layout_as_tree)
V(h)

#with renamed vertex attributes
set.seed(1)
h<-set.vertex.attribute(h, "name", value=sample(1:10,10))

plot(h,layout=layout_as_tree)
obj<-graph.bfs(h, root="3", neimode='out', order=TRUE, father=TRUE,unreachable=FALSE)  #father output seems wrong
obj$father<-vertex_attr(h, "name")[obj$father]
obj


#------------------


V(h)[8]
r2
V(h)
r$order
r2$order
r$order
r$father[r$order, na_ok = TRUE]
E(h)$weight <- runif(ecount(h))
plot(h,edge.label=round(E(h)$weight,2))
M<-as_adjacency_matrix(g)
M<-apply(M,2,divide.row.sum)
M<-apply(M,1,prop)
g<-graph_from_adjacency_matrix(as.matrix(M),mode="directed",weighted=TRUE)
plot(g,edge.label=round(E(g)$weight,2))

#adjacent vertices
adjacent_vertices(g, c(2,3), mode = c("out"))

#edges
E(g)[incident(g, 4,"out")]$weight

#weights are on the non-closer side of the arrow
el <- matrix( c(1, 2, 3, 4), nc = 2, byrow = TRUE)
el
g<-graph_from_edgelist(el)
plot(g)
E(g)$weight<-c(1,2)
plot(g,edge.label=round(E(g)$weight,2))
E(g)[incident(g,4,"in")]$weight


#-----lets try small network-------
g <- sample_smallworld(1, 50, 5, 0.05)
M<-as_adjacency_matrix(g)
M<-apply(M,2,divide.row.sum)
M<-apply(M,1,prop)
g<-graph_from_adjacency_matrix(as.matrix(M),mode="directed",weighted=TRUE)
plot(g,edge.label=round(E(g)$weight,2))

#use layout
coords <- layout_(g, as_star())
plot(g, layout = coords)

#cricle layout
g %>%
add_layout_(in_circle(), component_wise()) %>%
plot()

#tree layout
plot(g, layout = layout_as_tree)

#edges
E(g)[incident(g, 4,"out")]$weight

#----page rank----- 
#http://infolab.stanford.edu/~backrub/google.html
#http://www.ams.org/samplings/feature-column/fcarc-pagerank

length(E(g))
length(V(g))

#gives rank for each vector
page_rank(g)$vector



#-------load real data-------------

mydata = read.table("citation.txt")

g<-graph_from_data_frame(mydata, directed = TRUE)

#-----exploratory data--------
#no nodes
length(V(g))

#no edges
length(E(g))

#degree 
deg.out<-degree(g,mode="out")
deg.in<-degree(g,mode="in")

#point estimates
mean(deg.out)
max(deg.out)
min(deg.out)
length(deg.out[deg.out==0])
length(deg.out[deg.out==max(deg.out)])
length(deg.out)
#hist(log(deg), breaks=1:vcount(g)-1, main="Histogram of node degree")

mean(deg.in)
max(deg.in)
min(deg.in)
length(deg.in[deg.in==0])
length(deg.in[deg.in==max(deg.in)])
length(deg.in)

#degree distribution
deg.dist.out<- degree_distribution(g, mode="out")
plot(x=0:max(deg), y=deg.dist.out, pch=19, cex=1.2, col="orange",
     xlab="Degree", ylab="Distribution")

#cumulative degree distribution
deg.dist.out<- degree_distribution(g, cumulative=TRUE, mode="out")
#plot(x=0:max(deg), y=1-deg.dist.out, pch=19, cex=1.2, col="orange",
#      xlab="Degree", ylab="Cumulative Frequency")

power<-power.law.fit(deg.out)
plot(deg.dist.out,log="x",
     bg="black",pch=21,
     xlab="Degree",
     ylab="Cumulative Frequency")


#centrality
degree(g, mode="in")
centr_degree(g, mode="in", normalized=T)

#closeness
closeness(g, mode="all", weights=NA) 
centr_clo(g, mode="all", normalized=T)

#eigenvector(small world no warning)
eigen_centrality(g, directed=T, weights=NA)
centr_eigen(g, directed=T, normalized=T)

#union of two graphs
par(mfrow=c(1,1))
net1 <- graph_from_literal(D-A:B:F:G, A-C-F-A, B-E-G-B, A-B, F-G,
                           H-F:G, H-I-J)
net2 <- graph_from_literal(D-A:F:Y, B-A-X-F-H-Z, F-Y)
plot(net1)
plot(net2)
str(net1 %u% net2)
plot(net1 %u% net2)
#IGF
#ZF


graph.reverse <- function (graph) {
  if (!is.directed(graph))
    return(graph)
  e <- get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}

net1<-graph_from_literal( F-+K,E-+K,C-+B,A -+ B -+ C-+A,A-+ E,F-+A,G-+A,K-+A-+F)
plot(net1)
net2<-graph.reverse(net1)
pg<-page_rank(net1)
pg2<-page_rank(net2)

pg$vector
sort(degree(net1,mode="in"))
sort(pg$vector)
sort(degree(net2,mode="in"))
sort(pg2$vector)

x<-1-pg$vector
sort(pg$vector)
sort(x/sum(x))
sort(x/sqrt(sum(x^2))) #does not sum to 1

#oooh aitchison sums to 1
sum(acomp(rep(1/7,7))-acomp(pg$vector))
sort(acomp(rep(1/7,7))-acomp(pg$vector))
sort(pg$vector)
betweenness(net1, v = V(net1), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)

#compute betweeness centrality
g <- sample_gnp(10000, 5/20, directed=TRUE)
deg<-degree(g,mode="in")
hist(deg)
bet<-betweenness(g, v = V(g), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)
eig<-eigen_centrality(g,directed=TRUE)
pg<-page_rank(g)
sum(pg$vector) 
eig
bet


g<-make_full_citation_graph(100, directed = TRUE)
as_adjacency_matrix(g)

#contract two vertices
g <- make_ring(10)
g$name <- "Ring"
V(g)$name <- letters[1:vcount(g)]
E(g)$weight <- runif(ecount(g))

g2 <- contract(g, rep(1:5, each=2),
               vertex.attr.comb=toString)
plot(g)
plot(g2)


plot(cbind(c(1,7),c(2,3)))
cbind(c(1,7),c(2,3))
0.8*c(7,3)+0.2*c(1,2)
points(t(0.8*c(7,3)+0.2*c(1,2)))

plot(seq(0,20,0.1),1/(exp(seq(0,20,0.1))))

plot(seq(0,20,0.1),1/(1+exp(seq(0,20,0.1))))
plot(seq(0,20,0.1),1/(1+exp(-seq(0,20,0.1))))


plot(h)
page_rank(h)
barplot(page_rank(h)$vector)

#generate erdos renyli
g <- sample_gnp(100, 5/20, directed=TRUE)

#generate forest fire
g <- sample_forestfire(100, fw.prob=0.37, bw.factor=0.32/0.37)

#generate citation graph
g<-make_full_citation_graph(100, directed = TRUE)

#generate small world graph
g <- sample_smallworld(1, 100, 5, 0.05)
#NORMAL 

#generate barbasi alber
g<-sample_pa(100)

plot(g)
hist(page_rank(g)$vector)
hist(page_rank(g)$vector)

x<-page_rank(g)$vector
hist(x)
x[x>mean(x)]<-x[x>mean(x)]-mean(x)
hist(x)
x<-c(1,2,3,10,32)

x[x>mean(x)]-mean(x)

if(x>mean(x)){
  x<-x+1
}


max(page_rank(g)$vector)

page_rank(g)$vector[2:5]/sum(page_rank(g)$vector[1:3])

g<-make_tree(100)
plot(g)
h<-make_tree(100)
x<-g %u% h
plot(x)
hist(page_rank(x)$vector)

net1<-graph_from_literal(A-+B,A-+C,B-+D:E:F,C-+G:H)
net2 <- graph_from_literal(1-+2:3:4,3-+B,E-+1)
plot(net1)
plot(net2)
x<-net1 %u% net2
x
page_rank(x)
plot(x)
hist(page_rank(x)$vector)
sort(page_rank(x)$vector)



W<-100
x<-c(100,10,50,20)
x<-x/sum(x)
x*W
x<- c(0.55555556 +0.05555556, 0.27777778 +0.11111111)
x*W

net1<-graph_from_literal(A-+B:C)
net2 <- graph_from_literal(D-+C:E:F)
plot(net1)
plot(net2)
x<-net1 %u% net2

net1<-graph_from_literal(A-+B,B-+C,C-+A, C-+D)
plot(net1)
page_rank(net1)$vector
page_rank(net1,personalized=c(0,0,0,0.25))$vector

pl