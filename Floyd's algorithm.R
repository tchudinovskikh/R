Gr <- matrix(c(0,18,0,0,0,0,0,0,23,25,0,
               0,0,40,30,0,0,0,0,15,0,0,
               0,40,0,0,32,0,0,0,0,0,0,
               0,0,12,0,35,21,17,14,0,0,0,
               0,0,0,35,0,13,0,0,0,0,0,
               0,0,0,21,0,0,20,0,0,0,18,
               0,0,0,17,0,20,0,15,0,38,30,
               0,0,0,14,0,0,15,0,16,20,0,
               23,15,0,0,0,0,0,16,0,13,0,
               25,0,0,0,0,0,38,20,13,0,0,
               0,0,0,0,0,0,30,0,0,0,0),
             nrow=11, ncol=11, byrow=TRUE)
Gra <- Gr
for(i in 1:nrow(Gr)){
  for(j in 1:ncol(Gr)){
    if(Gr[i,j]==0 && i!=j){
      Gra[i,j] <- Inf
    }
  }
}

floyd <- function(G){
  D <- G
  n <- ncol(Gr)
  for(i in 1:n){
    for(u in 1:n){
      for(v in 1:n){
        D[u,v] <- min(D[u,v], D[u,i]+D[i,v])
      }
    }
  }
  return(D)
}

floyd_min <- function(G){
  mat <- floyd(G)
  mini <- Inf
  result <- c()
  for(i in 1:nrow(mat)){
    if(sum(mat[i,])<mini){
      mini <- sum(mat[i,])
      result <- mat[i,]
    }
  }
  return(result)
}
link <- function(G,v){
  if(v > nrow(G)){
    return("no this vertex")
  }
  c <- c()
  for(i in 1:ncol(G)){
    if(G[v,i] != 0){
      c <- c(c,i)
    }
  }
  return(c)
}
tree <- function(v,G){
  res <- rep(0,length(v))
  s <- which(v==0)
  res[s] <- s
  while(any(res==0)){
    d <- which(res!=0)
    for(i in d){
      a <- link(G,i)
      for(j in a){
        if(v[j]==G[i,j]+v[i] | v[j]==G[j,i]+v[i]){
          res[j] <- i
        }
      }
    }
  }
  return(res)
}
print(tree(floyd_min(Gra), Gra))
print(floyd_min(Gra))