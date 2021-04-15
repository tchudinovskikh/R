Gr <- matrix(c(0,18,0,0,0,0,0,0,23,25,0,
               18,0,40,30,0,0,0,0,15,0,0,
               0,40,0,12,32,0,0,0,0,0,0,
               0,30,12,0,35,21,17,14,0,0,0,
               0,0,32,35,0,13,0,0,0,0,0,
               0,0,0,21,13,0,20,0,0,0,18,
               0,0,0,17,0,20,0,15,0,38,30,
               0,0,0,14,0,0,15,0,16,20,0,
               23,15,0,0,0,0,0,16,0,13,0,
               25,0,0,0,0,0,38,20,13,0,0,
               0,0,0,0,0,18,30,0,0,0,0),
             nrow=11, ncol=11, byrow=TRUE)

add <- function(G,v1,v2,w=1){
  if(v1 > nrow(G) | v2 > ncol(G)){
    return(G)
  }else{
    G[v1,v2] <- w
    G[v2,v1] <- w
    return(G)
  }
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
prima <- function(G){
  r <- nrow(G)
  for(i in 1:r){
    if(sum(G[i,])==0){
      return("impossible")
    }
  }
  Gr <- matrix(0:0,nrow=r,ncol=r)
  res <- c(1)
  while(length(res)!=r){
    min <- Inf
    for(i in res){
      for(j in link(G,i)){
        if(G[i,j]<min && all(res!=j)){
          min <- G[i,j]
          v2 <- j
          v1 <- i
        }
      }
    }
    Gr <- add(Gr,v1,v2,min)
    res <- c(res,v2)
  }
  return(Gr)
}
print(prima(Gr))