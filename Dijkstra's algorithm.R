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
link2 <- function(G,v){
  if(v > nrow(G)){
    return("no this vertex")
  }
  c <- c()
  for(i in 1:ncol(G)){
    if(G[i,v] != 0){
      c <- c(c,i)
    }
  }
  return(c)
}

deixtra <- function(G,v){
  l <- nrow(G)
  v1 <- rep(Inf,l)
  v2 <- rep(0,l)
  v3 <- rep(1,l) 
  v1[v] <- 0
  v2[v] <- v
  for(i in 1:l){
    if(G[v,i]!=0){
      v1[i] <- G[v,i]
      v2[i] <- v
    }
  }
  v3[v] <- 0
  a <- v
  while(sum(v3)!=0){
    v6 <- which(v3==1)
    v4 <- link(G,a) 
    v5 <- c()
    for(i in 1:length(v6)){
      if(any(v4==v6[i]))
        v5 <- c(v5, v6[i])
    }
    for(i in v5){
      v1[i] <- min(v1[i], v1[a]+G[a,i])
    }
    m <- Inf
    k <- 0
    for(i in v6){
      if(v1[i]<m){
        m <- v1[i]
        k <- i
      }
    }
    v7 <- which(v3==0)
    v8 <- link2(G,k)
    v9 <- c()
    for(i in 1:length(v7)){
      if(any(v8==v7[i]))
        v9 <- c(v9, v7[i])
    }
    for(i in v9){
      if(v1[k]==v1[i]+Gr[i,k]){
        v2[k] <- i
      }
    }
    v3[k] <- 0
    a <- k
  }
  return(list(v1,v2))
}

deixtra_min <- function(G){
  mini <- Inf
  for(i in 1:nrow(G)){
    if(sum(deixtra(G,i)[[1]])<mini){
      mini <- sum(deixtra(G,i)[[1]])
      result <- deixtra(G,i)
    }
  }
  return(result)
}  
print(deixtra_min(Gr))