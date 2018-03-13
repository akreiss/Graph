################
##### TODO #####
################

### test

####### ARE EIGEN VECTORS CONVERGING ?
### try to find best matching eigen vectors
### try with more vertices / less edges ?
### more bootstrap replications ?

####### ESTIMATION
### Try spectral cut off, is quality of estimation better ?

####### PREDICTION ?
### If we have association between low and high dimension eigen vectors, try prediction ?




library(igraph)

## Create two samples of block model of different size
A=matrix(c(0.8,0.1,0.1,0.5),ncol=2)

## large
N=500
blocks=c(floor(N/2), ceiling(N/2))
G_large=sample_sbm(N,A,blocks)

# plot(G_large,main="Large")

## Find Adjacency Matrices
laplace_large=laplacian_matrix(G_large)

## Find eigenvalues
E_large=eigen(laplace_large,symmetric=TRUE)

## coordinates function of interest
ind <- 1:N
H <- ind^(-3)

## compute the function of interest itself
h <- E_large$vectors%*%H
V(G_large)$h=h

cat("First Fourier Coefficient of f_small: ",sum(f_small*E_small$vectors[,1]),"\n")
cat("Second Fourier Coefficient of f_small: ",sum(f_small*E_small$vectors[,2]),"\n")





########################
##### sub-sampling #####
########################
boot <- 10
Err <- matrix(nrow=boot, ncol=50, 0)
corr <- list()
for(i in 1:50){
  for(j in 1:boot){
    # sampling the graph
    n <- floor((N/50)*i)
    subInd <- sort(sample(x=(1:N), size=n))
    G_small <- induced_subgraph(G_large, subInd, impl = c("auto"))
    
    #G_small_noise <- G_small
    #V(G_small_noise)$h <- V(G_small_noise)$h + rnorm(mean=0, sd=0.1, length(V(G_small_noise)$h))
    
    # representing the sub sampled graph
    # dev.new()
    # plot(G_small,main="Small")
    
    # laplacian of the subsampled graph
    laplace_small <- laplacian_matrix(G_small)
    
    # eigen vectors and values of the graph
    E_small <- matrix(ncol=N, nrow=N, 0)
    E_small[subInd,((N-n+1):N)] <- eigen(laplace_small,symmetric=TRUE)$vectors
    
    col <- (t(E_small)%*%E_large$vectors)^2
    match <- matrix(ncol=2, nrow=n, 0)
    match <- apply(col, 1, which.max)
    col <- apply(col, 2, sort)
    name <- paste(i, j, sep="_")
    corr[[name]] <- col[c((N-1),N),]
    permut <- E_large$vectors[,match]
    Err[j,i] <- norm((permut-E_small))
  }
}

test <- matrix(ncol=)

E <- apply(Err, 2, mean)
plot(E, type ="l")
write_graph(G_small,"small","graphml")
write_graph(G_large,"large","graphml")