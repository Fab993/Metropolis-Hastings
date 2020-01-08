#---------------------------------------------------------------#
#            Simple Metropolist-Hastings Algorithm              #
#                for island-hopping problem                     #
#                   Fabrizio Leone (2019)                       #          
#---------------------------------------------------------------#


# Initialization
Nburn             <- 3000                   # Burn in replication
Nrep              <- 2000                   # Actual replication
Ntot              <- Nburn + Nrep           # Total replication
N                 <- 7                      # No. of islands
Idx               <- seq(1,N,1)       
Pop               <- sqrt(N^Idx)            # Pop. per island. It is the target 
End               <- matrix(NaN,Nrep)
Start             <- matrix(NaN,Nrep)
Start[1]          <- 5

# Metropolis-Hastings algorithm
for (i in 1:Ntot) {
  
  if(Start[i]>1 & Start[i]<N){
    Proposal          <- runif(1)          # Proposal move
  } else if (Start[i] == N){
    Proposal          <- 1
  } else {
    Proposal          <- 0
  }
  
  if (Proposal<1/2){                       # If proposal < 0.5 ==> Move right
    r                <- min(Pop[Start[i]+1]/Pop[Start[i]],1)   
    if (runif(1)<r){
      End[i]              <- Start[i] + 1
    } else {
      End[i]              <- Start[i] 
    }  
  } else {                                 # If proposal > 0.5 ==> Move left
    r                <- min(Pop[Start[i]-1]/Pop[Start[i]],1)
    if (runif(1)<r){
      End[i]              <- Start[i] - 1
    } else {
      End[i]              <- Start[i] 
    }
  }
  Start[i+1]         <- End[i]             # Start iteration t+1 from endpoint of t
  
}

End                  <- End[Nburn:Ntot]    # Discard burn-in

# Plot results
par(mfrow=c(1,2))
plot(Pop, xlab = "Island", ylab = "True Population",main="Metropolis - Hastings" , col = "blue", type="h")
hist(End, xlab = "Island", ylab = "Simulated Population", main="", col = "red")
