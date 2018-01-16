#Delivery 2, Exercise 5 BLOCK 2: Daniel Salgado Rojo

#install.packages("gRain")
#source("http://bioconductor.org/biocLite.R")
#biocLite("RBGL")
#biocLite("Rgraphviz")
#install.packages("Rcpp")
library(gRain)
library(gRbase)
library(Rgraphviz)
library(RBGL)
par(mfrow = c(1,1))

tf <- c("true","false")

# Specify the CPTs:
node.M <- cptable(~ M, values = c(2, 8), levels = tf)
node.S <- cptable(~ S|M, values = c(8, 2,2,8), levels = tf)
node.B <- cptable(~ B|M, values = c(2, 8, 5, 95 ), levels = tf)
node.C <- cptable(~ C|S+B , values = c(8, 2, 8, 2,8,2,5,95 ), levels = tf)
node.H <- cptable(~ H|B, values = c(8, 2, 6, 4 ), levels = tf)

# Create an intermediate representation of the CPTs:
nodes <- list(node.M,node.S,node.B,node.C, node.H)
plist <- compileCPT(nodes)


# Create a network of name "Norman.net", for instance:
BN<-grain(plist)
summary(BN)
# The graph:
BN.plot=plot(BN)
BN.plot


#################
# Subexercise (a)
#################
  
  #Compute P(M=T/H=F) and P(M=F/H=T)
  BNa1<-setEvidence(BN,nodes=c("H"),
                   states=c("false"))
  marg1=querygrain(BNa1,nodes
                  =c("M"), type="marginal")
  
  #P(M=T/H=F)= 0.1875
  marg1$M["true"]
  
  
  BNa2<-setEvidence(BN,nodes=c("H"),
                    states=c("true"))
  marg2=querygrain(BNa2,nodes
                  =c("M"), type="marginal")
  
  #P(M=F/H=T)= 0.7922078
  marg2$M["false"]

  
#################
# Subexercise (b)
#################
  binary_random <- function(p.prior.true){
    seed <- runif(1)
    if(seed < p.prior.true){return("true");}
    else{return("false");}
  }
  
  run_iterations = 10^6
  
  #LS Algorithm for a single evidence and the particular
  #BN from the current exercise.
  LS_alg <- function(pred.node = "M", 
                     evid.node = "H",
                     pred.value = "true", 
                     evid.value = "false",
                     iterations = run_iterations){
    
    count.pred_evid = 0; count.evid = 0;
    
    iter_counter = 0
    while(iter_counter < iterations){
      
      #Value assigntion
      M.value <- binary_random(plist$"M"[["true"]])
      S.value <- binary_random(plist$"S"[ "true" , M.value])
      B.value <- binary_random(plist$"B"[ "true" , M.value])
      C.value <- binary_random(plist$"C"[,, B.value]["true", S.value])
      H.value <- binary_random(plist$"H"["true", B.value])
      
      #Update counters
      node_value <- function(node = "M"){
        if(node == "M"){return(M.value)}
        else if(node == "S"){return(S.value)}
        else if(node == "B"){return(B.value)}
        else if(node == "C"){return(C.value)}
        else if(node == "H"){return(H.value)}
        else{print("Node not found");return(NA);}
      }
      
      e_observed <- node_value(evid.node)
      x_observed <- node_value(pred.node)
      
      if(e_observed == evid.value){ 
        count.evid = count.evid +1;
        if(x_observed == pred.value){
          count.pred_evid = count.pred_evid +1}
        }
      iter_counter = iter_counter +1;
    }#main simulation while
  
    return(count.pred_evid/count.evid)
  }#end function
  
  #P(M=T/H=F)
  estimated_lsprob1 <- LS_alg("M","H","true", "false")
  #P(M=F/H=T)
  estimated_lsprob2 <- LS_alg("M","H","false", "true")
  
  
#################
# Subexercise (c)
#################  
  
  #LW Algorithm for a single evidence and the particular
  #BN from the current exercise.
  LW_alg <- function(pred.node = "M", 
                     evid.node = "H",
                     pred.value = "true", 
                     evid.value = "false",
                     iterations = run_iterations
                     ){
    
    count.pred_evid = 0; count.evid = 0;
    likelihood.e <-0
    
    iter_counter = 0
    while(iter_counter < iterations){
      
      #Value assigntion root nodes (M)
      if("M" == evid.node){
        M.value <- evid.value
        likelihood.e <- plist$"M"[[evid.value]]
      }
      else{
        M.value <- binary_random(plist$"M"[["true"]])
      }
      #Value assignation for childs
      #S
      if("S" == evid.node){
        S.value <- evid.value
        likelihood.e <- plist$"S"[ "true" , M.value]
      } 
      else{
        S.value <- binary_random(plist$"S"[ "true" , M.value])
      }
      #B
      if("B" == evid.node){
        B.value <- evid.value
        likelihood.e <- plist$"B"[ "true" , M.value]
      }
      else{
        B.value <- binary_random(plist$"B"[ "true" , M.value])
      }
      #C
      if("C" == evid.node){
        C.value <- evid.value
        likelihood.e <- plist$"C"[,, B.value]["true", S.value]
      }
      else{
        C.value <- binary_random(plist$"C"[,, B.value]["true", S.value])
      }
      #H
      if("H" == evid.node){
        H.value <- evid.value
        likelihood.e <- plist$"H"["true", B.value]
      }
      else{
        H.value <- binary_random(plist$"H"["true", B.value])
      }
      
      
      #Update counters
      node_value <- function(node = "M"){
        if(node == "M"){return(M.value)}
        else if(node == "S"){return(S.value)}
        else if(node == "B"){return(B.value)}
        else if(node == "C"){return(C.value)}
        else if(node == "H"){return(H.value)}
        else{print("Node not found");return(NA);}
      }
      
      e_observed <- node_value(evid.node)
      x_observed <- node_value(pred.node)
      
      if(e_observed == evid.value){ 
        count.evid = count.evid + likelihood.e;
        if(x_observed == pred.value){
          count.pred_evid = count.pred_evid +likelihood.e}
      }
      iter_counter = iter_counter +1;
    }#main simulation while
    
    return(count.pred_evid/count.evid)
  }#end function
  
  
  #P(M=T/H=F)
  estimated_lwprob1 <- LW_alg("M","H","true", "false")
  #P(M=F/H=T)
  estimated_lwprob2 <- LW_alg("M","H","false", "true")
  

#################
# Subexercise (d)
#################
  #Compute P(M=T/H=F) by hand (see the pdf delviered)
  # the result obtained is: P(M=T/H=F) = 0.1875
#################
# Subexercise (e)
#################  
  # For evidence H=F and the query variable M, compute 
  # the Kullback-Leibler divergence for the LS Algorithm 
  # and also for the LW algorithm. Compare them.
  
  #Which algorithm seems to be better?
  # this question is answered after a plot for fr KL
  # divergences is generated.
  
  p_exact <- marg1$M[["true"]]
  #KL divergence for the LS algorithm
  KL_LS <- function(p_exact = marg1$M[["true"]],
                    iter = 10^4){
    p_ls <- LS_alg("M","H","true", "false",iter)
    kl <- p_exact * log(p_exact/p_ls) +   (1-p_exact)*log((1-p_exact)/(1-p_ls))
    if(kl <0){print("Negative KL_LS!!!")}
    return(kl)
  }
  #KL divergence for the LS algorithm
  KL_LW <- function(p_exact = marg1$M[["true"]],
                    iter = 10^4){
    p_lw <- LW_alg("M","H","true", "false", iter)
    kl <- p_exact * log(p_exact/p_lw) +   (1-p_exact)*log((1-p_exact)/(1-p_lw))
    if(kl <0){print("Negative KL_LW!!!")}
    return(kl)
  }
  
iter.vector <- c(10^2, 2*10^2, 3*10^2, 5*10^2, 10^3,4*10^3,10^4)
kls.vector <- numeric(length(iter.vector))
klw.vector <- numeric(length(iter.vector))
# for(i in 1:length(iter.vector)){
#   kls.vector[i] <- KL_LS(iter = iter.vector[i])
#   klw.vector[i] <- KL_LW(iter = iter.vector[i])
# }
reps = 10
kls.matrix <- matrix(data = 0, nrow = length(iter.vector)
                     , ncol = reps)
klw.matrix <- matrix(data = 0, nrow = length(iter.vector)
                     , ncol = reps)

for(i in 1:length(iter.vector)){
    for(j in 1:reps){
      kls.matrix[i,j] <- KL_LS(iter = iter.vector[i])
      klw.matrix[i,j] <- KL_LW(iter = iter.vector[i])
    }
    kls.vector[i] <- mean(kls.matrix[i,])
    klw.vector[i] <- mean(klw.matrix[i,])
  }

dev.off()
par(mfrow = c(1,1))

plot(log10(iter.vector), kls.vector, 
     ylim  = c(0, max( c(kls.vector, klw.vector)) ) ,
     xlab = "log10(Iterations)",
     ylab = "KL Divergence",
     main = "KL( P(M/H = F), P'(M/H = F) )",
     col = "blue")
points(log10(iter.vector), klw.vector, col = "red")

abline(h=0, col = "black")
legend("topright",
       c("LS", "LW"), col = c( "blue", "red"),
       bty = 'n',
       pch = 21,
       cex = 1)
  
  
#ANSWER/Discussion:
  # * For small number of iterations (<10^3) the LW method
  # seems to converge earlier to the exact solution.
  
  # * However, for larger number of iterations (>10^3) the
  # LS method seems to be closer (smaller divergence) to the
  # exact solution.

  # * Thus, we would say that the LW algorithm converges
  # faster if the number of iterations is relatively small,
  # but the LS method seems to approach more to the exact
  # solution as the number of interations increases.
  
#####################################  
  
# Estimated probabilities, sampling
reps = 10;

p1ls.vector <- numeric(length(iter.vector))
p2ls.vector <- numeric(length(iter.vector))

p1lw.vector <- numeric(length(iter.vector))
p2lw.vector <- numeric(length(iter.vector))

p1ls.matrix <- matrix(data = 0, nrow = length(iter.vector)
                     , ncol = reps)
p2ls.matrix <- matrix(data = 0, nrow = length(iter.vector)
                     , ncol = reps)

p1lw.matrix <- matrix(data = 0, nrow = length(iter.vector)
                      , ncol = reps)
p2lw.matrix <- matrix(data = 0, nrow = length(iter.vector)
                      , ncol = reps)

for(i in 1:length(iter.vector)){
  for(j in 1:reps){
    iter = iter.vector[i]
    p1ls.matrix[i,j] <- LS_alg("M","H","true", "false", iter)
    p2ls.matrix[i,j] <- LS_alg("M","H","false", "true", iter)
    p1lw.matrix[i,j] <- LW_alg("M","H","true", "false", iter)
    p2lw.matrix[i,j] <- LW_alg("M","H","false", "true", iter)
  }
  p1ls.vector[i] <- mean(p1ls.matrix[i,])
  p2ls.vector[i] <- mean(p2ls.matrix[i,])
  
  p1lw.vector[i] <- mean(p1lw.matrix[i,])
  p2lw.vector[i] <- mean(p2lw.matrix[i,])
  
}

#dev.off()
par(mfrow = c(1,2))
# p1 P(M=T/H=F)
p1_exact = 0.1875
plot(log10(iter.vector), p1ls.vector,
     ylim = c(min(p1ls.vector, p1lw.vector), max(p1ls.vector, p1lw.vector)),
     col = "blue",
     xlab = "log10(Iterations)",
     ylab = "Average estimated probability",
     main = "P(M=T/H=F)")
points(log10(iter.vector), p1lw.vector, col = "red")

abline(h=p1_exact, col = "black")

legend("topright",
       c("LS", "LW"), col = c( "blue", "red"),
       bty = 'n',
       pch = 21,
       cex = 0.6)

# p2 P(M=F/H=T)

plot(log10(iter.vector), p2ls.vector,
     ylim = c(min(p2ls.vector, p2lw.vector), max(p2ls.vector, p2lw.vector)),
     col = "blue",
     xlab = "log10(Iterations)",
     ylab = "Average estimated probability",
     main = "P(M=F/H=T)")
points(log10(iter.vector), p2lw.vector, col = "red")

abline(h=0.7922078, col = "black")

legend("bottomright",
       c("LS", "LW"), col = c( "blue", "red"),
       bty = 'n',
       pch = 21,
       cex = 0.6)




######################################
  
  
####################################################

  ##AT THE BEGINNING I STARTED TRYING TO CODE A GENERAL ALGORITHM:
  
  # LS_alg <- function(plist = plist,
  #                    nonroot.nodes = c("S","B","C","H"),
  #                    root.nodes = c("M"),
  #                    pred.node = "M", 
  #                    evid.node = "H",
  #                    pred.value = "true", 
  #                    evid,value = "false",
  #                    iterations = 10){
  #   
  #   count.pred_evid = 0; count.evid = 0;
  #   root.values <- numeric(length(root.nodes))
  #   nonroot.values <- numeric(length(nonroot.nodes))
  #   
  #   iter_counter = 0
  #   while(iter_counter < iterations){
  #     
  #     for( root in root.nodes ){
  #       index = 1
  #       root.values[index] <- binary_random(plist$root[["true"]])
  #       index = index+1
  #     }
  #     
  #     for( nonroot in nonroot.nodes){
  #       index = 1
  #       nonroot.values[index] <- binary_random(plist$root)
  #       
  #       index = index+1
  #       
  #     }
  #     
  #     
  #     
  #     
  #     
  #     
  #       
  #       
  #       iter_counter = iter_counter +1;
  #   }#main simulation while
  #   
  # }#end function
  # 