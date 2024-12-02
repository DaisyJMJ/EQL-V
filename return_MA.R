# The function of getting margins

return_MA <- function(Y, ncolY, Z, S, xMAs, MAs) {
  # Initialize lists to store xMAs and MAs
  
  
  # x_ma = apply(Y[,1:ncolY],2, function(x) sort(unique(x)))    # use lappy to make sure x_ma is always a list
  x_ma <- lapply(1:ncolY, function(i) sort(unique(Y[, i])))
  # #cut points
  # cuts_MA<- lapply(1:ncolY, function(x)
  #      sapply(x_ma[[x]], function(y)
  #          max(Z[which(Y[,x] == y),x])))
  
  #cut points are: Z_j^q
  cuts_MA<- lapply(1:ncolY, function(x)
    sapply(x_ma[[x]], function(y)
      max(Z[which(Y[,x] == y),x])))
  
  # MA_j<- sapply(1:ncolY,function(j) pnorm(cuts_MA[[j]],sd = sqrt(S[j,j])))
  MA_j <- lapply(1:ncolY, function(j) pnorm(cuts_MA[[j]], sd = sqrt(S[j, j])))  # lapply forces to be a list
  
  
  # store x_MA
  
  for(j in 1:ncolY){
    
    # 强行macth
    if (is.null(xMAs[[j]]) || ncol(xMAs[[j]]) == length(x_ma[[j]])) {    # Here is the problem
      # Combine the current vector with the matrix
      xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
    }
    else if (ncol(xMAs[[j]]) > length(x_ma[[j]])){
      x_ma[[j]] = c(x_ma[[j]], rep(NA, ncol(xMAs[[j]]) -length(x_ma[[j]])))
      xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
    }
    else
    {
      extra_cols <-length(x_ma[[j]]) - ncol(xMAs[[j]])
      xMAs[[j]] <- cbind(xMAs[[j]], matrix(NA, nrow = nrow(xMAs[[j]]), ncol = extra_cols))
      xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
    }
  }       
  
  
  for(j in 1:ncolY){
   
    # 强行macth
    if (is.null(MAs[[j]]) || ncol(MAs[[j]]) == length(MA_j[[j]])) {    # Here is the problem
      # Combine the current vector with the matrix
      MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
    }
    else if (ncol(MAs[[j]]) > length(MA_j[[j]])){
      MA_j[[j]] = c(MA_j[[j]], rep(NA, ncol(MAs[[j]]) -length(MA_j[[j]])))
      MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
    }
    else{
      extra_cols <-length(MA_j[[j]]) - ncol(MAs[[j]])
      MAs[[j]] <- cbind(MAs[[j]], matrix(NA, nrow = nrow(MAs[[j]]), ncol = extra_cols))
      MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
    }
  }       
  
  # Return the result as a list
  return(list(xMAs = xMAs, MAs = MAs, x_ma = x_ma, MA_j = MA_j))
}




##############
# if (ns >= burn ) {
#   S<-tcrossprod(Lambda) + diag(Sigma.diag)
#   
#   C <- S/(sqrt(diag(S)) %*% t(sqrt(diag(S))))
#   
#   C.post[, , ns - burn ] = C
#   alpha.post[,ns-burn] = alpha/sqrt(diag(S))
#   Y_copy = YR[,1:ncolY]
#   # compute the margin adjusmtent
#   x_ma =apply(Y[,1:ncolY],2, function(x) sort(unique(x)))
#   
#   # if (!identical(class(x_ma), "list")){
#   # x_ma = split(x_ma, col(x_ma))
#   # }
#   #cut points
#   cuts_MA<- lapply(1:ncolY, function(x)
#     sapply(x_ma[[x]], function(y)
#       max(Z[which(Y[,x] == y),x])))
#   
#   MA_j<- sapply(1:ncolY,function(j) pnorm(cuts_MA[[j]],sd = sqrt(S[j,j])))
#   
#   # if (!identical(class(MA_j), "list")){
#   #   MA_j = split(MA_j, col(MA_j))
#   # }
#   
#   # for (k in i:ncolY)
#   # {
#   # cat("Y:\n")
#   # print(Y)
#   #
#   # if (length(x_ma[[k]]) != length(MA_j[[k]]))
#   # {
#   # print("Here is the error")
#   # cat("x_ma:\n")
#   # print(x_ma)
#   # cat("class: \n")
#   # print(class(x_ma))
#   #
#   
#   # cat("cuts_MA:\n")
#   # print(cuts_MA)
#   #
#   # cat("MA_j:\n")
#   # print(MA_j)
#   
#   # cat("MAs:\n")
#   # print(MAs)
#   # cat("class: \n")
#   # print(class(MA_j))
#   
#   # MA_j = as.list(MA_j)
#   # TODO: is it useful? MAs              ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   # }
#   # 
#   
#   # store x_MA
#   
#   for(j in 1:ncolY){
#     
#     # print("j:")
#     # print(j)
#     # print("ncol(xMAs[[j]]):")
#     # print(ncol(xMAs[[j]]))
#     # 
#     # print("length(x_ma[[j]]):")
#     # print(length(x_ma[[j]]))
#     
#     # 强行macth
#     if (is.null(xMAs[[j]]) || ncol(xMAs[[j]]) == length(x_ma[[j]])) {    # Here is the problem
#       # Combine the current vector with the matrix
#       # print("Here 1")
#       xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
#       # print("step 1")
#     }
#     else if (ncol(xMAs[[j]]) > length(x_ma[[j]])){
#       # print("Here 2")
#       x_ma[[j]] = c(x_ma[[j]], rep(NA, ncol(xMAs[[j]]) -length(x_ma[[j]])))
#       xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
#       # print("step 2")
#     }
#     else
#     {
#       # print("Here 3")
#       extra_cols <-length(x_ma[[j]]) - ncol(xMAs[[j]])
#       xMAs[[j]] <- cbind(xMAs[[j]], matrix(NA, nrow = nrow(xMAs[[j]]), ncol = extra_cols))
#       xMAs[[j]] <- rbind(xMAs[[j]], x_ma[[j]])
#       # print("step 3")
#     }
#   }       
#   
#   
#   
#   for(j in 1:ncolY){
#     # print("j:")
#     # print(j)
#     # 
#     # # print("MAs[[j]]")
#     # # print(MAs[[j]])
#     # # print("MA_j[[j]]")
#     # # print(MA_j[[j]])
#     # 
#     # print("ncol(MAs[[j]]):")
#     # print(ncol(MAs[[j]]))
#     # 
#     # print("length(MA_j[[j]]):")
#     # print(length(MA_j[[j]]))
#     # 强行macth
#     if (is.null(MAs[[j]]) || ncol(MAs[[j]]) == length(MA_j[[j]])) {    # Here is the problem
#       # Combine the current vector with the matrix
#       # print("Here 4")
#       MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
#       # print("step 4")
#     }
#     else if (ncol(MAs[[j]]) > length(MA_j[[j]])){
#       # print("Here 5")
#       MA_j[[j]] = c(MA_j[[j]], rep(NA, ncol(MAs[[j]]) -length(MA_j[[j]])))
#       MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
#       # print("step 5")
#     }
#     else{
#       # print("Here 6")
#       extra_cols <-length(MA_j[[j]]) - ncol(MAs[[j]])
#       MAs[[j]] <- cbind(MAs[[j]], matrix(NA, nrow = nrow(MAs[[j]]), ncol = extra_cols))
#       MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
#       # print("step 6")
#     }
#   }       
#   
#   
#   # for(j in 1:ncolY){
#   # 
#   #   # 强行macth
#   #   if (is.null(MAs[[j]]) || ncol(MAs[[j]]) == length(MA_j[[j]])) {    # Here is the problem
#   #     # Combine the current vector with the matrix
#   #     MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
#   #   }
#   # }
#   
#   lbs = sapply(1:ncolY, function(j) aux_infos[[j]][1])  # TODO: is it useful?
#   ubs = sapply(1:ncolY, function(j) aux_infos[[j]][length(aux_infos[[j]])])   # TODO: is it useful?
#   
#   if((ns-burn)%%imp_every == 0){ # imputation
#     
#     for (i in 1:ncolY){
#       print("i:")
#       print(i)
#       
#       print("length(c(MA_j[[i]]))")
#       print(length(c(MA_j[[i]])))
#       print("MA_j[[i]]")
#       print(MA_j[[i]])
#       print("c(MA_j[[i]])")
#       print(c(MA_j[[i]]))
#       
#       print("x_ma[[i]]")
#       print(x_ma[[i]])
#       print("c(x_ma[[i]])")
#       print(c(x_ma[[i]]))
#       print("length(c(x_ma[[i]]))")
#       print(length(c(x_ma[[i]])))
#     }
#     
#     # Modified TODO with unlist
#     Fn_inv = sapply(1:ncolY, function(j)
#       splinefun(c(MA_j[[j]]),c(x_ma[[j]]), method = "monoH.FC"))
#     
#     # print("pass")
#     for(j in 1:ncolY){
#       
#       na_inds = which(is.na(YR[,j]))
#       
#       Y_mis<- Fn_inv[[j]](pnorm(Z[na_inds,j], sd = sqrt(S[j,j])))
#       
#       
#       Y_copy[na_inds,j] = Y_mis
#       
#     }
#     Completed_Data[[m]] = Y_copy
#     m = m+1
#   }
# }