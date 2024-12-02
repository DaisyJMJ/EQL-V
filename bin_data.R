
bin_data <- function(YR, ncolY, aux_quantiles = NULL, aux_infos =NULL, MA = F) {
  # F_invs = vector('list', ncolY) # interpolated cdf
  aux_bins = vector('list', ncolY) # get aux bin membership
  Y_binned = NULL
  
  for (j in 1:ncolY) {
    Y_j = YR[, j]
    
    if (is.null(aux_quantiles[[j]])) {
      # if there is no auxiliary information, use empirical deciles quantile for EQL
      auxq = seq(0, 1, by = 0.10)
      
      if (MA[j]) {
        aux_info = sort(unique(c(seq(min(Y_j, na.rm = T), max(Y_j, na.rm = T), length.out = 15),
                                 quantile(YR[, j], seq(0, 1, by = 0.10), type = 1,na.rm = T))))
        # Bin data for EQL/EHQL
        Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
      } else {
        aux_info = quantile(YR[, j], auxq, type = 1,na.rm = TRUE)
        # Bin data for EQL/EHQL
        Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
      }
      
      aux_bins[[j]] = findInterval(Y_j, quantile(YR[, j], auxq, type = 1,na.rm = TRUE), left.open = T) + 1 # get auxiliary bin membership
      aux_quantiles[[j]] = auxq # save aux quantiles
      # F_invs[[j]] = splinefun(seq(0, 1, by = 0.5), auxq) # save interpolated ecdf
      
    } 
    else {
      auxq = aux_quantiles[[j]]
      a_info = aux_infos[[j]]  # quantile_draw is called outside

      # Here assume auxiliary information is not fixed, random sample generated
      # a_info = asymptotic_quantile(Y_j, auxq, n_generated = 1)$Generated_Quantiles   #!!!!!! Modified
      
      #Modified:
      # for simulation purpose, use the whole complete dataset
      # a_info = asymptotic_quantile(Y_full[j], auxq, n_generated = 1)$Generated_Quantiles 
      
      # print(unlist(a_info))
      if (MA[j]) {
        aux_info = sort(unique(c(seq(min(Y_j, na.rm = T), max(Y_j, na.rm = T), length.out = 15), unlist(a_info) )))
        # Bin data for EQL/EHQL
        # Y_j_binned is essentially the left/right bound of the aux_info interval Y_j falls into
        Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
      } else {
        # aux_info = quantile(YR[, j], auxq, type = 1)
        # Bin data for EQL/EHQL
        Y_j_binned = a_info[findInterval(Y_j, unlist(a_info), left.open = T) + 1]  
      }
      
      # the number of intervals ( e.g., the six smallest interval)  # unlist(a.info)
      aux_bins[[j]] = findInterval(Y_j, a_info, left.open = T) + 1 # get auxiliary bin membership 
      # print("inside function")
      # print(aux_bins[[j]])
      # if (any(aux_bins[[j]] == 1, na.rm = TRUE)) {
      # 
      #   print("j")
      #   print(j)
      # 
      #   print("a_info")
      #   print(a_info)
      # 
      # 
      #   print("not correct")
      # }
      # TODO: use aux_info (has margin adjustments) for binning?
    }
    
    Y_binned = cbind(Y_binned, Y_j_binned) # inefficient computing
  }
  
  return(list(Y_binned = Y_binned, aux_bins = aux_bins, aux_quantiles = aux_quantiles))
}


#### Below is the first version, not functional

# bin_data <- function(YR, ncolY, aux_quantiles, aux_infos, MA) {
#   F_invs = vector('list', ncolY) # interpolated cdf
#   aux_bins = vector('list', ncolY) # get aux bin membership
#   Y_binned = NULL
#   
#   for (j in 1:ncolY) {
#     Y_j = YR[, j]
#     
#     if (is.null(aux_quantiles[[j]])) {
#       # if there is no auxiliary information, use empirical deciles quantile for EQL
#       auxq = seq(0, 1, by = 0.10)
#       
#       if (MA[j]) {
#         aux_info = sort(unique(c(seq(min(Y_j, na.rm = T), max(Y_j, na.rm = T), length.out = 15),
#                                  quantile(YR[, j], seq(0, 1, by = 0.10), type = 1))))
#         # Bin data for EQL/EHQL
#         Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
#       } else {
#         aux_info = quantile(YR[, j], auxq, type = 1)
#         # Bin data for EQL/EHQL
#         Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
#       }
#       
#       aux_bins[[j]] = findInterval(Y_j, quantile(YR[, j], auxq, type = 1), left.open = T) + 1 # get auxiliary bin membership
#       aux_quantiles[[j]] = auxq # save aux quantiles
#       # F_invs[[j]] = splinefun(seq(0, 1, by = 0.5), auxq) # save interpolated ecdf
#       
#     } else {
#       auxq = aux_quantiles[[j]]
#       a_info = aux_infos[[j]]
#       
#       if (MA[j]) {
#         aux_info = sort(unique(c(seq(min(Y_j, na.rm = T), max(Y_j, na.rm = T), length.out = 15), a_info)))
#         # Bin data for EQL/EHQL
#         Y_j_binned = aux_info[findInterval(Y_j, aux_info, left.open = T) + 1]
#       } else {
#         # aux_info = quantile(YR[, j], auxq, type = 1)
#         # Bin data for EQL/EHQL
#         Y_j_binned = aux_info[findInterval(Y_j, a_info, left.open = T) + 1]
#       }
#       
#       aux_bins[[j]] = findInterval(Y_j, a_info, left.open = T) + 1 # get auxiliary bin membership
#     }
#     
#     Y_binned = cbind(Y_binned, Y_j_binned)
#   }
#   
#   return(list(Y_binned = Y_binned, aux_bins = aux_bins, aux_quantiles = aux_quantiles, F_invs = F_invs))
# }

# 
# Y <- matrix(NA, nrow = 50, ncol = 2)
# Y[, 1] <- rnorm(50, mean = 5, sd = 2)  # First column from N(5, 2)
# Y[, 2] <- rnorm(50, mean = 10, sd = 3) # Second column from N(10, 3)
# 
# # Step 2: Introduce some NA values randomly in Y
# missing_indices_col1 <- sample(1:50, 10)  # Randomly select 10 indices for NA in column 1
# missing_indices_col2 <- sample(1:50, 15)  # Randomly select 15 indices for NA in column 2
# 
# Y[missing_indices_col1, 1] <- NA
# Y[missing_indices_col2, 2] <- NA
# 
# # Step 3: Create the R matrix to indicate missingness
# R <- matrix(0, nrow = 50, ncol = 2)
# R[is.na(Y)] <- 1  # Set to 1 if the value in Y is NA
# 
# # Combine Y and R into a single matrix called YR
# YR <- cbind(Y, R)
# 
# # Add column names for clarity
# colnames(YR) <- c("Y1", "Y2", "R1", "R2")
# 
# # Print the resulting matrix
# print(YR)

