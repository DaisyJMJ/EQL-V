# # Define a function to compute quantiles, their asymptotic standard errors, and generate samples
# asymptotic_quantile <- function(YR, auxq, n_generated = 1) {
#   n <- length(YR)  # Sample size
#   density_est <- density(YR,na.rm = T)  # Kernel density estimate of the data
#   
#   results <- list()  # To store results for each quantile
#   all_generated_quantiles <- list()  # To store all generated quantiles across quantiles
#   
#   # A flag to indicate whether we have found non-decreasing quantiles
#   quantiles_in_order <- FALSE
#   
#   while (!quantiles_in_order) {
#     # Flag to track whether all quantiles are in non-decreasing order
#     quantiles_in_order <- TRUE
#     
#     # Temporary storage for the generated quantiles
#     current_generated_quantiles <- list()
#     
#     for (p in auxq) {
#       # Estimate the p-quantile
#       sample_quantile <- quantile(YR, probs = p, na.rm = T)
#       
#       # Estimate the density at the sample quantile
#       f_theta_p_hat <- approx(density_est$x, density_est$y, xout = sample_quantile)$y
#       
#       # Asymptotic standard error
#       asymptotic_se <- sqrt(p * (1 - p)) / (sqrt(n) * f_theta_p_hat)
#       
#       # Generate samples from the asymptotic normal distribution
#       generated_quantiles <- rtruncnorm(n_generated, a = sample_quantile-asymptotic_se,b=sample_quantile+asymptotic_se,
#                                         mean = sample_quantile, sd = asymptotic_se)
#       
#       # Store the results for each quantile
#       current_generated_quantiles[[paste0("Quantile_", p)]] <- generated_quantiles
#       
#       # Store the generated quantiles in the overall list
#       all_generated_quantiles[[paste0("Quantile_", p)]] <- generated_quantiles
#     }
#     
#     # Check if the generated quantiles are in non-decreasing order
#     all_quantiles <- unlist(current_generated_quantiles)
#     if (any(diff(all_quantiles) < 0)) {
#       # If the quantiles are not in non-decreasing order, set the flag to FALSE
#       quantiles_in_order <- FALSE
#     }
#   }
#   
#   # Add all generated quantiles to the result as a separate entry
#   results$Generated_Quantiles <- all_generated_quantiles
#   
#   return(results)  # Return all results as a list
# }
# 
# # YR is a column 
# 
# quantile_draw <- function(YR, ncolY, aux_quantiles, aux_infos, err, n_generated = 1){
#   
#   results <- list()  # To store results for each quantile
#   
#   all_generated_quantiles <- list()  # To store all generated quantiles across quantiles
#   for (j in 1:ncolY){
#   
#     Y_j = YR[,j]
#     quantile_j = aux_quantiles[[j]]
#     quantile_value_j = aux_infos[[j]]
#   
#     # A flag to indicate whether we have found non-decreasing quantiles
#     quantiles_in_order <- FALSE
#   
#    while (!quantiles_in_order) {
#       # Flag to track whether all quantiles are in non-decreasing order
#       quantiles_in_order <- TRUE
#     
#       # Temporary storage for the generated quantiles
#       current_generated_quantiles <- list()
#     
#       for (p in quantile_j) {
#       
#         val_true = quantile_value_j[p]
#       
#         # Generate samples from the asymptotic normal distribution
#         generated_quantiles <- rnorm(n_generated,
#                                         mean = val_true, sd = err)
#       
#         # Store the results for each quantile
#         current_generated_quantiles[[paste0(j," col Quantile_", p)]] <- generated_quantiles
#       
#         # Store the generated quantiles in the overall list
#         all_generated_quantiles[[paste0(j," col Quantile_", p)]] <- generated_quantiles
#       }
#     
#       # Check if the generated quantiles are in non-decreasing order
#       all_quantiles <- unlist(current_generated_quantiles)
#       if (any(diff(all_quantiles) < 0)) {
#         # If the quantiles are not in non-decreasing order, set the flag to FALSE
#         quantiles_in_order <- FALSE
#       }
#     }
#   
#     # Add all generated quantiles to the result as a separate entry
#     results$Generated_Quantiles <- all_generated_quantiles
#   }
#   
#   return(results)  # Return all results as a list
#   
# }


#This the Chatgpt modified code I think it's working

quantile_draw <- function(YR, ncolY, aux_quantiles, aux_infos, err = 0, n_generated = 1) {
  
  # Initialize a list to store generated quantiles for each column
  all_generated_quantiles <- list()
  
  for (j in 1:ncolY) {
    Y_j = YR[, j]
    quantile_j = aux_quantiles[[j]]
    quantile_value_j = aux_infos[[j]]
    
    # A flag to indicate whether we have found non-decreasing quantiles
    quantiles_in_order <- FALSE
    
    while (!quantiles_in_order) {
      # Temporary storage for the generated quantiles for this column
      current_generated_quantiles <- numeric(length(quantile_j))
      
      for (k in seq_along(quantile_j)) {
        p = quantile_j[k]
        val_true = quantile_value_j[k]
        
        # Generate samples from the asymptotic normal distribution
        generated_quantiles <- rnorm(n_generated, mean = val_true, sd = err)
        
        # Store the generated value (take the first if `n_generated > 1`)
        # If the quantile is 0, ensure the generated value is not larger than the maximum value of the data
        if (p == 0) {
          current_generated_quantiles[k] <- min(generated_quantiles[1], min(Y_j-0.001, na.rm = TRUE))
        }
        # If the quantile is 1, ensure the generated value is not smaller than the minimum value of the data
        else if (p == 1) {
          current_generated_quantiles[k] <- max(generated_quantiles[1], max(Y_j+0.001, na.rm = TRUE))
        } 
        else {
          # Store the generated value (take the first if `n_generated > 1`)
          current_generated_quantiles[k] <- generated_quantiles[1]
        }
        
      }
      
      # Check if the generated quantiles are in non-decreasing order
      if (all(diff(current_generated_quantiles) >= 0)) {
        quantiles_in_order <- TRUE
      }
    }
    
    # Store the generated quantiles for this column in the list
    all_generated_quantiles[[paste0("Column_", j)]] <- current_generated_quantiles
  }
  
  return(all_generated_quantiles)  # Return the list of generated quantiles
}

# YR <- matrix(runif(100, min = 0, max = 10), ncol = 5)
# 
# # Specify the number of columns of Y
# ncolY <- 2
# 
# # Define the auxiliary quantiles
# aux_quantiles <- list(
#   c(1, 2, 3, 4, 5),
#   c(1, 2, 3, 4, 5)
# )
# 
# # Define the auxiliary information (quantile values)
# aux_infos <- list(
#   c(-Inf, 3, 5, 6.3, Inf),
#   c(-Inf, 2, 4, 6, Inf)
# )
# 
# # Set error term
# err <- 0.5
# 
# result <- quantile_draw(YR, ncolY, aux_quantiles, aux_infos, err)

# # Print results for each quantile
# cat(p, "-Quantile:", sample_quantile, "\n")
# cat("Asymptotic Standard Error:", asymptotic_se, "\n")
# cat("Generated quantiles from the asymptotic distribution:", generated_quantiles, "\n\n")

