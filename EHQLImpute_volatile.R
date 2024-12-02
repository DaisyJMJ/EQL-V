#' Imputation of nonignorable missing data using the EHQL Gaussian Copula
#'
#' The user specifies auxiliary quantiles of the marginal distribution of each study variable,
#' and inputs an observed data matrix (Y,R) containing the observed and missing study variables (Y) and missingness indicators.
#' The function estimates the EHQL Gaussian copula, returns posterior samples of the copula parameters, and creates nImps completed data sets
#'
#' @param YR Observed data matrix. It should be partitioned into the study variables Y followed by missingness indicators R.
#' @param ncolY Number of study variables
#' @param ncolR Number of study variables modeled as noningorable
#' @param aux_quantiles List of length ncolY containing auxiliary quantiles assumed known for each study variable. aux_quantiles[[j]] should be a vector containg 0, intermediate quantiles, and 1.
#' @param aux_infos List of length ncolY containing the known auxiliary values of the study variables. aux_infos[[j]] should be a vector containing \eqn{(F_j^{-1}(0),\dots,F_{j}^{-1}(\tau),\dots,F_{j}^{-1}(1))}. In addition aux_infos[[j]] should be the same length as aux_quantiles[[j]]
#' @param MA Vector logicals of length ncolY indicating whether or not to estimate the margin adjustment at intermediate quantiles using the margin adjustment. Default is True for each study variable, which specifies an evenly spaced
#' grid of 15 points across \eqn{Y_j^{obs}}, and learns the intermediate quantiles of these points during the EHQL Gibbs sampler. These posterior samples are then smoothed and used for imputation.
#' @param nImps Number of completed data sets to produce
#' @param nsamp Number of iterations for the MCMC
#' @param burn Number of Burn-in samples for the MCMC
#' @param err Standard error for the quantile drawing, default to 0
#' @import truncnorm
#' @import stats
#' @return C.post,alpha.post, and MAs, which are the nsamp-burn posterior samples of the copula correlation, latent intercept vector, and margin adjustment 
#' +(list or length ncolY, MAs[[j]] computes the margin adjustment at x_ma[[j]]), respectively.
#'  In addition, YImpute is a list of length nImps containing imputed data sets.
#' @export

# YR stays unchanged 

EHQLImpute_volatile<- function(YR,
                      ncolY,
                      ncolR,
                      aux_quantiles,
                      aux_infos,
                      MA = rep(T, ncolY),
                      nImps,
                      nsamp,
                      burn,
                      err = 0){

  
  xMAs <- vector("list", ncolY)
  MAs <- vector("list", ncolY)
  # F_invs = vector('list',ncolY) #interpolated cdf
  aux_bins = vector('list',ncolY) #get aux bin membership
  Completed_Data = vector('list',nImps)# store completed data

  imp_every = floor((nsamp-burn)/nImps)

  # Store original auxiliary input so not updated through each binning
  aux_infos_originial = aux_infos
  aux_quantiles_original = aux_quantiles

  # Call the new bin_data function, in side we generate a random draw in the quantile_draw
  aux_infos = quantile_draw(YR, ncolY, aux_quantiles, aux_infos,err)
  binning_results <- bin_data(YR, ncolY, aux_quantiles, aux_infos, MA)  #### modification: replaced this with bin_data()
  
  # Extract the results
  Y_binned <- binning_results$Y_binned
  aux_bins <- binning_results$aux_bins
  aux_quantiles <- binning_results$aux_quantiles

  #create binned data
  Y <- as.matrix(cbind(Y_binned,YR[,((ncolY+1):ncol(YR))]))
  nobs = dim(Y)[1]
  n = dim(Y)[1]
  p <- dim(Y)[2]
  plugin.marginal = rep(F,p)


  #initiate ranks and latent data
  R <- NULL
  for (j in 1:p) {

    R <- cbind(R, match(Y[, j], sort(unique(Y[, j]))))

  }
  Rlevels <- apply(R, 2, max, na.rm = TRUE)

  Ranks <- apply(Y, 2, rank, ties.method = "max", na.last = "keep")
  N <- apply(!is.na(Ranks), 2, sum)
  U <- t(t(Ranks)/(N + 1))
  Z = NULL
  nobs = n
  for(j in 1:p){
    if(j <= ncolY){
      Z = cbind(Z, U[,j])
    }else{  
      Zj = array(0,n)
      # here Y is already combined, refer up                    #TODO: a,b反了吗？
      Zj[which(Y[,j] == 1)] = rtruncnorm(sum(Y[,j] == 1),a = 0) # lower bound 0, sum(Y[,j] == 0): numebr of samples generated
      Zj[which(Y[,j] == 0)] = rtruncnorm(sum(Y[,j] == 0),b = 0) # upper bound 0
      Z = cbind(Z,Zj)
    }

  }

  Zfill <- matrix(rnorm(n * p), n, p)
  Z[is.na(Y)] <- Zfill[is.na(Y)]
  Z[is.na(Z)] =  Zfill[is.na(Z)]
  
  # initialize covariance matrix

  k.star = floor(p/2)
  #
  # Initialize Factor Model Parameters: these are

  # Local df:
  nu = 3;

  # Global shrinkage:
  a1 = 2; a2 = 3;

  # Error variance:
  a.sigma = 1; b.sigma = 0.3


  # Use SVD for simplicity
  svd0 = svd(Z); # plot(1-cumsum(svd0$d^2)/sum(svd0$d^2)); #plot(svd0$d[-1]/svd0$d[-min(p,n)])

  # Factor loadings (p x k.star):
  Lambda = svd0$v[1:p,1:k.star]

  # Factors (n x k.star):
  eta = svd0$u[,1:k.star] #y%*%Lambda

  # Diagonal error variance (p-dimensional vector):
  Sigma.diag = rep(1,p)
  #

  #Local and global precision parameters:
  phi.jh = 1/Lambda^2; tau.h = delta.h = rep(1, k.star)

  # intercept
  alpha = rep(0,p)
  C.post= array(dim = c(p, p, floor(nsamp)- burn))

  alpha.post<- array(dim = c(p, floor(nsamp)- burn))
  have_aux = rep(T,ncolY)

  #for eql
  is_cat_bin = c(rep(0,ncolY), rep(1,ncolR))


  #for imputations

  m = 1
  
# Start of the Gibbs Sampler
  for (ns in 1:nsamp){
    
    # print("Start")
    #Step 1: Sample the factor loadings

    cp.eta = crossprod(eta)
    for(j in 1:p){
      chQj = chol(diag(phi.jh[j,]*tau.h, k.star) + cp.eta/Sigma.diag[j])
      lj = crossprod(eta, Z[,j] - alpha[j])/Sigma.diag[j]
      Lambda[j,] = backsolve(chQj,forwardsolve(t(chQj), lj) + rnorm(k.star))
    }

    # Step 2: sample the error variances
    eps = Z - sweep(tcrossprod(eta, Lambda),2,alpha,'+')

    Sigma.diag = apply(eps, 2, function(x) 1/rgamma(n = 1, shape = a.sigma + n/2,
                                                    rate = b.sigma + 1/2*sum(x^2)))

    # Step 3: sample the factors
    chQeta = chol(diag(k.star) + crossprod(Lambda, diag(1/Sigma.diag))%*%Lambda)
    leta = tcrossprod(crossprod(Lambda, diag(1/Sigma.diag)), sweep(Z,2,alpha,'-'))
    eta = t(backsolve(chQeta,forwardsolve(t(chQeta), leta) + rnorm(n*k.star))) #for(i in 1:n) eta[i,]= backsolve(chQeta,forwardsolve(t(chQeta), leta[,i]) + rnorm(k.star))

    # Step 4: sample phi.jh
    phi.jh = matrix(rgamma(n = p*k.star, shape = (nu + 1)/2,
                           rate = (nu + Lambda^2*matrix(rep(tau.h, each = p), nrow = p))/2), nrow = p) #for(h in 1:k.star){for(j in 1:p) phi.jh[j,h] = rgamma(n = 1, shape = (nu + 1)/2, rate = (nu + Lambda[j,h]^2*tau.h[h])/2)

    # Step 5: sample tau.h via delta.h
    delta.h = sampleMGP(theta.jh = sqrt(phi.jh)*Lambda, delta.h = delta.h, a1 = a1, a2 = a2)
    tau.h = cumprod(delta.h)

    # Step 6: Sample alpha

    eps = Z - tcrossprod(eta, Lambda)
    for(j in (ncolY +1):p){
      lalpha = sum(eps[,j])/Sigma.diag[j]
      Qalpha = (n/Sigma.diag[j] + 1)^-1
      alpha[j] = rnorm(1,Qalpha*lalpha,sqrt(Qalpha))
    }


    # Step 7: EHQL resampling of Z
    Z_past = Z ######################## TODO

    for(j in 1:p){

      Zj = resample_Z_MA(j,Z,Y,R,Rlevels,alpha, Lambda, eta, Sigma.diag,
                         plugin.marginal,is_cat_bin,have_aux,aux_quantiles,aux_bins,
                         Z_past)
      Z[,j] = Zj

    }


    if(any(is.na(Z))){
      break
    }
    if( (ns%%1000) == 0) {

      print(cat("Sampling is:", round(100 * ns/nsamp), "percent done"))

    }
    if (ns >= burn ) {
      S<-tcrossprod(Lambda) + diag(Sigma.diag)
      
      C <- S/(sqrt(diag(S)) %*% t(sqrt(diag(S))))

      C.post[, , ns - burn ] = C
      alpha.post[,ns-burn] = alpha/sqrt(diag(S))
      Y_copy = YR[,1:ncolY]
     
      # Calculate maginal adjustments
      result <- return_MA(Y,ncolY,Z,S,xMAs, MAs)
      xMAs <- result$xMAs
      MAs <- result$MAs
      x_ma <- result$x_ma
      MA_j <- result$MA_j
      # for(j in 1:ncolY){
      # 
      #   # 强行macth
      #   if (is.null(MAs[[j]]) || ncol(MAs[[j]]) == length(MA_j[[j]])) {    # Here is the problem
      #     # Combine the current vector with the matrix
      #     MAs[[j]] <- rbind(MAs[[j]], MA_j[[j]])
      #   }
      # }
      
      lbs = sapply(1:ncolY, function(j) aux_infos[[j]][1])  # TODO: is it useful?
      ubs = sapply(1:ncolY, function(j) aux_infos[[j]][length(aux_infos[[j]])])   # TODO: is it useful?
      
      if((ns-burn)%%imp_every == 0){ # imputation
        
        # for (i in 1:ncolY){
        #   print("i:")
        #   print(i)
        # 
        # print("length(c(MA_j[[i]]))")
        # print(length(c(MA_j[[i]])))
        # print("MA_j[[i]]")
        # print(MA_j[[i]])
        # print("c(MA_j[[i]])")
        # print(c(MA_j[[i]]))
        # 
        # print("x_ma[[i]]")
        # print(x_ma[[i]])
        # print("c(x_ma[[i]])")
        # print(c(x_ma[[i]]))
        # print("length(c(x_ma[[i]]))")
        # print(length(c(x_ma[[i]])))
        # }
        # 
        # Modified TODO with unlist
        Fn_inv = sapply(1:ncolY, function(j)
          splinefun(c(MA_j[[j]]),c(x_ma[[j]]), method = "monoH.FC"))
        
        # print("pass")
        for(j in 1:ncolY){

          na_inds = which(is.na(YR[,j]))

          Y_mis<- Fn_inv[[j]](pnorm(Z[na_inds,j], sd = sqrt(S[j,j])))


          Y_copy[na_inds,j] = Y_mis

        }
        Completed_Data[[m]] = Y_copy
        m = m+1
      }
    }
    
    # TODO: below before return is what has been modified
    # print("here3")
    # Update bins -> update Y
    # Call the new bin_data function, in side we generate a random draw in the quantile asymptotic distribution
    aux_infos = quantile_draw(YR, ncolY, aux_quantiles_original, aux_infos_originial,err)
    binning_results <- bin_data(YR, ncolY, aux_quantiles_original, aux_infos, MA) 
    
    # print("here4")
    # Extract the results
    Y_binned <- binning_results$Y_binned
    aux_bins <- binning_results$aux_bins
    aux_quantiles <- binning_results$aux_quantiles
    #create binned data
    Y <- as.matrix(cbind(Y_binned,YR[,((ncolY+1):ncol(YR))]))
    nobs = dim(Y)[1]
    n = dim(Y)[1]
    p <- dim(Y)[2]
    plugin.marginal = rep(F,p)
    
    # TODO: do we need to initialize the values again?
    

    
  }

  
  # End of the Gibbs Sampler
  return(list(
    C.post = C.post,
    alpha.post = alpha.post,
    MAs = MAs,
    x_ma = xMAs,
    YImpute = Completed_Data
  ))
}
