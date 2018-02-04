"estimateHS" <- function( price.diff, price, ind, indlag, mid )
{
  # compute midquote difference
  ind.diff      <- ind - indlag
  
  # initialize output
  output <- data.frame( N = integer(), beta1 = double(), beta1_se = double(), beta1_t = double(),
                        beta1_p = double(), beta2 = double(), beta2_se = double(), beta2_t = double(),
                        beta2_p = double(), R = double(), Radj = double(), Fstat = double(), 
                        Fstat_p = double(), spread = double(), spread_std = double(), spread_se = double(), 
                        spread.est = double(), spread.est_se = double(), phi = double(), phi_se = double() )
                                          
  # estimate model with Newey West standard errors
  res           <- lm( price.diff ~ 0 + ind.diff + indlag )  
  summry        <- summary( res )
  nwvcov        <- sandwich::NeweyWest( res )
  nwstderr      <- sqrt( diag( nwvcov ) )
  # number of observations 
  output[1, 1]  <- length( price.diff )  # number of observations 
  # beta_1
  output[1, 2]  <- res$coefficients[1] # beta_1                                    
  output[1, 3]  <- nwstderr[1] # stderr( beta_1 )
  output[1, 4]  <- output[1, 2] / output[1, 3] # t( beta_1 )
  output[1, 5]  <- 2 * pt( abs( output[1, 4] ), df = output[1, 1] - 2, lower.tail = FALSE ) # p( beta_1 )
  # beta_2
  output[1, 6]  <- res$coefficients[2] # beta_2 
  output[1, 7]  <- nwstderr[2] # stderr( beta_2 ) 
  output[1, 8]  <- output[1, 6] / output[1, 7] # t( beta_2 )
  output[1, 9]  <- 2 * pt( abs( output[1, 8] ), df = output[1, 1] - 2, lower.tail = FALSE ) # p( beta_2 )
  # statistics 
  output[1, 10] <- summry$r.squared # R squared
  output[1, 11] <- summry$adj.r.squared # adj. R squared
  output[1, 12] <- summry$fstatistic[1] # F-statistic
  output[1, 13] <- pf( summry$fstatistic[1], df1 = summry$df[1], df2 = summry$df[2], lower.tail = FALSE )
  # spread
  spreads       <- ind * ( price - mid ) * 2 
  output[1, 14] <- mean( spreads, na.rm = TRUE ) # empirical eff. spread
  output[1, 15] <- sd( spreads, na.rm = TRUE  ) # std( empirical eff. spread )
  output[1, 16] <- sd( spreads, na.rm = TRUE ) / sqrt( output[1, 1] ) # stderr( spread )
  output[1, 17] <- res$coefficients[1] * 2 # estimated spread 
  output[1, 18] <- sqrt( 2 ) * output[1, 3] # stderr( estimated spread )
  # phi
  output[1, 19] <- res$coefficients[1] - res$coefficients[2]  # phi
  nabla         <- as.matrix( c( 1, -1 ) )
  output[1, 20] <- sqrt( t( nabla ) %*% nwvcov %*% nabla ) # stderr( phi )
  
  return ( output )
}

"estimateHSMod" <- function( price.diff, price, ind, ind.lag, mid, mid.lag )
{
  # define data.frame to store results
  output <- data.frame( N = integer(), a = double(), a_se = double(), a_t = double(), a_p = double(), 
                        phi12 = double(), phi12_se = double(), phi12_t = double(), phi12_p = double(), 
                        varv = double(), varu = double(), covuv = double(), spread = double(), 
                        spread_std = double(), spread_se = double(), spread.est1 = double(), 
                        spread.est2 = double(), stable = logical() )
  # define start parameters
  phi         <- 0.01
  theta       <- 0.03
  # define the VARMA model
  AR          <- array( c( 1, 0, 0, 0, 0, -( theta - phi ), 1, 0 ), c( 2, 2, 2 ) )
  MA          <- array( c( 1, 0, 0, 1 ), c( 1, 2, 2 ) )
  arma        <- dse::ARMA( A = AR, B = MA, C = NULL )
  
  # estimate variance of v_t (v_t itself is identical to ind)
  varv          <- var( ind )     
  # calculate the first midquote difference
  dmid          <- mid - mid.lag
  # estimate u_t, sigma(u_t) and cov(v_t,u_t)
  res           <- lm( dmid ~ 0 + ind.lag )
  ut            <- res$residuals
  varu          <- var( ut )
  covuv         <- cov( ut, ind )
  # estimate VARMA 
  arma.dat      <- dse::TSdata( input = NULL, output = cbind( price.diff, ind ) )
  arma.est      <- dse::estMaxLik( arma, arma.dat )
  stable        <- dse::stability( arma.est, verbose = FALSE )
  res.cov       <- arma.est$estimates$cov
  # calculate spread and spread estimates
  spreads       <- ind * ( price - mid ) * 2
  spread        <- mean( spreads, na.rm = TRUE )
  spread.est1   <- res.cov[1, 2] / res.cov[2, 2] * 2
  spread.est2   <- ( res.cov[1, 2] - covuv ) / res.cov[2, 2] * 2
  # store results
  output[1, 1]  <- length( price.diff ) # number of observations 
  # a
  output[1, 2]  <- res$coefficients[1]  # a
  ## estimate Newey West standard errors
  output[1, 3]  <- sqrt( sandwich::NeweyWest( res )[1, 1] ) # se( a )
  output[1, 4]  <- output[1, 2] / output[1, 3] # t( a ) 
  output[1, 5]  <- 2 * pt( abs( output[1, 4] ), df = output[1, 1] - 1, lower.tail = FALSE ) # p( a )
  # phi12
  output[1, 6]  <- -arma.est$estimates$results$par[1] # phi12
  ## estimate standard error via the inverse hessian
  minv          <- solve( arma.est$estimates$results$hessian )
  minv          <- sqrt( diag( minv ) ) 
  output[1, 7]  <- minv[1] # se( phi12 )
  output[1, 8]  <- output[1, 6] / output[1, 7] # t( phi12 )
  output[1, 9]  <- 2 * pt( abs( output[1, 8] ), df = output[1, 1] - 2, lower.tail = FALSE ) # p( phi12 )   
  ## store var( v_t ), var( u_t ), cov( u_t, v_t )
  output[1, 10] <- varv # v( v_t )
  output[1, 11] <- varu # v( u_t )
  output[1, 12] <- covuv # cov( u_t,v_t )
  ## store spread and spread estimates
  output[1, 13] <- spread # empirical eff. spread
  output[1, 14] <- sd( spreads ) # std( empirical eff. spread )
  output[1, 15] <- output[1, 14] / sqrt( output[1, 1] ) # stderr( empirical eff. spread )
  output[1, 16] <- spread.est1 # estimate of eff. spread from HS model
  output[1, 17] <- spread.est2 # estimate of eff. spread from mod. HS model
  ## store stability classifier from VARMA
  output[1, 18] <- stable[1] # stability clasifier 
  
  return( output )
}