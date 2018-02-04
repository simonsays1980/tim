estimateMRR <- function( price.diff, price, indicator, indicator.lag,
                         midquote, midquote.lag, spread.quoted ) 
{
  # add here some safeguards 
  n          <- length( price.diff )
  features   <- cbind( price.diff, indicator, indicator.lag ) 
  dmid       <- midquote - midquote.lag
  spread.eff <- 2 * indicator * ( price - midquote ) 
  
  # moments for GMM estimation
  moments <- function( beta, x ) 
  {
    err   <- ( x[,1] - ( beta[2] + beta[1] ) * x[,2] + ( beta[2] + beta[3] * beta[1] ) * x[,3] )
    m1    <- x[,2] * x[,3] - beta[3] * x[,2]^2
    m2    <- err * x[,2]
    m3    <- err * x[,3]
    moms  <- cbind( m1, m2, m3 )
    return( moms )
  }
  # estimate GMM model
  result <- gmm::gmm( moments, features, c( theta = 0.03, phi = 0.015, rho = 0.2 ), 
                      prewhite = 1, kernel = "Bartlett", vcov = "HAC", 
                      bw = sandwich::bwNeweyWest ) 
  summry <- summary( result ) 
  # create output object
  output <- data.frame( theta = double(), theta_std = double(), theta_t = double(), 
                        theta_p = double(), phi = double(), phi_std = double(), 
                        phi_t = double(), phi_p = double(), rho = double(), 
                        rho_std = double(), rho_t = double(), rho_p = double(), 
                        R2 = double(), R2_adj = double(), F_test = double(), 
                        F_pval = double(), theta_start = double(), phi_start = double(),
                        rho_start = double(), n = integer(), eps_std = double(),
                        eta_std = double(), spread_eff = double(), spread_eff_std = double(),
                        spread_eff_emp = double(), spread_eff_emp_std = double(), 
                        spread_eff_emp_se = double(), spread_eff_emp_med = double(), 
                        spread_quoted = double(), spread_quoted_std = double(), 
                        spread_quoted_se = double(), spread_quoted_med = double() )
  # theta
  output[1, 1]   <- summry$coefficients[1, 1]  # theta
  output[1, 2]   <- summry$coefficients[1, 2]  # theta se
  output[1, 3]   <- summry$coefficients[1, 3]  # theta t-val
  output[1, 4]   <- summry$coefficients[1, 4]  # theta p-val
  # phi
  output[1, 5]   <- summry$coefficients[2, 1]  # phi
  output[1, 6]   <- summry$coefficients[2, 2]  # phi se
  output[1, 7]   <- summry$coefficients[2, 3]  # phi t-val
  output[1, 8]   <- summry$coefficients[2, 4]  # phi p-val
  # rho
  output[1, 9]   <- summry$coefficients[3, 1]  # rho
  output[1, 10]  <- summry$coefficients[3, 2]  # rho se
  output[1, 11]  <- summry$coefficients[3, 3]  # rho t-val
  output[1, 12]  <- summry$coefficients[3, 4]  # rho p-val
  # statistics
  fitted.vals    <- ( output[1, 5] + output[1, 1] ) * features[,2] - 
    ( output[1, 5] + output[1, 9] * output[1, 1] ) * features[,3]
  SSE            <- sum( ( fitted.vals - mean( fitted.vals ) )^2 )
  SST            <- sum( ( features[,1] - mean( features[,1] ) )^2 )
  output[1, 13]  <- SSE / SST # R2
  output[1, 14]  <- 1 - ( result$n - 1 ) / ( result$n - 2 ) * ( 1 - output[1, 13] ) # R adj.
  output[1, 15]  <- ( result$n - 2 ) / 2 * output[1, 13] / ( 1 - output[1, 13] ) # F-stat
  output[1, 16]  <- pf( output[1, 15], result$n - 2, 2, result$n, lower.tail = FALSE ) # F p-val
  # starting values
  output[1, 17]  <- 0.03      # theta start
  output[1, 18]  <- 0.015     # phi start
  output[1, 19]  <- 0.2       # rho start
  output[1, 20]  <- result$n  # obs
  # residuals - epsilon & eta
  epsilon        <- dmid - output[1, 1] * indicator + output[1, 9] * output[1, 1] * indicator.lag
  output[1, 21]  <- sd( epsilon ) 
  output[1, 22]  <- sd( price.diff - fitted.vals - epsilon ) # eta se
  # spreads
  ## estimated effective spread
  output[1, 23]  <- 2 * ( output[1, 5] + output[1, 1] ) # est. eff. spread
  nabla          <- as.matrix( c( 2, 2, 0 ) )
  vcov           <- as.matrix( result$vcov )
  output[1, 24]  <- sqrt( t( nabla ) %*% vcov %*% nabla ) # est. eff. spread se
  ## empirically measured effective spread
  output[1, 25]  <- mean( spread.eff ) # emp. eff. spread mean
  output[1, 26]  <- sd( spread.eff )   # emp. eff. spread sd
  output[1, 27]  <- sd( spread.eff ) / sqrt( n ) # emp. eff. spread se  
  output[1, 28]  <- quantile( spread.eff, probs = c( .5 ), type = 5 ) # emp. eff. spread med
  output[1, 29]  <- mean( spread.quoted ) # quoted spread
  output[1, 30]  <- sd( spread.quoted ) # quoted spread sd
  output[1, 31]  <- sd( spread.quoted ) / sqrt( n ) # quoted spread se
  output[1, 32]  <- quantile( spread.quoted, probs = c( .5 ), type = 5 ) # med. spread quoted
  
  return( output )
}

estimateMRRMod <- function( price.diff, price, indicator, indicator.lag, indicator.lag2,
                            midquote, midquote.lag ) 
{
  output <- data.frame( N = integer(), rho = double(), a1 = double(), a2 = double(), 
                        psi1 = double(), psi2 = double(), varv = double(), varu = double(),
                        covuv = double(), spread = double(), spread.est1 = double(),
                        spread.est2 = double(), stable = logical(), rho_std = double(),
                        a1_std = double(), a2_std = double(), psi1_std = double(), 
                        psi2_std = double() )
  # define starting parameters
  phi           <- .01
  rho           <- .2
  # define the AR component of the VARMA model
  AR            <- array( c( 1, 0, 0, 0, 0, -phi * ( rho - 1 ), 1, -rho ), c( 2, 2, 2 ) )
  # define the MA component of the VARMA model
  MA            <- array( c( 1, 0, 0, 1 ), c( 1, 2, 2 ) )
  # define the VARMA model
  # as `dse` package is simply loaded but not attached to the search path 
  # a namespace has to be used
  arma          <- dse::ARMA( A = AR, B = MA, C = NULL )
  
  output[1, 1]  <- length( price.diff )
  # estimate v_t and sigma(v_t)
  res1          <- lm( indicator ~ 0 + indicator.lag )
  vt            <- res1$residuals
  vtlag         <- vt[1:length( vt ) - 1]
  varv          <- var( vt )
  # calculate the midquote difference
  dmid          <- midquote - midquote.lag
  # bring `dmid` time series to the same length as `vtlag`
  dmid          <- dmid[2:length( dmid )]
  # compute second difference 
  dq            <- indicator.lag - indicator.lag2
  # bring `dq` time series to the same length as `vtlag`
  dq            <- dq[2:length( dq )]
  # estimate u_t, sigma(u_t), and cov(v_t,u_t)
  res2          <- lm( dmid ~ 0 + dq + vtlag )
  ut            <- res2$residuals
  varu          <- var( ut )
  covuv         <- cov( ut, vt[2:length( vt )] )
  # estimate the VARMA model
  arma.dat      <- dse::TSdata( input = NULL, output = cbind( price.diff, indicator ) )
  arma.est      <- dse::estMaxLik( arma, arma.dat )
  stable        <- dse::stability( arma.est, verbose = FALSE )
  res.cov       <- arma.est$estimates$cov
  spread        <- mean( indicator * ( price - midquote ) * 2, na.rm = TRUE )
  spread.est1   <- res.cov[1, 2] / res.cov[2, 2]
  spread.est2   <- ( res.cov[1, 2] - covuv ) / res.cov[2, 2] * 2
  # store rho 
  output[1, 2]  <- res1$coefficients[1]
  # store a1 and a2
  output[1, 3]  <- res2$coefficients[1]
  output[1, 4]  <- res2$coefficients[2]
  # store psi1 and psi2 
  output[1, 5]  <- -arma.est$estimates$results$par[1]
  output[1, 6]  <- -arma.est$estimates$results$par[2]
  # store var(u_t), var(v_t), and cov(u_t,v_t)
  output[1, 7]  <- varv
  output[1, 8]  <- varu
  output[1, 9]  <- covuv
  # store spread estimates
  output[1, 10] <- spread       # spread_eff_emp
  output[1, 11] <- spread.est1  # spread est from MRR
  output[1, 12] <- spread.est2  # spread est from MRR modified
  # store stability classifier
  output[1, 13] <- stable
  # store HAC consistent standard errors for rho
  output[1, 14] <- sqrt( sandwich::NeweyWest( res1 )[1, 1] )
  # store HAC consistent standard errors for a1 and a2
  step2cov      <- sqrt( diag( sandwich::NeweyWest( res2 ) ) )
  output[1, 15] <- step2cov[1]
  output[1, 16] <- step2cov[2]
  # store standard errors for psi1 and psi2 by inverting the hessian
  minv          <- solve( arma.est$estimates$results$hessian )
  minv          <- sqrt( diag( minv ) )
  output[1, 17] <- minv[1]
  output[1, 18] <- minv[2]
  
  return( output ) 
}