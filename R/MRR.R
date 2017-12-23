estimateMRR <- function( price.diff, price, indicator, indicator.lag,
                         midquote, midquote.lag, spread.quoted ) 
{
  # add here some safeguards 
  n          <- length( price.diff )
  features   <- cbind( price.diff, indicator, indicator.lag ) 
  dmid       <- midquote - midquote.lag
  spread.eff <- 2 * indicator * ( price - midquote ) 
  
  moments <- function( beta, x ) 
  {
    err   <- ( x[,1] - ( beta[2] + beta[1] ) * x[,2] + ( beta[2] + beta[3] * beta[1] ) * x[,3] )
    m1    <- x[,2] * x[,3] - beta[3] * x[,2]^2
    m2    <- err * x[,2]
    m3    <- err * x[,3]
    moms  <- cbind( m1, m2, m3 )
    return( moms )
  }
  result <- gmm::gmm( moments, features, c( theta = 0.03, phi = 0.015, rho = 0.2 ), 
                      prewhite = 1, kernel = "Bartlett", vcov = "HAC", 
                      bw = sandwich::bwNeweyWest ) 
  summry <- summary( result ) 
  output <- vector( mode = 'double', length = 32 )
  output.names <- c( "theta", "theta_std", "theta_t" , "theta_p", "phi", "phi_std",
                     "phi_t", "phi_p", "rho", "rho_std", "rho_t", "rho_p","R2", "R2_adj", 
                     "F_test", "F_pval", "theta_start", "phi_start", "rho_start", "n",
                     "eps_std", "eta_std", "spread_eff", "spread_eff_std", "spread_eff_emp",
                     "spread_eff_emp_std", "spread_eff_emp_se", "spread_eff_emp_med",
                     "spread_quoted", "spread_quoted_std", "spread_quoted_se",                
                     "spread_quoted_med" )
  names( output ) <- output.names
  # theta
  output[1]   <- summry$coefficients[1, 1]  # theta
  output[2]   <- summry$coefficients[1, 2]  # theta se
  output[3]   <- summry$coefficients[1, 3]  # theta t-val
  output[4]   <- summry$coefficients[1, 4]  # theta p-val
  # phi
  output[5]   <- summry$coefficients[2, 1]  # phi
  output[6]   <- summry$coefficients[2, 2]  # phi se
  output[7]   <- summry$coefficients[2, 3]  # phi t-val
  output[8]   <- summry$coefficients[2, 4]  # phi p-val
  # rho
  output[9]   <- summry$coefficients[3, 1]  # rho
  output[10]  <- summry$coefficients[3, 2]  # rho se
  output[11]  <- summry$coefficients[3, 3]  # rho t-val
  output[12]  <- summry$coefficients[3, 4]  # rho p-val
  # statistics
  fitted.vals <- ( output[5] + output[1] ) * features[,2] + 
    ( output[5] + output[9] * output[1] ) * features[,3]
  SSE         <- sum( ( fitted.vals - mean( fitted.vals ) )^2 )
  SST         <- sum( ( features[,1] - mean( features[,1] ) )^2 )
  output[13]  <- SSE / SST # R2
  output[14]  <- ( result$n - 1 ) / ( result$n - 2 ) * ( 1 - output[13] ) # R adj.
  output[15]  <- ( result$n - 2 ) / 2 * output[13] / ( 1 - output[13] ) # F-stat
  output[16]  <- pf( output[15], result$n - 2, 2, result$n, lower.tail = FALSE ) # F p-val
  # starting values
  output[17]  <- 0.03      # theta start
  output[18]  <- 0.015     # phi start
  output[19]  <- 0.2       # rho start
  output[20]  <- result$n  # obs
  # residuals - epsilon & eta
  epsilon     <- dmid - output[1] * indicator + output[9] * output[1] * indicator.lag
  output[21]  <- sd( epsilon ) 
  output[22]  <- sd( price.diff - fitted.vals - output[21] ) # eta se
  # spreads
  ## estimated effective spread
  output[23]  <- 2 * ( output[5] + output[1] ) # est. eff. spread
  nabla       <- as.matrix( c( 2, 2, 0 ) )
  vcov        <- as.matrix( result$vcov )
  output[24]  <- sqrt( t( nabla ) %*% vcov %*% nabla ) # est. eff. spread se
  ## empirically measured effective spread
  output[25]  <- mean( spread.eff ) # emp. eff. spread mean
  output[26]  <- sd( spread.eff )   # emp. eff. spread sd
  output[27]  <- sd( spread.eff ) / sqrt( n ) # emp. eff. spread se  
  output[28]  <- quantile( spread.eff, probs = c( .5 ), type = 5 ) # emp. eff. spread med
  output[29]  <- mean( spread.quoted ) # quoted spread
  output[30]  <- sd( spread.quoted ) # quoted spread sd
  output[31]  <- sd( spread.quoted ) / sqrt( n ) # quoted spread se
  output[32]  <- quantile( spread.quoted, probs = c( .5 ), type = 5 ) # med. spread quoted
  
  return( output )
}