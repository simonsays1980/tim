#' Estimates the model of Madhavan, Richardson, and Roomans (1997)
#' 
#' @param price_diff a numeric vector containing the series of first price differences.
#' @param price a numeric vector containing the price series.
#' @param indicator an integer vector containing the trade direction with a buy as 1 and
#' a sell as -1.
#' @param indicator_lag an integer vector containing the first lag of the indicator series.
#' @param midquote a numeric vector with the midquote price series.
#' @param midquote_lag a numeric vector containing the first lag of the midquote series.
#' @param spread_quoted a numeric vector containing the quoted spread series. 
#' 
#' @return A data.frame with the following values:
#' \describe{
#'  \item{n}{the number of observation used in estimation.}
#'  \item{theta}{the adverse selection component.}
#'  \item{theta_std}{the standard deviation of the adverse selection component.}
#'  \item{theta_t}{the t-value of the adverse selection component.}
#'  \item{theta_p}{the p-value of the adverse selection component.}
#'  \item{phi}{the transitory cost component.}
#'  \item{phi_std}{the standard deviation of the transitory cost component.}
#'  \item{phi_t}{the t-value of the transitory cost component.}
#'  \item{phi_p}{the p-value of the transitory cost component.}
#'  \item{rho}{the indicator first order autocorrelation.}
#'  \item{rho_std}{the standard deviation of the indicator autocorrelation.}
#'  \item{rho_t}{the t-value of the indicator autocorrelation.}
#'  \item{rho_p}{the p-value of the indicator autocorrelation.}
#'  \item{r2}{the coefficient of determination.}
#'  \item{r2_adj}{the adjusted coefficient of determination.}
#'  \item{f_test}{the value of the F-statistic.}
#'  \item{f_pval}{the p-value of the F-statistic.}
#'  \item{theta_start}{the start value for theta in the numerical optimization.}
#'  \item{phi_start}{the start value for phi in the numerical optimization.}
#'  \item{rho_start}{the start value for rho in the numerical optimization.}
#'  \item{eps_std}{the estimated standard deviation of the epsilon error term 
#'  (order flow innovations).}
#'  \item{eta_std}{the estimated standard deviation of the eta error term 
#'  (stochastic rounding errors).}
#'  \item{spread_eff}{the effective spread estimated from the model using the 
#'  formula \eqn{2(\theta+\phi)}.}
#'  \item{spread_eff_std}{the standard deviation of the effective spread, calculated 
#'  via the \emph{delta method}.}
#'  \item{spread_eff_emp}{the empirical effective spread, calculated directly from the data 
#'  by the arithmetic mean of the series \eqn{q_t(p_t-m_t)}, where \eqn{q_t} is the trade 
#'  direction, \eqn{p_t} the transaction price, and \eqn{m_t} the price midquote series.}
#'  \item{spread_eff_emp_std}{the standard deviation of the empirical effective spread calculated 
#'  as the standard deviation of the series \eqn{q_t(p_t-m_t)}.}
#'  \item{spread_eff_emp_se}{the standard error of the estimated empirical effective spread 
#'  using the formula \eqn{SE=STD/\sqrt n}.}
#'  \item{spread_eff_emp_med}{the median of the empirical effective spread calculated as the 
#'  median of the series \eqn{q_t(p_t-m_t)}.}
#'  \item{spread_quoted}{the mean quoted spread using the input quoted spread series.}
#'  \item{spread_quoted_std}{the standard deviation of the quoted spread using the input 
#'  quoted spread series.}
#'  \item{spread_quoted_se}{the standard error of the quoted spread estimate using the 
#'  formula \eqn{SE=STD/\sqrt n}.}
#'  \item{spread_quoted_med}{the median of the quoted spread series.}
#' }
#'  
#' @details The function estimates for given data the trade indicator model of 
#' \href{https://academic.oup.com/rfs/article-abstract/10/4/1035/1605749}{Madhavan, 
#' Richardson, and Roomans (1997)}. For estimation a GMM approach is used similar to 
#' the one desribed in the paper of the three authors. For application the \code{\link{gmm}}-
#' package is used with a \emph{Bartlett}-kernel, a heteroscedastic and autocorrelation 
#' consistent variance-covariance matrix with a \emph{Newey West bandwith}. For details
#' it is referred to the \code{\link[gmm]{gmm}}-function. 
#' 
#' @references Madhavan, Richardson & Roomans (1997), "Why Do Security Prices Change? A
#' Transaction-Level Analysis of NYSE Stocks," Review of Financial Studies, Vol. 10, 
#' No. 4, pp. 1035-1064.

estimate_mrr <- function( price_diff, price, indicator, indicator_lag,
                          midquote, midquote_lag, spread_quoted ) 
{
  # add here some safeguards 
  n          <- length( price_diff )
  features   <- cbind( price_diff, indicator, indicator_lag ) 
  dmid       <- midquote - midquote_lag
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
  output <- data.frame( n = integer(), theta = double(), theta_std = double(), 
                        theta_t = double(), theta_p = double(), phi = double(), 
                        phi_std = double(), phi_t = double(), phi_p = double(), 
                        rho = double(), rho_std = double(), rho_t = double(), 
                        rho_p = double(), r2 = double(), r2_adj = double(), 
                        f_test = double(), f_pval = double(), theta_start = double(), 
                        phi_start = double(), rho_start = double(), eps_std = double(),
                        eta_std = double(), spread_eff = double(), spread_eff_std = double(),
                        spread_eff_emp = double(), spread_eff_emp_std = double(), 
                        spread_eff_emp_se = double(), spread_eff_emp_med = double(), 
                        spread_quoted = double(), spread_quoted_std = double(), 
                        spread_quoted_se = double(), spread_quoted_med = double() )
  
  # number of observations
  output[1, 1]  <- result$n  # obs
  
  # theta
  output[1, 2]   <- summry$coefficients[1, 1]  # theta
  output[1, 3]   <- summry$coefficients[1, 2]  # theta se
  output[1, 4]   <- summry$coefficients[1, 3]  # theta t-val
  output[1, 5]   <- summry$coefficients[1, 4]  # theta p-val
  # phi
  output[1, 6]   <- summry$coefficients[2, 1]  # phi
  output[1, 7]   <- summry$coefficients[2, 2]  # phi se
  output[1, 8]   <- summry$coefficients[2, 3]  # phi t-val
  output[1, 9]   <- summry$coefficients[2, 4]  # phi p-val
  # rho
  output[1, 10]  <- summry$coefficients[3, 1]  # rho
  output[1, 11]  <- summry$coefficients[3, 2]  # rho se
  output[1, 12]  <- summry$coefficients[3, 3]  # rho t-val
  output[1, 13]  <- summry$coefficients[3, 4]  # rho p-val
  
  # statistics
  fitted.vals    <- ( output[1, 5] + output[1, 1] ) * features[,2] - 
    ( output[1, 5] + output[1, 9] * output[1, 1] ) * features[,3]
  SSE            <- sum( ( fitted.vals - mean( fitted.vals ) )^2 )
  SST            <- sum( ( features[,1] - mean( features[,1] ) )^2 )
  output[1, 14]  <- SSE / SST # R2
  output[1, 15]  <- 1 - ( result$n - 1 ) / ( result$n - 2 ) * ( 1 - output[1, 13] ) # R adj.
  output[1, 16]  <- ( result$n - 2 ) / 2 * output[1, 13] / ( 1 - output[1, 13] ) # F-stat
  output[1, 17]  <- pf( output[1, 15], result$n - 2, 2, result$n, lower.tail = FALSE ) # F p-val
  
  # starting values
  output[1, 18]  <- 0.03      # theta start
  output[1, 19]  <- 0.015     # phi start
  output[1, 20]  <- 0.2       # rho start
  
  # residuals - epsilon & eta
  epsilon        <- dmid - output[1, 1] * indicator + output[1, 9] * output[1, 1] * indicator_lag
  output[1, 21]  <- sd( epsilon ) 
  output[1, 22]  <- sd( price_diff - fitted.vals - epsilon ) # eta se
  
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
  output[1, 29]  <- mean( spread_quoted ) # quoted spread
  output[1, 30]  <- sd( spread_quoted ) # quoted spread sd
  output[1, 31]  <- sd( spread_quoted ) / sqrt( n ) # quoted spread se
  output[1, 32]  <- quantile( spread_quoted, probs = c( .5 ), type = 5 ) # med. spread quoted
  
  return( output )
}

#' Estimates the model of Madhavan, Richardson & Roomans (1997) using the approach by 
#' Theissen & Zehnder (2015)
#' 
#' @param price_diff a numeric vector containing the series of first price differences.
#' @param price a numeric vector containing the price series.
#' @param indicator an integer vector containing the trade direction with a buy as 1 and
#' a sell as -1.
#' @param indicator_lag an integer vector containing the first lag of the trade indicator series.
#' @param indicator_lag2 an integer vector containing the second lag of the trade indicator series.
#' @param midquote a numeric vector with the midquote price series.
#' @param midquote_lag a numeric vector containing the first lag of the midquote series.
#' 
#' @return A data.frame with the following values:
#' \describe{
#'  \item{n}{the number of observation used in estimation.}
#'  \item{rho}{the estimated first order autocorrelation, \eqn{\rho}, of the trade 
#'  indicator series, i.e. the parameter of the AR(1) model in step 1.}
#'  \item{rho_std}{the standard deviation of the estimated first order autocorrelation,
#'  \eqn{\rho} of step 1.}
#'  \item{a1}{the first parameter estimate of the OLS model in step 2, \eqn{\alpha_1}.}
#'  \item{a1_std}{the standard deviation of the first parameter estimate of the OLS 
#'  model in step 2, \eqn{\alpha_1}.}
#'  \item{a2}{the second parameter estimate of the OLS model in step 2, \eqn{\alpha_2}.}
#'  \item{a2_std}{the standard deviation of the second parameter estimate of the OLS 
#'  model in step 2, \eqn{\alpha_2}.}
#'  \item{psi1}{the parameter (1,2) of the VARMA model in step 3.}
#'  \item{psi1_std}{the standard deviation of the parameter (1,2) of the VARMA model 
#'  in step 3.}
#'  \item{psi2}{the parameter (2,2) of the VARMA model in step 3.}
#'  \item{psi2_std}{the standard deviation of the parameter (2,2) of the VARMA model 
#'  in step 3.}
#'  \item{varv}{the variance of the trade innovation, \eqn{\nu} from step 1.}
#'  \item{varu}{the variance of the public information arrival, \eqn{u} from step 2.}
#'  \item{covuv}{the covariance of the trade innovation, \eqn{\nu} from step 1, and
#'  the public information arrival, \eqn{u} from step 2.}
#'  \item{spread_eff_emp}{the empirical effective spread measured from the data.}
#'  \item{spread_eff_est1}{the estimation of the effective spread using the traditional 
#'  estimation approach.}
#'  \item{spread_eff_est2}{the estimation of the effective spread using the modified 
#'  estimation approach.} 
#'  \item{stable}{the stability of the estimated polynomials, i.e. if all roots are 
#'  inside the unit circle.}
#' }
#'  
#' @details The function estimates for given data the trade indicator model of 
#' \href{https://academic.oup.com/rfs/article-abstract/10/4/1035/1605749}{Madhavan, 
#' Richardson & Roomans (1997)} using the modified estimation approach by 
#' Theissen & Zehnder (2015). The latter authors use a three-step estimation approach 
#' to overcome the negative bias in the estimation of the effective spread. The major
#' part of this bias appears to come from the adverse selection component \eqn{\theta}
#' in the trade indicator model of Madhavan, Richardson & Roomans (MRR). 
#' 
#' The authors propose a three-step estimation procedure that results in an unbiased
#' estimation of both effective spread and also the adverse selection component as they
#' explicitly account for endogeneity in their model, namely a correlation between the
#' trade innovation and the public information arrival. The estimation procedure involves
#' a VARMA part that is estimated using the \code{\link{dse}}-package.
#' 
#' Standard deviations for the VARMA model (step 3) are estimated using the \emph{delta 
#' method}. 
#' 
#' @references Madhavan, Richardson & Roomans (1997), "Why Do Security Prices Change? A 
#' Transaction-Level Analysis of NYSE Stocks," Review of Financial Studies, Vol. 10, 
#' No. 4, pp. 1035-1064.
#' 
#' Zehnder & Theissen (2015), "Estimation of Trading Costs: Trade Indicator Models 
#' Revisited," unpublished.

estimate_mrr_mod <- function( price_diff, price, indicator, indicator_lag, indicator_lag2,
                              midquote, midquote_lag ) 
{
  output <- data.frame( n = integer(), rho = double(), rho_std = double(), a1 = double(), 
                        a1_std = double(), a2 = double(), a2_std = double(), psi1 = double(), 
                        psi1_std = double(), psi2 = double(), psi2_std = double(), 
                        varv = double(), varu = double(), covuv = double(), 
                        spread_eff_emp = double(), spread_eff_est1 = double(),
                        spread_eff_est2 = double(), stable = logical() )
  
  # define starting parameters
  phi             <- .01
  rho             <- .2
  
  # define the AR component of the VARMA model
  AR              <- array( c( 1, 0, 0, 0, 0, -phi * ( rho - 1 ), 1, -rho ), c( 2, 2, 2 ) )
  
  # define the MA component of the VARMA model
  MA              <- array( c( 1, 0, 0, 1 ), c( 1, 2, 2 ) )
  
  # define the VARMA model
  # as `dse` package is simply loaded but not attached to the search path 
  # a namespace has to be used
  arma            <- dse::ARMA( A = AR, B = MA, C = NULL )
  
  output[1, 1]    <- length( price_diff ) # n
  
  # estimate v_t and sigma(v_t)
  res1            <- lm( indicator ~ 0 + indicator_lag )
  vt              <- res1$residuals
  vtlag           <- vt[1:length( vt ) - 1]
  varv            <- var( vt )
  
  # calculate the midquote difference
  dmid            <- midquote - midquote_lag
  
  # bring `dmid` time series to the same length as `vtlag`
  dmid            <- dmid[2:length( dmid )]
  
  # compute second difference 
  dq              <- indicator_lag - indicator_lag2
  
  # bring `dq` time series to the same length as `vtlag`
  dq              <- dq[2:length( dq )]
  
  # estimate u_t, sigma(u_t), and cov(v_t,u_t)
  res2            <- lm( dmid ~ 0 + dq + vtlag )
  ut              <- res2$residuals
  varu            <- var( ut )
  covuv           <- cov( ut, vt[2:length( vt )] )
  
  # estimate the VARMA model
  arma_dat        <- dse::TSdata( input = NULL, output = cbind( price_diff, indicator ) )
  arma_est        <- dse::estMaxLik( arma, arma_dat )
  stable          <- dse::stability( arma_est, verbose = FALSE )
  res_cov         <- arma_est$estimates$cov
  spread_eff_emp  <- mean( indicator * ( price - midquote ) * 2, na.rm = TRUE )
  spread_eff_est1 <- res_cov[1, 2] / res_cov[2, 2] * 2
  spread_eff_est2 <- ( res_cov[1, 2] - covuv ) / res_cov[2, 2] * 2
  
  # store rho 
  output[1, 2]    <- res1$coefficients[1]                                   # rho
  
  # store HAC consistent standard errors for rho
  output[1, 3]    <- sqrt( sandwich::NeweyWest( res1 )[1, 1] )
  
  # calculate HAC consistent standard errors for a1 and a2
  step2cov        <- sqrt( diag( sandwich::NeweyWest( res2 ) ) )
  
  # store a1 and a2
  output[1, 4]    <- res2$coefficients[1]                                   # a1
  output[1, 5]    <- step2cov[1]
  output[1, 6]    <- res2$coefficients[2]                                   # a2
  output[1, 7]    <- step2cov[2]
  
  # calculate standard errors for psi1 and psi2 by inverting the hessian
  minv            <- solve( arma_est$estimates$results$hessian )
  minv            <- sqrt( diag( minv ) )
  
  # store psi1 and psi2  
  output[1, 8]    <- -arma_est$estimates$results$par[1]                     # psi1
  output[1, 9]    <- minv[1]
  output[1, 10]   <- -arma_est$estimates$results$par[2]                     # psi2
  output[1, 11]   <- minv[2]
  
  # store var(u_t), var(v_t), and cov(u_t,v_t)
  output[1, 12]   <- varv                                                   # varv
  output[1, 13]   <- varu                                                   # varu
  output[1, 14]   <- covuv
  
  # store spread estimates
  output[1, 15]   <- spread_eff_emp       # spread_eff_emp
  output[1, 16]   <- spread_eff_est1      # spread est from MRR
  output[1, 17]   <- spread_eff_est2      # spread est from MRR modified
  
  # store stability classifier
  output[1, 18]   <- stable
  
  return( output ) 
}