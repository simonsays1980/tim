#' Estimates the model of Huang & Stoll (1997)
#' 
#' @param price_diff a numeric vector containing the series of first price differences.
#' @param price a numeric vector containing the price series.
#' @param indicator an integer vector containing the trade direction with a buy as 1 and
#' a sell as -1.
#' @param indicator_lag an integer vector containing the first lag of the indicator series.
#' @param midquote a numeric vector with the midquote price series.
#' 
#' @return A data.frame with the following values:
#' \describe{
#'  \item{n}{the number of observation used in estimation.}
#'  \item{beta1}{the effective half-spread.}
#'  \item{beta1_std}{the standard deviation of the effective half-spread.}
#'  \item{beta1_t}{the t-value of the effective half-spread.}
#'  \item{beta1_p}{the p-value of the effective half-spread.}
#'  \item{beta2}{the part of the spread due to adverse selection and inventory.}
#'  \item{beta2_std}{the standard deviation of the adverse selection and inventory\cr
#'  spread component.}
#'  \item{beta2_t}{the t-value of the adverse selection and inventory\cr 
#'  spread component.}
#'  \item{beta2_p}{the p-value of the adverse selection and inventory\cr 
#'  spread component.}
#'  \item{r2}{the coefficient of determination.}
#'  \item{r2_adj}{the adjusted coefficient of determination.}
#'  \item{f_stat}{the value of the F-statistic.}
#'  \item{f_pval}{the p-value of the F-statistic.}
#'  \item{spread_eff_est}{the effective spread estimated from the model using the 
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
#' }
#'  
#' @details The function estimates for given data the trade indicator model of 
#' \href{https://academic.oup.com/rfs/article-abstract/10/4/995/1605656}{Huang & Stoll (1997)}. 
#' For estimation an OLS approach is used similar to the one desribed in the paper of the two 
#' authors. For application the \code{\link{lm}}-function is used together with
#' \code{\link[sandwich]{NeweyWest}} for NeweyWest standard errors. For details
#' it is referred to the \code{\link[sandwich]{NeweyWest}}-function. 
#' 
#' @references Huang & Stoll (1997), "The Component of the Bif-Ask Spread: A 
#' General Approach," The Review of Financial Studies, Vol. 10, Issue 4., pp. 995-1034.

estimate_hs <- function( price_diff, price, indicator, indicator_lag, midquote )
{
  # compute midquote difference
  indicator_diff      <- indicator - indicator_lag
  
  # initialize output
  output <- data.frame( n = integer(), beta1 = double(), beta1_se = double(), beta1_t = double(),
                        beta1_p = double(), beta2 = double(), beta2_se = double(), beta2_t = double(),
                        beta2_p = double(), phi = double(), phi_se = double(), r2 = double(), 
                        r2_adj = double(), f_stat = double(), f_pval = double(), 
                        spread_eff_emp = double(), spread_eff_emp_std = double(), 
                        spread_eff_emp_se = double(), spread_eff_emp_med = double(), 
                        spread_eff_est = double(), spread_eff_est_se = double() )
                                          
  # estimate model with Newey West standard errors
  res           <- lm( price_diff ~ 0 + indicator_diff + indicator_lag )  
  summry        <- summary( res )
  nwvcov        <- sandwich::NeweyWest( res )
  nwstderr      <- sqrt( diag( nwvcov ) )
  
  # number of observations 
  output[1, 1]  <- length( price_diff )                               # number of observations 
  
  # beta_1
  output[1, 2]  <- res$coefficients[1]                                # beta_1                                    
  output[1, 3]  <- nwstderr[1]                                        # stderr( beta_1 )
  output[1, 4]  <- output[1, 2] / output[1, 3]                        # t( beta_1 )
  output[1, 5]  <- 2 * pt( abs( output[1, 4] ), 
                           df = output[1, 1] - 2, 
                           lower.tail = FALSE ) # p( beta_1 )
  
  # beta_2
  output[1, 6]  <- res$coefficients[2]                                # beta_2 
  output[1, 7]  <- nwstderr[2]                                        # stderr( beta_2 ) 
  output[1, 8]  <- output[1, 6] / output[1, 7]                        # t( beta_2 )
  output[1, 9]  <- 2 * pt( abs( output[1, 8] ), 
                           df = output[1, 1] - 2, 
                           lower.tail = FALSE ) # p( beta_2 )
  
  # phi
  output[1, 10] <- res$coefficients[1] - res$coefficients[2]          # phi
  nabla         <- as.matrix( c( 1, -1 ) )
  output[1, 11] <- sqrt( t( nabla ) %*% nwvcov %*% nabla )            # stderr( phi )
  
  # statistics 
  output[1, 12] <- summry$r.squared                                   # R squared
  output[1, 13] <- summry$adj.r.squared                               # adj. R squared
  output[1, 14] <- summry$fstatistic[1]                               # F-statistic
  output[1, 15] <- pf( summry$fstatistic[1], df1 = summry$df[1],      
                       df2 = summry$df[2], lower.tail = FALSE )       # p( F-statistic )
  # spread
  spreads       <- indicator * ( price - midquote ) * 2 
  output[1, 16] <- mean( spreads, na.rm = TRUE )                      # emp. eff. spread
  output[1, 17] <- sd( spreads, na.rm = TRUE  )                       # std( emp. eff. spread )
  output[1, 18] <- sd( spreads, na.rm = TRUE ) / sqrt( output[1, 1] ) # stderr( emp. eff. spread )
  output[1, 19] <- quantile( spreads, probs = c( .5 ), type = 5 )     # med( emp. eff. spread )
  output[1, 20] <- res$coefficients[1] * 2                            # est. spread 
  output[1, 21] <- sqrt( 2 ) * output[1, 3]                           # stderr( est. spread )
  
  
  return ( output )
}

#' Estimates the model of Huang & Stoll (1997) using the approach by 
#' Theissen & Zehnder (2015)
#' 
#' @param price_diff a numeric vector containing the series of first price differences.
#' @param price a numeric vector containing the price series.
#' @param indicator an integer vector containing the trade direction with a buy as 1 and
#' a sell as -1.
#' @param indicator_lag an integer vector containing the first lag of the trade indicator series.
#' @param midquote a numeric vector with the midquote price series.
#' @param midquote_lag a numeric vector containing the first lag of the midquote series.
#' 
#' @return A data.frame with the following values:
#' \describe{
#'  \item{n}{the number of observation used in estimation.}
#'  \item{a}{the first parameter estimate of the OLS model in step 1, \eqn{\alpha}.}
#'  \item{a_std}{the standard deviation of the first parameter estimate of the OLS 
#'  model in step 1, \eqn{\alpha}.}
#'  \item{a_t}{the t-value of the first parameter estimate of the OLS  model in step 1, 
#'  \eqn{\alpha}.}
#'  \item{a_pval}{the p-value of the first parameter estimate of the OLS  model in step 1, 
#'  \eqn{\alpha}.}
#'  \item{psi12}{the parameter (1,2) of the VARMA model in step 2.}
#'  \item{psi12_std}{the standard deviation of the parameter (1,2) of the VARMA model 
#'  in step 2.}
#'  \item{psi12_tval}{the t-value of the parameter (1,2) of the VARMA model in step 3.}
#'  \item{psi12_pval}{the p-value of the paramater (1,2) of the VARMA model in step 3.}
#'  \item{varv}{the variance of the trade innovation, \eqn{\nu} from step 1.}
#'  \item{varu}{the variance of the public information arrival, \eqn{u} from step 2.}
#'  \item{covuv}{the covariance of the trade innovation, \eqn{\nu} from step 1, and
#'  the public information arrival, \eqn{u} from step 2.}
#'  \item{spread_eff_est1}{the estimation of the effective spread using the traditional 
#'  estimation approach.}
#'  \item{spread_eff_est2}{the estimation of the effective spread using the modified 
#'  estimation approach.} 
#'  \item{spread_eff_emp}{the empirical effective spread measured from the data.}
#'  \item{spread_eff_emp_std}{the standard deviation of the empirical effective spread calculated 
#'  as the standard deviation of the series \eqn{q_t(p_t-m_t)}.}
#'  \item{spread_eff_emp_se}{the standard error of the estimated empirical effective spread 
#'  using the formula \eqn{SE=STD/\sqrt n}.}
#'  \item{spread_eff_emp_med}{the median of the empirical effective spread calculated as the 
#'  median of the series \eqn{q_t(p_t-m_t)}.}
#'  \item{stable}{the stability of the estimated polynomials, i.e. if all roots are 
#'  inside the unit circle.}
#' }
#'  
#' @details The function estimates for given data the trade indicator model of 
#' \href{https://academic.oup.com/rfs/article-abstract/10/4/995/1605656}{Huang & 
#' Stoll (1997)} using the modified estimation approach by 
#' Theissen & Zehnder (2015). The latter authors use a two-step estimation approach 
#' to overcome the negative bias in the estimation of the effective spread. The major
#' part of this bias appears to come from the adverse selection component \eqn{\theta}
#' in the trade indicator model of Huang & Stoll (HS). 
#' 
#' The authors propose a two-step estimation procedure that results in a less biased
#' estimation of both, the effective spread and the adverse selection component, as they
#' explicitly account for endogeneity in their model, namely a correlation between the
#' trade innovation and the public information arrival. The estimation procedure involves
#' a VARMA part that is estimated using the \code{\link{dse}}-package.
#' 
#' Standard deviations for the VARMA model (step 2) are estimated using the \emph{delta 
#' method}. 
#' 
#' @references Huang & Stoll (1997), "The Component of the Bif-Ask Spread: A 
#' General Approach," The Review of Financial Studies, Vol. 10, Issue 4., pp. 995-1034.
#' 
#' Zehnder & Theissen (2015), "Estimation of Trading Costs: Trade Indicator Models 
#' Revisited," unpublished.

estimate_hs_mod <- function( price_diff, price, indicator, indicator_lag, midquote, midquote_lag )
{
  # define data.frame to store results
  output <- data.frame( n = integer(), a = double(), a_se = double(), a_tval = double(), 
                        a_pval = double(), phi12 = double(), phi12_std = double(), 
                        phi12_tval = double(), phi12_pval = double(), varv = double(), 
                        varu = double(), covuv = double(), spread_eff_est1 = double(), 
                        spread_eff_est2 = double(), spread_eff_emp = double(), 
                        spread_eff_emp_std = double(), spread_eff_emp_se = double(), 
                        spread_eff_med = double(), stable = logical() )
  
  # define start parameters
  phi               <- 0.01
  theta             <- 0.03
  
  # define the VARMA model
  AR                <- array( c( 1, 0, 0, 0, 0, -( theta - phi ), 1, 0 ), c( 2, 2, 2 ) )
  MA                <- array( c( 1, 0, 0, 1 ), c( 1, 2, 2 ) )
  arma              <- dse::ARMA( A = AR, B = MA, C = NULL )
  
  # estimate variance of v_t (v_t itself is identical to indicator)
  varv              <- var( indicator )     
  
  # calculate the first midquote difference
  dmid              <- midquote - midquote_lag
  # estimate u_t, sigma(u_t) and cov(v_t,u_t)
  res               <- lm( dmid ~ 0 + indicator_lag )
  ut                <- res$residuals
  varu              <- var( ut )
  covuv             <- cov( ut, indicator )
  
  # estimate VARMA 
  arma_dat          <- dse::TSdata( input = NULL, output = cbind( price_diff, indicator ) )
  arma_est          <- dse::estMaxLik( arma, arma_dat )
  stable            <- dse::stability( arma_est, verbose = FALSE )
  res_cov           <- arma_est$estimates$cov
  
  # calculate spread and spread estimates
  spreads           <- indicator * ( price - midquote ) * 2
  spread_eff_emp    <- mean( spreads, na.rm = TRUE )
  spread_eff_est1   <- res_cov[1, 2] / res_cov[2, 2] * 2
  spread_eff_est2   <- ( res_cov[1, 2] - covuv ) / res_cov[2, 2] * 2
  
  # store results
  output[1, 1]      <- length( price_diff )                           # number of observations 
  
  # a
  output[1, 2]      <- res$coefficients[1]                            # a
  
  ## estimate Newey West standard errors
  output[1, 3]      <- sqrt( sandwich::NeweyWest( res )[1, 1] )       # se( a )
  output[1, 4]      <- output[1, 2] / output[1, 3]                    # t( a ) 
  output[1, 5]      <- 2 * pt( abs( output[1, 4] ), 
                               df = output[1, 1] - 1, 
                               lower.tail = FALSE )                   # p( a )
  
  # phi12
  output[1, 6]      <- -arma_est$estimates$results$par[1] # phi12
  
  ## estimate standard error via the inverse hessian
  minv              <- solve( arma_est$estimates$results$hessian )
  minv              <- sqrt( diag( minv ) ) 
  output[1, 7]      <- minv[1]                                        # se( phi12 )
  output[1, 8]      <- output[1, 6] / output[1, 7]                    # t( phi12 )
  output[1, 9]      <- 2 * pt( abs( output[1, 8] ), 
                               df = output[1, 1] - 2, 
                               lower.tail = FALSE )                   # p( phi12 ) 
  
  ## store var( v_t ), var( u_t ), cov( u_t, v_t )
  output[1, 10]     <- varv                                           # v( v_t )
  output[1, 11]     <- varu                                           # v( u_t )
  output[1, 12]     <- covuv                                          # cov( u_t,v_t )
  
  ## store spread and spread estimates
  output[1, 13]     <- spread_eff_est1                                # est. eff. spread from HS model
  output[1, 14]     <- spread_eff_est2                                # est. eff. spread from mod. HS model
  output[1, 15]     <- spread_eff_emp                                 # empirical eff. spread
  output[1, 16]     <- sd( spreads )                                  # std( empirical eff. spread )
  output[1, 17]     <- output[1, 14] / sqrt( output[1, 1] )           # stderr( empirical eff. spread )
  output[1, 18]     <- quantile( spreads, probs = c( .5 ), type = 5 ) # med( emp. eff. spread )
  
  
  ## store stability classifier from VARMA
  output[1, 19]     <- stable[1]                                      # stability clasifier 
  
  return( output )
}