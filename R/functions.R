#' Compute price impact
#'
#' \code{price_impact} calculates the price impact for a certain lead \eqn{x}. 
#' 
#' @param indicator an integer vector or numeric vector for \eqn{t}.
#' @param price a numeric vector holding the prices for \eqn{t}.
#' @param price_lead a numeric vector holding the leading prices for \eqn{t+x}.
#' 
#' @details This function takes intraday data from a financial exchange and calculates the 
#' price impact defined \eqn{ PI = \frac{1}{T}\sum_{t=1}^Tq_t(m_{t+x}-m_t) }, where 
#' \eqn{ q_t } is the price indicator at time \eqn{ t }, \eqn{ m_t } is the midquote 
#' at time\eqn{ t } and \eqn{ x } is a certain time interval, e.g. 1 or 5 minutes. 
#' So, the indicator time always corresponds to the starting midquote \eqn{ m_t }. 
#' All price impacts are calculated in US $-Cents.
#' 
#' The function uses internally makes use of \code{\link{mean}} and excluding all 
#' NA values (making use of its input argument \code{na.rm = TRUE}). Note that 
#' the user has to prepare the time series data, i.e. calculate for the \code{price} 
#' data with index \eqn{t} the corresponding \code{price_lead} with index \eqn{t+x}.

price_impact <- function( indicator, price, price_lead ) {
  # calculate price impact and remove NAs
  impact <- mean( indicator * ( price_lead - price ), na.rm = TRUE )  
  return( impact )
}

#' Calculates the 1-minute price lead
#' 
#' @param price a numeric vector containing the price series.
#' @param tradetime a POSIXct vector containing the time stamps of the price series. 
#' 
#' @return a numeric vector containing the 1-minute price lead.
#' 
#' @details This function calculates the 1-minute lead price corresponding to each 
#' of the entries in \code{price}. The return value has the same length as the input 
#' vector to ensure compatibility inside of e.g. a \code{data.frame}. 
#' 
#' The \code{\link{POSIXct}} datetime format is a requirement as this function runs 
#' fast C++-code in the background that expects a double format which is provided by
#' the POSIXct format (see herefore the 
#' \href{http://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1Datetime.html}{Rcpp::Datetime}). 
#' 
#' For matching an NOCB (next observation carried backwards) approach is used, i.e. 
#' if for a price with certain timestamp there exists 1-minute price lead, the next 
#' >1-minute price lead is used. This leads possibly to repeating price lead entries. 
#' Furthermore, as the length of the return vector is the same as the input vector,
#' the last entries of the return vector are usually \code{NA} values.
#' 
#' If a timestamp is required for reasons of documentation or testing 
#' it is referred to the \code{data.frame}-equivalent \code{\link{nocb_1min_df}}, 
#' which returns next to the 1-minute lead price series in addition the 1-minute 
#' lead timestamp.
#' 
#' @seealso \code{\link{nocb_1min_df}}, \code{\link{nocb_5min_vec}}, \code{\link{nocb_5min_df}}

nocb_1min_vec <- function( price, time_stamp ) {
  return( nocb_1min_vector( price, time_stamp ) )
} 

#' Calculates the 1-minute price lead and corresponding timestamp
#' 
#' @param price a numeric vector containing the price series.
#' @param tradetime a POSIXct vector containing the time stamps of the price series. 
#' 
#' @return a data.frame containing the 1-minute price lead and corresponding timestamp.
#' 
#' @details This function calculates the 1-minute lead price corresponding to each 
#' of the entries in \code{price}. The return value has the same length as the input 
#' vector to ensure compatibility inside of e.g. a \code{data.frame}. 
#' 
#' The \code{\link{POSIXct}} datetime format is a requirement as this function runs 
#' fast C++-code in the background that expects a double format which is provided by
#' the \code{POSIXct} format  (see herefore the 
#' \href{http://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1Datetime.html}{Rcpp::Datetime}). 
#' 
#' For matching an NOCB (next observation carried backwards) approach is used, i.e. 
#' if for a price with certain timestamp there exists 1-minute price lead, the next 
#' >1-minute price lead is used. This leads possibly to repeating price lead entries. 
#' Furthermore, as the length of the return vector is the same as the input vector,
#' the last entries of the return vector are usually \code{NA} values.
#' 
#' If no timestamp is required it is referred to the \code{vector}-equivalent 
#' \code{\link{nocb_1min_vec}}, which returns next to the 1-minute lead price 
#' series in addition the 1-minute lead timestamp.
#' 
#' @seealso \code{\link{nocb_1min_vec}}, \code{\link{nocb_5min_vec}}, \code{\link{nocb_5min_df}}

nocb_1min_df <- function( price, time_stamp ) {
  return( nocb_1min_dataframe( price, time_stamp ) )
}

#' Calculates the 5-minute price lead
#' 
#' @param price a numeric vector containing the price series.
#' @param tradetime a POSIXct vector containing the time stamps of the price series. 
#' 
#' @return a numeric vector containing the 5-minute price lead.
#' 
#' @details This function calculates the 5-minute lead price corresponding to each 
#' of the entries in \code{price}. The return value has the same length as the input 
#' vector to ensure compatibility inside of e.g. a \code{data.frame}. 
#' 
#' The \code{\link{POSIXct}} datetime format is a requirement as this function runs 
#' fast C++-code in the background that expects a double format which is provided by
#' the \code{POSIXct} format  (see herefore the 
#' \href{http://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1Datetime.html}{Rcpp::Datetime}). 
#' 
#' For matching an NOCB (next observation carried backwards) approach is used, i.e. 
#' if for a price with certain timestamp there exists 5-minute price lead, the next 
#' >5-minute price lead is used. This leads possibly to repeating price lead entries. 
#' Furthermore, as the length of the return vector is the same as the input vector,
#' the last entries of the return vector are usually \code{NA} values.
#' 
#' If a timestamp is required for reasons of documentation or testing 
#' it is referred to the \code{data.frame}-equivalent \code{\link{nocb_5min_df}}, 
#' which returns next to the 5-minute lead price series in addition the 5-minute 
#' lead timestamp.
#' 
#' @seealso \code{\link{nocb_5min_df}}, \code{\link{nocb_1min_vec}}, \code{\link{nocb_1min_df}}

nocb_5min_vec <- function( price, time_stamp ) {
  return( nocb_5min_vector( price, time_stamp ) )
} 

#' Calculates the 5-minute price lead and corresponding timestamp
#' 
#' @param price a numeric vector containing the price series.
#' @param tradetime a POSIXct vector containing the time stamps of the price series. 
#' 
#' @return a data.frame containing the 5-minute price lead and corresponding timestamp.
#' 
#' @details This function calculates the 5-minute lead price corresponding to each 
#' of the entries in \code{price}. The return value has the same length as the input 
#' vector to ensure compatibility inside of e.g. a \code{data.frame}. 
#' 
#' The \code{\link{POSIXct}} datetime format is a requirement as this function runs 
#' fast C++-code in the background that expects a double format which is provided by
#' the \code{POSIXct} format  (see herefore the 
#' \href{http://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1Datetime.html}{Rcpp::Datetime}). 
#' 
#' For matching an NOCB (next observation carried backwards) approach is used, i.e. 
#' if for a price with certain timestamp there exists 5-minute price lead, the next 
#' >5-minute price lead is used. This leads possibly to repeating price lead entries. 
#' Furthermore, as the length of the return vector is the same as the input vector,
#' the last entries of the return vector are usually \code{NA} values.
#' 
#' If no timestamp is required it is referred to the \code{vector}-equivalent 
#' \code{\link{nocb_5min_vec}}, which returns next to the 5-minute lead price 
#' series in addition the 1-minute lead timestamp.
#' 
#' @seealso \code{\link{nocb_5min_vec}}, \code{\link{nocb_1min_vec}}, \code{\link{nocb_1min_df}}

nocb_5min_df <- function( price, time_stamp ) {
  return( nocb_5min_dataframe( price, time_stamp ) )
}

#' Applies the Lee & Ready (1991) algorithm
#' 
#' @param price a numeric vector containing the transaction price series.
#' @param bid_price a numeric vector containing the bid price series.
#' @param ask_price a numeric vector containing the ask price series. 
#' 
#' @return an integer vector containing the trade direction with a buy 
#' indicated as 1 and a sell indicated as -1
#' 
#' @details This function applies the algorithm by 
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1540-6261.1991.tb02683.x}{Lee & Ready (1991)}
#' to infer trade direction from prices. The function runs fast C++-code in the 
#' background using the \code{\link{Rcpp}} functionality. Note that the 
#' length of all input vectors have to be the same, otherwise the C++-code 
#' throws an exception. 
#' 
#' @references Lee and Ready (1991), "Inferring Trade Direction from Intraday Data," 
#' Journal of Finance, Vol. 42, Issue 2 

lee_ready_vec <- function( price, bid_price, ask_price ) {
  return( lee_ready_vector( price, bid_price, ask_price ) )
}