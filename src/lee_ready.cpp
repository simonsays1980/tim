#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
IntegerVector lee_ready_vector( NumericVector price, NumericVector bidprice, 
                                NumericVector askprice ) {
  const int nrows = price.length();
  if ( nrows != bidprice.length() || nrows != askprice.length() ) {
    throw std::invalid_argument( "Arguments differ in lengths" );
  }
  IntegerVector indicator( nrows );
  NumericVector midprice = ( askprice + bidprice ) / 2.0;
  
  for( int i = 2; i <= nrows; ++i ) {
    if ( price[i] == askprice[i] ) {
      indicator[i] = 1;
    } else if ( price[i] == bidprice[i] ) {
      indicator[i] = -1;
    } else {
      if ( price[i] > midprice[i] ) {
        indicator[i] = 1;
      } else if ( price[i] < midprice[i] ) {
        indicator[i] = -1;
      } else { 
        /* price == midpice */
        if ( price[i] > price[i-1] ) {
          indicator[i] = 1;
        } else if ( price[i] < price[i-1] ) {
          indicator[i] = -1;
        } else {
          if ( price[i] > price[i-2] ) {
            indicator[i] = 1;
          } else {
            indicator[i] = -1;
          }
        }
      }
    }
  }
  return indicator;
}