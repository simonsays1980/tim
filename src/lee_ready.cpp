#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector lee_ready_vector( NumericVector &price, NumericVector &bidprice, 
                                NumericVector &askprice ) {
  const int nrows = price.length();
  IntegerVector indicator( nrows );
  if ( nrows < 3 ) {
    return indicator;
  }
  if ( nrows != bidprice.length() || nrows != askprice.length() ) {
    throw std::invalid_argument( "Arguments differ in lengths" );
  }
  
  NumericVector midprice = ( askprice + bidprice ) / 2.0;
  
  for( int i = 0; i < nrows - 2; ++i ) {
    if ( price[i+2] == askprice[i+2] ) {
      indicator[i+2] = 1;
    } else if ( price[i+2] == bidprice[i+2] ) {
      indicator[i+2] = -1;
    } else {
      if ( price[i+2] > midprice[i+2] ) {
        indicator[i+2] = 1;
      } else if ( price[i+2] < midprice[i+2] ) {
        indicator[i+2] = -1;
      } else { 
        /* price == midpice */
        if ( price[i+2] > price[i+1] ) {
          indicator[i+2] = 1;
        } else if ( price[i+2] < price[i+1] ) {
          indicator[i+2] = -1;
        } else {
          if ( price[i+2] > price[i] ) {
            indicator[i+2] = 1;
          } else {
            indicator[i+2] = -1;
          }
        }
      }
    }
  }
  
  return indicator;
}