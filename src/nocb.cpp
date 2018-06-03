#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector nocb_1min_vector( NumericVector &price, newDatetimeVector &tradetime ) {
  
  const int nrows = price.length();
  NumericVector price1( nrows, NumericVector::get_na() ); 
  Datetime datetime( nrows );
  int j = 0;
  for ( int i = 0; i < nrows; ++i ) {
    // shift by 1 minute
    datetime = tradetime[i] + 60;
    for ( j = i+1; j < nrows; ++j ) {
      if ( Datetime( tradetime[j] ) > datetime ) {
        price1[i] = price[j];
        break;
      } else {
        continue;
      }
    }
  }
  
  return price1;
}

// [[Rcpp::export]]
DataFrame nocb_1min_dataframe( NumericVector &price, newDatetimeVector &tradetime ) {

  const int nrows = price.length();
  NumericVector price1( nrows, NumericVector::get_na() );
  newDatetimeVector tradetime1 = rep( newDatetimeVector::get_na(), nrows );
  //newDatetimeVector tradetime5( nrows );
  Datetime datetime( "1970-01-01 00:00:00" );
  int j = 0;
  
  for ( int i = 0; i < nrows; ++i ) {
    // shift by 5 minutes
    datetime = tradetime[i] + 60;
    for ( j = i+1; j < nrows; ++j ) {
      if ( Datetime( tradetime[j] ) > datetime ) {
        price1[i] = price[j];
        tradetime1[i] = tradetime[j];
        break;
      } else {
        continue;
      }
    }
  }
  DataFrame tradeframe = DataFrame::create( Named( "price1" )    = price1, 
                                            Named( "datetime1" ) = tradetime1 );
  return tradeframe;
}

// [[Rcpp::export]]
NumericVector nocb_5min_vector( NumericVector &price, newDatetimeVector &tradetime ) {
  
  const int nrows = price.length();
  NumericVector price5( nrows, NumericVector::get_na() ); 
  Datetime datetime( nrows );
  int j = 0;
  for ( int i = 0; i < nrows; ++i ) {
    // shift by 5 minutes
    datetime = tradetime[i] + 5 * 60;
    for ( j = i+1; j < nrows; ++j ) {
      if ( Datetime( tradetime[j] ) > datetime ) {
        price5[i] = price[j];
        break;
      } else {
        continue;
      }
    }
  }
  
  return price5;
}

// [[Rcpp::export]]
DataFrame nocb_5min_dataframe( NumericVector &price, newDatetimeVector &tradetime ) {
  
  const int nrows = price.length();
  NumericVector price5( nrows, NumericVector::get_na() );
  newDatetimeVector tradetime5 = rep( newDatetimeVector::get_na(), nrows );
  //newDatetimeVector tradetime5( nrows );
  Datetime datetime( "1970-01-01 00:00:00" );
  int j = 0;
  
  for ( int i = 0; i < nrows; ++i ) {
    // shift by 5 minutes
    datetime = tradetime[i] + 5 * 60;
    for ( j = i+1; j < nrows; ++j ) {
      if ( Datetime( tradetime[j] ) > datetime ) {
        price5[i] = price[j];
        tradetime5[i] = tradetime[j];
        break;
      } else {
        continue;
      }
    }
  }
  DataFrame tradeframe = DataFrame::create( Named( "price5" )    = price5, 
                                            Named( "datetime5" ) = tradetime5 );
  return tradeframe;
}
