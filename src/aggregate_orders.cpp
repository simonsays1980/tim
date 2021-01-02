#include <Rcpp.h>
using namespace Rcpp;


//[[Rcpp::export]]
DataFrame test_func( DataFrame table ){

  DatetimeVector timestamp = table["timestamp"];
  DatetimeVector out( 1 );
  //DatetimeVector out( timestamp.length() );
  out[0] = timestamp[0];

  for (int i = 1; i < timestamp.length(); ++i){
    out.push_back( timestamp[i] );
    //out[i] = timestamp[i];
  }
  
  
  return DataFrame::create( Named( "timestamp_out", out.getDatetimes() ) );
}
// [[Rcpp::export]]
DataFrame aggregate_orders_tbl( DataFrame table ) {
  
  // Input
  DatetimeVector timestamp_in    = table["timestamp"] ;
  NumericVector aggr_order_id_in = table["aggr_order_id"];
  NumericVector buysell_in       = table["buysell"];
  NumericVector midquote_in      = table["midquote"];
  NumericVector price_in         = table["price"];
  IntegerVector trade_in         = table["trade"];
  NumericVector volume_in        = table["volume"];
  CharacterVector wkn_in         = table["wkn"];
  
  // Output
  const int n = 1;
  DatetimeVector timestamp_out( n );
  NumericVector aggr_order_id_out( n );
  NumericVector buysell_out( n );
  NumericVector midquote_out( n );
  NumericVector price_out( n );
  IntegerVector trade_out( n );
  NumericVector volume_out( n );
  CharacterVector wkn_out( n );
  
  // First row
  timestamp_out[0]     = timestamp_in[0];
  aggr_order_id_out[0] = aggr_order_id_in[0];
  buysell_out[0]       = buysell_in[0];
  midquote_out[0]      = midquote_in[0];
  price_out[0]         = price_in[0];
  trade_out[0]         = trade_in[0];
  volume_out[0]        = volume_in[0];
  wkn_out[0]           = wkn_in[0];
  
  // Aggregate orders
  const int n_in = price_in.size();
  int order_count = 1;
  int total_count = 0;
  for ( int i = 1; i < n_in; ++i ) {
    Datetime dt     = timestamp_in[i];
    Datetime dt_lag = timestamp_in[i-1];
    
    if ( wkn_in[i] == wkn_in[i-1] &&
         dt.getYearday() == dt_lag.getYearday() &&
         aggr_order_id_in[i] != NA_REAL &&
         aggr_order_id_in[i] == aggr_order_id_in[i-1] &&
         trade_in[i] != 0 &&
         buysell_in[i] == buysell_in[i-1] ) {
      // No further row gets appended
      order_count += 1;
      total_count += 1;
      price_out[i-total_count] = price_out[i-total_count] + price_in[i];
      volume_out[i-total_count] = volume_out[i-total_count] + volume_in[i];
    } else {
      // further row gets appended
      if ( order_count > 1 ) {
        price_out[i-total_count-1] = price_out[i-total_count-1] / order_count;
        order_count = 1;
      }
      // Append all input values to the vector output.
      timestamp_out.push_back( dt );
      aggr_order_id_out.push_back( aggr_order_id_in[i] );
      buysell_out.push_back( buysell_in[i] );
      // If entry i is a quote use the midpoint otherwise 
      // use the last midquote.
      if ( trade_in[i] == 0 ) {
        midquote_out.push_back( midquote_in[i] );
      } else {
        midquote_out.push_back( midquote_out[i-total_count-1] );
      }
      price_out.push_back( price_in[i] );
      trade_out.push_back( trade_in[i] );
      volume_out.push_back( volume_in[i] );
      wkn_out.push_back( wkn_in[i] );
    }
  }
  
  // for( int i = 0; i < price_out.length(); ++i) {
  //   Rcout << "=======\n";
  //   Rcout << "i: " << i << "\n";
  //   Rcout << "total_count: " << total_count << "\n";
  //   Rcout << "timestamp: " << timestamp_out[i] << "\n";
  //   Rcout << "aggr_order_id: " << aggr_order_id_out[i] << "\n";
  //   Rcout << "buysell: " << buysell_out[i] << "\n";
  //   Rcout << "midquote: " << midquote_out[i] << "\n";
  //   Rcout << "price: " << price_out[i] << "\n";
  //   Rcout << "volume: " << volume_out[i] << "\n";
  //   Rcout << "wkn: " << wkn_out[i] << "\n\n";
  // }
  
  return DataFrame::create( Named( "timestamp" )     = timestamp_out.getDatetimes(),
                                           Named( "aggr_order_id" ) = aggr_order_id_out,
                                           Named( "buysell" )       = buysell_out,
                                           Named( "midquote" )      = midquote_out,
                                           Named( "price" )         = price_out,
                                           Named( "trade" )         = trade_out,
                                           Named( "volume" )        = volume_out,
                                           Named( "wkn" )           = wkn_out,
                                           Named( "stringsAsFactors") = false );
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
