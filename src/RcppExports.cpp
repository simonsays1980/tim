// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// test_func
DataFrame test_func(DataFrame table);
RcppExport SEXP _tim_test_func(SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(test_func(table));
    return rcpp_result_gen;
END_RCPP
}
// aggregate_orders_tbl
DataFrame aggregate_orders_tbl(DataFrame table);
RcppExport SEXP _tim_aggregate_orders_tbl(SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(aggregate_orders_tbl(table));
    return rcpp_result_gen;
END_RCPP
}
// lee_ready_vector
IntegerVector lee_ready_vector(NumericVector& price, NumericVector& bidprice, NumericVector& askprice);
RcppExport SEXP _tim_lee_ready_vector(SEXP priceSEXP, SEXP bidpriceSEXP, SEXP askpriceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type price(priceSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type bidprice(bidpriceSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type askprice(askpriceSEXP);
    rcpp_result_gen = Rcpp::wrap(lee_ready_vector(price, bidprice, askprice));
    return rcpp_result_gen;
END_RCPP
}
// nocb_1min_vector
NumericVector nocb_1min_vector(NumericVector& price, newDatetimeVector& tradetime);
RcppExport SEXP _tim_nocb_1min_vector(SEXP priceSEXP, SEXP tradetimeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type price(priceSEXP);
    Rcpp::traits::input_parameter< newDatetimeVector& >::type tradetime(tradetimeSEXP);
    rcpp_result_gen = Rcpp::wrap(nocb_1min_vector(price, tradetime));
    return rcpp_result_gen;
END_RCPP
}
// nocb_1min_dataframe
DataFrame nocb_1min_dataframe(NumericVector& price, newDatetimeVector& tradetime);
RcppExport SEXP _tim_nocb_1min_dataframe(SEXP priceSEXP, SEXP tradetimeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type price(priceSEXP);
    Rcpp::traits::input_parameter< newDatetimeVector& >::type tradetime(tradetimeSEXP);
    rcpp_result_gen = Rcpp::wrap(nocb_1min_dataframe(price, tradetime));
    return rcpp_result_gen;
END_RCPP
}
// nocb_5min_vector
NumericVector nocb_5min_vector(NumericVector& price, newDatetimeVector& tradetime);
RcppExport SEXP _tim_nocb_5min_vector(SEXP priceSEXP, SEXP tradetimeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type price(priceSEXP);
    Rcpp::traits::input_parameter< newDatetimeVector& >::type tradetime(tradetimeSEXP);
    rcpp_result_gen = Rcpp::wrap(nocb_5min_vector(price, tradetime));
    return rcpp_result_gen;
END_RCPP
}
// nocb_5min_dataframe
DataFrame nocb_5min_dataframe(NumericVector& price, newDatetimeVector& tradetime);
RcppExport SEXP _tim_nocb_5min_dataframe(SEXP priceSEXP, SEXP tradetimeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type price(priceSEXP);
    Rcpp::traits::input_parameter< newDatetimeVector& >::type tradetime(tradetimeSEXP);
    rcpp_result_gen = Rcpp::wrap(nocb_5min_dataframe(price, tradetime));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tim_test_func", (DL_FUNC) &_tim_test_func, 1},
    {"_tim_aggregate_orders_tbl", (DL_FUNC) &_tim_aggregate_orders_tbl, 1},
    {"_tim_lee_ready_vector", (DL_FUNC) &_tim_lee_ready_vector, 3},
    {"_tim_nocb_1min_vector", (DL_FUNC) &_tim_nocb_1min_vector, 2},
    {"_tim_nocb_1min_dataframe", (DL_FUNC) &_tim_nocb_1min_dataframe, 2},
    {"_tim_nocb_5min_vector", (DL_FUNC) &_tim_nocb_5min_vector, 2},
    {"_tim_nocb_5min_dataframe", (DL_FUNC) &_tim_nocb_5min_dataframe, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
