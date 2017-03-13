// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// count_unique_geno
IntegerVector count_unique_geno(StringMatrix g);
RcppExport SEXP qtl2convert_count_unique_geno(SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< StringMatrix >::type g(gSEXP);
    __result = Rcpp::wrap(count_unique_geno(g));
    return __result;
END_RCPP
}
// encode_geno
StringMatrix encode_geno(const StringMatrix& g, const StringMatrix& old_values, const StringVector& new_values);
RcppExport SEXP qtl2convert_encode_geno(SEXP gSEXP, SEXP old_valuesSEXP, SEXP new_valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const StringMatrix& >::type g(gSEXP);
    Rcpp::traits::input_parameter< const StringMatrix& >::type old_values(old_valuesSEXP);
    Rcpp::traits::input_parameter< const StringVector& >::type new_values(new_valuesSEXP);
    __result = Rcpp::wrap(encode_geno(g, old_values, new_values));
    return __result;
END_RCPP
}
// find_consensus_geno
StringVector find_consensus_geno(StringMatrix g);
RcppExport SEXP qtl2convert_find_consensus_geno(SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< StringMatrix >::type g(gSEXP);
    __result = Rcpp::wrap(find_consensus_geno(g));
    return __result;
END_RCPP
}
// find_unique_geno
StringMatrix find_unique_geno(StringMatrix g);
RcppExport SEXP qtl2convert_find_unique_geno(SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< StringMatrix >::type g(gSEXP);
    __result = Rcpp::wrap(find_unique_geno(g));
    return __result;
END_RCPP
}