// encode genotype matrix using a set of codes
#ifndef ENCODE_GENO_H
#define ENCODE_GENO_H

#include <Rcpp.h>

// encode genotypes matrixusing a set of codes
//
// genotypes is markers x individuals
// old_values is markers x n_new_values
// new_values is vector of new values
Rcpp::StringMatrix encode_geno(const Rcpp::StringMatrix& g,
                               const Rcpp::StringMatrix& old_values,
                               const Rcpp::StringVector& new_values);

#endif // ENCODE_GENO_H
