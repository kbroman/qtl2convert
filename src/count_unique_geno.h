// count unique genotypes for each row of a genotype matrix
#ifndef COUNT_UNIQUE_GENO_H
#define COUNT_UNIQUE_GENO_H

#include <Rcpp.h>

// count unique genotype in a vector of calls
//
// NO missing values; use "NA" instead
//
int count_unique_geno_1mar(const std::vector<std::string>& g);


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to count the unique genotypes
//
Rcpp::IntegerVector count_unique_geno(Rcpp::StringMatrix g);

#endif // COUNT_UNIQUE_GENO_H
