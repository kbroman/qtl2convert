// find unique genotypes for each row of a genotype matrix
// if >2 or <2 genotypes, return "NA"s
#ifndef FIND_UNIQUE_GENO_H
#define FIND_UNIQUE_GENO_H

#include <Rcpp.h>

// find unique genotype in a vector of calls
//
// NO missing values; use "NA" instead
//
// if <2 or >2 unique values, return ("NA",NA")
//
std::vector<std::string> find_unique_geno_1mar(const std::vector<std::string>& g);


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to find the unique genotypes
//
Rcpp::StringMatrix find_unique_geno(Rcpp::StringMatrix g);

#endif // FIND_UNIQUE_GENO_H
