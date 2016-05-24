// find consensus genotype for each row of a genotype matrix
#ifndef FIND_CONSENSUS_GENO_H
#define FIND_CONSENSUS_GENO_H

#include <Rcpp.h>

// find consensus genotype for each vector of calls
//
// NO missing values; use "NA" instead
//
// if all NA: return NA
// if no genotype most common: return NA
// otherwise return the most common genotype
//
std::string consensus_geno(const std::vector<std::string>& g);


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to find the "consensus" genotype
//
Rcpp::StringVector find_consensus_geno(Rcpp::StringMatrix g);

#endif // FIND_CONSENSUS_GENO_H
