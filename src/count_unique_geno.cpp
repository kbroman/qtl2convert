// count unique genotypes for each row of a genotype matrix

#include "count_unique_geno.h"
#include <Rcpp.h>
using namespace Rcpp;

// count unique genotype in a vector of calls
//
// NO missing values; use "NA" instead
//
int count_unique_geno_1mar(const std::vector<std::string>& g)
{
    int n = g.size();

    // create hash
    std::map<std::string,int>unique_g;

    // count unique values that are not "NA"
    for(int i=0; i<n; i++) {
        if(g[i] != "NA")
            unique_g[g[i]] = 1;
    }

    return(unique_g.size());
}


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to count the unique genotypes
//
// [[Rcpp::export(".count_unique_geno")]]
IntegerVector count_unique_geno(StringMatrix g)
{
    int n_mar = g.rows();
    int n_ind = g.cols();
    IntegerVector result(n_mar);


    // loop over markers and apply function above
    // oog...rough conversions StringVector <-> std::vector<std::string>
    for(int mar=0; mar<n_mar; mar++) {
        std::vector<std::string> input(n_ind);
        for(int ind=0; ind<n_ind; ind++)
            input[ind] = Rcpp::as<std::string>(g(mar,ind));

        result[mar] = count_unique_geno_1mar(input);
    }

    return result;
}
