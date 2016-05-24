// find unique genotypes for each row of a genotype matrix
// if >2 or <2 genotypes, return "NA"s

#include "find_unique_geno.h"
#include <Rcpp.h>
using namespace Rcpp;

// find unique genotype in a vector of calls
//
// NO missing values; use "NA" instead
//
// if <2 or >2 unique values, return ("NA",NA")
//
std::vector<std::string> find_unique_geno_1mar(const std::vector<std::string>& g)
{
    unsigned int n = g.size();

    // create hash
    std::map<std::string,unsigned int>unique_g;

    // count unique values that are not "NA"
    for(unsigned int i=0; i<n; i++) {
        if(g[i] != "NA")
            unique_g[g[i]] = 1;
    }

    unsigned int n_unique = unique_g.size();
    std::vector<std::string> result(2);

    if(n_unique < 2 || n_unique > 2) {
        result[0] = result[1] = "NA";
    }
    else {
        unsigned int i=0;
        for(std::map<std::string,unsigned int>::iterator p=unique_g.begin();
            p != unique_g.end(); ++p, i++)
            result[i] = p->first;
    }

    return(result);
}


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to find the unique genotypes
//
// [[Rcpp::export(".find_unique_geno")]]
StringMatrix find_unique_geno(StringMatrix g)
{
    unsigned int n_mar = g.rows();
    unsigned int n_ind = g.cols();
    StringMatrix result(n_mar,2);


    // loop over markers and apply consensus_geno (above)
    // oog...rough conversions StringVector <-> std::vector<std::string>
    for(unsigned int mar=0; mar<n_mar; mar++) {
        std::vector<std::string> input(n_ind);
        for(unsigned int ind=0; ind<n_ind; ind++)
            input[ind] = Rcpp::as<std::string>(g(mar,ind));

        std::vector<std::string> temp_result = find_unique_geno_1mar(input);

        result(mar,0) = temp_result[0];
        result(mar,1) = temp_result[1];
    }

    return result;
}
