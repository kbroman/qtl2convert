// find consensus genotype for each row of a genotype matrix

#include "find_consensus_geno.h"
#include <Rcpp.h>
using namespace Rcpp;

// find consensus genotype for each vector of calls
//
// NO missing values; use "NA" instead
//
// if all NA: return NA
// if no genotype most common: return NA
// otherwise return the most common genotype
//
// [[Rcpp::export(".consensus_geno")]]
std::string consensus_geno(const std::vector<std::string>& g)
{
    unsigned int n = g.size();

    // create hash
    std::map<std::string,unsigned int>counts;

    // count unique values that are not "NA"
    unsigned int n_notmissing = 0;
    for(unsigned int i=0; i<n; i++) {
        if(g[i] != "NA") {
            ++counts[g[i]];
            n_notmissing++;
        }
    }
    if(n_notmissing==0) return("NA");

    // find maximum; keep track of whether it's a tie
    unsigned int max_count = 0;
    std::string return_val = "NA";
    bool tie = true;
    for(std::map<std::string, unsigned int>::iterator p=counts.begin(); p != counts.end(); ++p) {
        if(p->second == max_count) {
            tie = true;
        }
        else if(p->second > max_count) {
            tie = false;
            max_count = p->second;
            return_val = p->first;
        }
    }

    // return "NA" if tie; otherwise return the consensus value
    if(tie) return("NA");
    return return_val;
}


// For each row (corresonding to a marker) in a genotype matrix,
//    apply the function above to find the "consensus" genotype
//
// [[Rcpp::export(".find_consensus_geno")]]
StringVector find_consensus_geno(StringMatrix g)
{
    unsigned int n_mar = g.rows();
    unsigned int n_ind = g.cols();
    StringVector result(n_mar);


    // loop over markers and apply consensus_geno (above)
    // oog...rough conversions StringVector <-> std::vector<std::string>
    for(unsigned int mar=0; mar<n_mar; mar++) {
        std::vector<std::string> input(n_ind);
        for(unsigned int ind=0; ind<n_ind; ind++)
            input[ind] = Rcpp::as<std::string>(g(mar,ind));

        result[mar] = consensus_geno(input);
    }

    return result;
}
