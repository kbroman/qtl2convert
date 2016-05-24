// encode genotype matrix using a set of codes

#include "encode_geno.h"
#include <Rcpp.h>
using namespace Rcpp;

// encode genotypes matrixusing a set of codes
//
// genotypes is markers x individuals
// old_values is markers x n_new_values
// new_values is vector of new values
//
// [[Rcpp::export(".encode_geno")]]
StringMatrix encode_geno(const StringMatrix& g,
                         const StringMatrix& old_values,
                         const StringVector& new_values)
{
    int n_mar = g.rows();
    int n_ind = g.cols();
    int n_code = old_values.cols();
    StringMatrix result(n_mar,n_ind);

    if(old_values.rows() != n_mar)
        throw std::invalid_argument("nrow(g) != nrow(old_values)");
    if(new_values.size() != n_code)
        throw std::invalid_argument("ncol(old_values) != length(new_values)");

    for(int ind=0; ind<n_ind; ind++) {
        for(int mar=0; mar<n_mar; mar++) {
            if(StringVector::is_na(g(mar,ind)))
                result(mar,ind)=NA_STRING;
            else {
                bool unassigned=true;
                for(int codei=0; codei<n_code; codei++) {
                    if(g(mar,ind)==old_values(mar,codei)) {
                        unassigned=false;
                        result(mar,ind) = new_values[codei];
                    }
                }
                if(unassigned) result(mar,ind) = NA_STRING;
            }
        }
    }

    return result;
}
