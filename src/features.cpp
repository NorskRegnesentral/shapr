#include <Rcpp.h>
using namespace Rcpp;

//' Sampling of coaltions
//'
//' @keywords internal
// [[Rcpp::export]]
List sample_coalitions_cpp(int m, IntegerVector n_coalitions) {

    int n = n_coalitions.length();
    List l(n);

    for (int i = 0; i < n; i++) {

        int s = n_coalitions[i];
        IntegerVector k = sample(m, s);
        std::sort(k.begin(), k.end());
        l[i] = k;

    }

    return l;
}
