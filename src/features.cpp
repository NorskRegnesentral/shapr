#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
List sample_features_cpp(int m, IntegerVector n_features) {

    int n = n_features.length();
    List l(n);

    for (int i = 0; i < n; i++) {

        int s = n_features[i];
        IntegerVector k = sample(m, s);
        std::sort(k.begin(), k.end());
        l[i] = k;

    }

    return l;
}
