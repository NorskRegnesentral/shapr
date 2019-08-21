#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//'
//' @export
//'
// [[Rcpp::export]]
List sample_features_cpp(int m, IntegerVector nfeatures) {

    int n = nfeatures.length();
    List l(n);

    for (int i = 0; i < n; i++) {

        int s = nfeatures[i];
        IntegerVector k = sample(m, s);
        std::sort(k.begin(), k.end());
        l[i] = k;

    }

    return l;
}
