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

//' We here return a list of strings, where each string is a comma-separated list of integers, but the structure
//' could also have been a CharacterVector. The reason for using a list here is that sample_features_cpp returns a list.
//' @keywords internal
// [[Rcpp::export]]
CharacterVector sample_features_cpp_str_paired(int m, IntegerVector n_features, bool paired_shap_sampling = true) {

  int n = n_features.size();
  CharacterVector result(paired_shap_sampling ? 2 * n : n);

  for (int i = 0; i < n; i++) {

    int s = n_features[i];
    IntegerVector k = sample(m, s);
    std::sort(k.begin(), k.end());

    // Boolean vector to mark presence of elements in k
    std::vector<bool> present(m + 1, false);
    for (int idx = 0; idx < s; idx++) {
      present[k[idx]] = true;
    }

    // Generate both the ss and paired_ss strings in a single pass
    std::stringstream ss;
    std::stringstream paired_ss;
    bool first_ss = true;
    bool first_paired_ss = true;

    for (int j = 1; j <= m; j++) {
      if (present[j]) {
        if (!first_ss) {
          ss << ",";
        } else {
          first_ss = false;
        }
        ss << j;
      } else if (paired_shap_sampling) {
        if (!first_paired_ss) {
          paired_ss << ",";
        } else {
          first_paired_ss = false;
        }
        paired_ss << j;
      }
    }

    result[i * (paired_shap_sampling ? 2 : 1)] = ss.str();

    if (paired_shap_sampling) {
      result[i * 2 + 1] = paired_ss.str();
    }
  }

  return result;
}
