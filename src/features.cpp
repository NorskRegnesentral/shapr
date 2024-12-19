#include <Rcpp.h>
using namespace Rcpp;

//' We here return a vector of strings/characters, i.e., a CharacterVector,
//' where each string is a space-separated list of integers.
//'
//' @param n_coalitions IntegerVector.
//' The number of features to sample for each feature combination.
//' @inheritParams create_coalition_table
//' @keywords internal
// [[Rcpp::export]]
CharacterVector sample_coalitions_cpp_str_paired(int m, IntegerVector n_coalitions, bool paired_shap_sampling = true) {

  int n = n_coalitions.size();
  CharacterVector result(paired_shap_sampling ? 2 * n : n);

  for (int i = 0; i < n; i++) {

    int s = n_coalitions[i];
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
          ss << " ";
        } else {
          first_ss = false;
        }
        ss << j;
      } else if (paired_shap_sampling) {
        if (!first_paired_ss) {
          paired_ss << " ";
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
