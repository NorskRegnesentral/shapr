# Below we include other versions of the C++ function that we have tried.
# However, they were slower than the new version.


# #include <Rcpp.h>
# #include <sstream>
# #include <algorithm>
# #include <vector>
# using namespace Rcpp;
# // [[Rcpp::export]]
# IntegerVector get_complement(int m, IntegerVector k) {
#   // Create a set with all integers from 1 to m
#   std::set<int> all_numbers;
#   for (int i = 1; i <= m; ++i) {
#     all_numbers.insert(i);
#   }
#
#   // Erase elements that are present in k
#   for (int i = 0; i < k.size(); ++i) {
#     all_numbers.erase(k[i]);
#   }
#
#   // Convert the set to an IntegerVector
#   IntegerVector complement(all_numbers.begin(), all_numbers.end());
#
#   return complement;
# }
#
# //' @keywords internal
# // [[Rcpp::export]]
# CharacterVector sample_coalitions_cpp_str_paired1(int m, IntegerVector n_features, bool paired_shap_sampling = true) {
#
#   int n = n_features.length();
#   int result_size = paired_shap_sampling ? 2 * n : n;
#   CharacterVector result(result_size);
#
#   for (int i = 0; i < n; i++) {
#
#     int s = n_features[i];
#     IntegerVector k = sample(m, s);
#     std::sort(k.begin(), k.end());
#
#     // Convert sampled features to a comma-separated string
#     std::stringstream ss;
#     for (int j = 0; j < s; j++) {
#       if (j != 0) {
#         ss << ",";
#       }
#       ss << k[j];
#     }
#     result[i * (paired_shap_sampling ? 2 : 1)] = ss.str();
#
#     if (paired_shap_sampling) {
#       // Get complement and convert to string
#       IntegerVector complement = get_complement(m, k);
#       std::stringstream paired_ss;
#       for (int j = 0; j < complement.size(); j++) {
#         if (j != 0) {
#           paired_ss << ",";
#         }
#         paired_ss << complement[j];
#       }
#       result[i * 2 + 1] = paired_ss.str();
#     }
#   }
#
#   return result;
# }
#
#
# //' @keywords internal
# // [[Rcpp::export]]
# CharacterVector sample_coalitions_cpp_str_paired2(int m, IntegerVector n_features, bool paired_shap_sampling = true) {
#
#   int n = n_features.length();
#   int result_size = paired_shap_sampling ? 2 * n : n;
#   CharacterVector result(result_size);
#
#   for (int i = 0; i < n; i++) {
#
#     int s = n_features[i];
#     IntegerVector k = sample(m, s);
#     std::sort(k.begin(), k.end());
#
#     // Convert sampled features to a comma-separated string
#     std::stringstream ss;
#     for (int j = 0; j < s; j++) {
#       if (j != 0) {
#         ss << ",";
#       }
#       ss << k[j];
#     }
#     result[i * (paired_shap_sampling ? 2 : 1)] = ss.str();
#
#     if (paired_shap_sampling) {
#       // Collect integers from 1 to m not in k
#       std::stringstream paired_ss;
#       for (int j = 1; j <= m; j++) {
#         if (std::find(k.begin(), k.end(), j) == k.end()) {
#           if (paired_ss.tellp() > 0) {
#             paired_ss << ",";
#           }
#           paired_ss << j;
#         }
#       }
#       result[i * 2 + 1] = paired_ss.str();
#     }
#   }
#
#   return result;
# }
#
#
# //' @keywords internal
# // [[Rcpp::export]]
# CharacterVector sample_coalitions_cpp_str_paired3(int m, IntegerVector n_features, bool paired_shap_sampling = true) {
#
#   int n = n_features.size();
#   int result_size = paired_shap_sampling ? 2 * n : n;
#   CharacterVector result(result_size);
#
#   for (int i = 0; i < n; i++) {
#
#     int s = n_features[i];
#     IntegerVector k = sample(m, s);
#     std::sort(k.begin(), k.end());
#
#     // Convert sampled features to a comma-separated string
#     std::stringstream ss;
#     for (int j = 0; j < s; j++) {
#       if (j != 0) {
#         ss << ",";
#       }
#       ss << k[j];
#     }
#     result[i * (paired_shap_sampling ? 2 : 1)] = ss.str();
#
#     if (paired_shap_sampling) {
#       // Use a boolean vector to mark presence of elements in k
#       std::vector<bool> present(m + 1, false);
#       for (int idx = 0; idx < s; idx++) {
#         present[k[idx]] = true;
#       }
#
#       // Build the complement string
#       std::stringstream paired_ss;
#       for (int j = 1; j <= m; j++) {
#         if (!present[j]) {
#           if (paired_ss.tellp() > 0) {
#             paired_ss << ",";
#           }
#           paired_ss << j;
#         }
#       }
#       result[i * 2 + 1] = paired_ss.str();
#     }
#   }
#
#   return result;
# }
#
#
#
#
# //' @keywords internal
# // [[Rcpp::export]]
# CharacterVector sample_coalitions_cpp_str_paired4(int m, IntegerVector n_features, bool paired_shap_sampling = true) {
#
#   int n = n_features.size();
#   int result_size = paired_shap_sampling ? 2 * n : n;
#   CharacterVector result(result_size);
#
#   for (int i = 0; i < n; i++) {
#
#     int s = n_features[i];
#     IntegerVector k = sample(m, s);
#     std::sort(k.begin(), k.end());
#
#     // Use a boolean vector to mark presence of elements in k
#     std::vector<bool> present(m + 1, false);
#     for (int idx = 0; idx < s; idx++) {
#       present[k[idx]] = true;
#     }
#
#     // Generate both the ss and paired_ss strings in a single pass
#     std::stringstream ss;
#     std::stringstream paired_ss;
#     bool first_ss = true;
#     bool first_paired_ss = true;
#
#     for (int j = 1; j <= m; j++) {
#       if (present[j]) {
#         if (!first_ss) {
#           ss << ",";
#         } else {
#           first_ss = false;
#         }
#         ss << j;
#       } else if (paired_shap_sampling) {
#         if (!first_paired_ss) {
#           paired_ss << ",";
#         } else {
#           first_paired_ss = false;
#         }
#         paired_ss << j;
#       }
#     }
#
#     result[i * (paired_shap_sampling ? 2 : 1)] = ss.str();
#
#     if (paired_shap_sampling) {
#       result[i * 2 + 1] = paired_ss.str();
#     }
#   }
#
#   return result;
# }
