# scripts of all models and response functions

# experiment lm 1
make_response_onehot_lm1 = function(X, beta){
  feat_1_2 = X[, feat_1_2]
  feat_1_3 = X[, feat_1_3]
  feat_2_2 = X[, feat_2_2]
  feat_2_3 = X[, feat_2_3]
  feat_3_2 = X[, feat_3_2]
  feat_3_3 = X[, feat_3_3]
  feat_4_2 = X[, feat_4_2]
  feat_4_3 = X[, feat_4_3]
  feat_5_2 = X[, feat_5_2]
  feat_5_3 = X[, feat_5_3]
  feat_6_2 = X[, feat_6_2]
  feat_6_3 = X[, feat_6_3]
  return (
    beta[1] +
      beta[2] * feat_1_2 +
      beta[3] * feat_1_3 +
      beta[4] * feat_2_2 +
      beta[5] * feat_2_3 +
      beta[6] * feat_3_2 +
      beta[7] * feat_3_3 +
      beta[8] * feat_4_2 +
      beta[9] * feat_4_3+
      beta[10] * feat_5_2 +
      beta[11] * feat_5_3 +
      beta[12] * feat_6_2 +
      beta[13] * feat_6_3)

}

form_onehot_lm1 <- response ~
  feat_1_2 +
  feat_1_3 +
  feat_2_2 +
  feat_2_3 +
  feat_3_2 +
  feat_3_3 +
  feat_4_2 +
  feat_4_3+
  feat_5_2 +
  feat_5_3 +
  feat_6_2 +
  feat_6_3

make_response_lm1 = function(X, beta){
  feat_1_ = X[, feat_1_]
  feat_2_ = X[, feat_2_]
  feat_3_ = X[, feat_3_]
  feat_4_ = X[, feat_4_]
  feat_5_ = X[, feat_5_]
  feat_6_ = X[, feat_6_]
  return (
    beta[1] +
      beta[2] * feat_1_ +
      beta[3] * feat_2_ +
      beta[4] * feat_3_ +
      beta[5] * feat_4_ +
      beta[6] * feat_5_ +
      beta[7] * feat_6_)
}

form_lm1 <- response ~
  (feat_1_ + feat_2_) +
  (feat_3_ + feat_4_) +
  (feat_5_ + feat_6_)
