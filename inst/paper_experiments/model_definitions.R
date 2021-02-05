# scripts of all models and response functions

# experiment 1
make_response1 = function(X, beta){
  feat_1_ = X[, feat_1_]
  feat_2_ = X[, feat_2_]
  feat_3_ = X[, feat_3_]
  feat_4_ = X[, feat_4_]
  feat_5_ = X[, feat_5_]
  feat_6_ = X[, feat_6_]
  feat_7_ = X[, feat_7_]
  feat_8_ = X[, feat_8_]
  feat_9_ = X[, feat_9_]
  feat_10_ = X[, feat_10_]
  return (
    beta[1] +
      beta[2] * feat_1_ +
      beta[3] * feat_2_ +
      beta[4] * feat_3_ +
      beta[5] * feat_4_ +
      beta[6] * feat_5_ +
      beta[7] * feat_6_ +
      beta[8] * feat_7_ +
      beta[9] * feat_8_ +
      beta[10] * feat_9_ +
      beta[11] * feat_10_)

}

form1 <- response ~
  (feat_1_ + feat_2_) +
  (feat_3_ + feat_4_) +
  (feat_5_ + feat_6_) +
  (feat_7_ + feat_8_) +
  (feat_9_ + feat_10_)

# experiment 2
make_response2 = function(X, beta){
feat_1_ = X[, feat_1_]
feat_2_ = X[, feat_2_]
feat_3_ = X[, feat_3_]
feat_4_ = X[, feat_4_]
feat_5_ = X[, feat_5_]
feat_6_ = X[, feat_6_]
feat_7_ = X[, feat_7_]
feat_8_ = X[, feat_8_]
feat_9_ = X[, feat_9_]
feat_10_ = X[, feat_10_]
return (
  beta[1] +
    beta[2] * feat_1_ +
    beta[3] * feat_2_ +
    beta[4] * feat_3_ +
    beta[5] * feat_4_ +
    beta[6] * feat_5_ +
    beta[7] * feat_6_ +
    beta[8] * feat_7_ +
    beta[9] * feat_8_ +
    beta[10] * feat_9_ +
    beta[11] * feat_10_ +
    beta[12] * (feat_1_ * feat_2_) +
    beta[13] * (feat_3_ * feat_4_) +
    beta[14] * (feat_5_ * feat_6_) +
    beta[15] * (feat_7_ * feat_8_) +
    beta[16] * (feat_9_ * feat_10_))

}

form2 <- response ~
  (feat_1_ * feat_2_) +
  (feat_3_ * feat_4_) +
  (feat_5_ * feat_6_) +
  (feat_7_ * feat_8_) +
  (feat_9_ * feat_10_)

# experiment 3
make_response3 = function(X, beta){

  feat_1_ = X[, feat_1_]
  feat_2_ = X[, feat_2_]
  feat_3_ = X[, feat_3_]
  feat_4_ = X[, feat_4_]
  feat_5_ = X[, feat_5_]
  feat_6_ = X[, feat_6_]
  feat_7_ = X[, feat_7_]
  feat_8_ = X[, feat_8_]
  feat_9_ = X[, feat_9_]
  feat_10_ = X[, feat_10_]
  return (
    beta[1] +
      beta[2] * feat_1_ +
      beta[3] * feat_2_ +
      beta[4] * feat_3_ +
      beta[5] * feat_4_ +
      beta[6] * feat_5_ +
      beta[7] * feat_6_ +
      beta[8] * feat_7_ +
      beta[9] * feat_8_ +
      beta[10] * feat_9_ +
      beta[11] * feat_10_ +
      beta[12] * (feat_1_ * feat_5_) +
      beta[13] * (feat_1_ * feat_7_) +
      beta[14] * (feat_1_ * feat_9_) +
      beta[15] * (feat_3_ * feat_5_) +
      beta[16] * (feat_3_ * feat_7_) +
      beta[17] * (feat_3_ * feat_9_) +
      beta[18] * (feat_5_ * feat_9_)
  )

}

form3 <- response ~
  feat_1_ +
  feat_2_ +
  feat_3_ +
  feat_4_ +
  feat_5_ +
  feat_6_ +
  feat_7_ +
  feat_8_ +
  feat_9_ +
  feat_10_ +
  (feat_1_ * feat_5_) +
  (feat_1_ * feat_7_) +
  (feat_1_ * feat_9_) +
  (feat_3_ * feat_5_) +
  (feat_3_ * feat_7_) +
  (feat_3_ * feat_9_) +
  (feat_5_ * feat_9_)
