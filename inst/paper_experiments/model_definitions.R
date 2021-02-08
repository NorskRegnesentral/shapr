# scripts of all models and response functions

# experiment lm 1
make_response_lm1 = function(X, beta){
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

form_lm1 <- response ~
  (feat_1_ + feat_2_) +
  (feat_3_ + feat_4_) +
  (feat_5_ + feat_6_) +
  (feat_7_ + feat_8_) +
  (feat_9_ + feat_10_)

# experiment lm 2
make_response_lm2 = function(X, beta){
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

form_lm2 <- response ~
  (feat_1_ * feat_2_) +
  (feat_3_ * feat_4_) +
  (feat_5_ * feat_6_) +
  (feat_7_ * feat_8_) +
  (feat_9_ * feat_10_)

# experiment lm 3
make_response_lm3 = function(X, beta){

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

form_lm3 <- response ~
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

# experiment gam 1
library(mgcv)
f1 <- function(a, b) a*b + a*b^2 + b*a^2

make_response_gam1 = function(X, beta){
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
      cos(feat_1_) +
      cos(feat_2_) +
      cos(feat_3_) +
      cos(feat_4_) +
      cos(feat_5_) +
      cos(feat_6_) +
      cos(feat_7_) +
      cos(feat_8_) +
      cos(feat_9_) +
      cos(feat_10_))
}

form_gam1 <- response ~
  s(feat_1_) +
  s(feat_2_) +
  s(feat_3_) +
  s(feat_4_) +
  s(feat_5_) +
  s(feat_6_) +
  s(feat_7_) +
  s(feat_8_) +
  s(feat_9_) +
  s(feat_10_)

# experiment gam 2
make_response_gam2 = function(X, beta){
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
      f1(feat_1_, feat_2_) +
      f1(feat_3_, feat_4_) +
      f1(feat_5_, feat_6_) +
      f1(feat_7_, feat_8_) +
      f1(feat_9_, feat_10_))

}

form_gam2 <- response ~
  s(feat_1_, feat_2_) +
  s(feat_3_, feat_4_) +
  s(feat_5_, feat_6_) +
  s(feat_7_, feat_8_) +
  s(feat_9_, feat_10_)

# experiment gam 3
make_response_gam3 = function(X, beta){

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
      cos(feat_2_) +
      cos(feat_4_) +
      cos(feat_6_) +
      cos(feat_8_) +
      cos(feat_10_) +
      f1(feat_1_, feat_5_) +
      f1(feat_1_, feat_7_) +
      f1(feat_1_, feat_9_) +
      f1(feat_3_, feat_5_) +
      f1(feat_3_, feat_7_) +
      f1(feat_3_, feat_9_) +
      f1(feat_5_, feat_9_)
  )

}

form_gam3 <- response ~
  s(feat_2_) +
  s(feat_4_) +
  s(feat_6_) +
  s(feat_8_) +
  s(feat_10_) +
  s(feat_1_, feat_5_) +
  s(feat_1_, feat_7_) +
  s(feat_1_, feat_9_) +
  s(feat_3_, feat_5_) +
  s(feat_3_, feat_7_) +
  s(feat_3_, feat_9_) +
  s(feat_5_, feat_9_)

