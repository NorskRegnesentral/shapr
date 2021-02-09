# scripts of all models and response functions

# experiment lm 1
form_lm1 <- response ~
  feat_1_ +
  feat_2_ +
  feat_3_ +
  feat_4_ +
  feat_5_ +
  feat_6_ +
  feat_7_ +
  feat_8_ +
  feat_9_ +
  feat_10_
  
# experiment lm 2
form_lm2 <- response ~
  (feat_1_ * feat_2_) +
  (feat_3_ * feat_4_) +
  (feat_5_ * feat_6_) +
  (feat_7_ * feat_8_) +
  (feat_9_ * feat_10_)

# experiment lm 3
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
