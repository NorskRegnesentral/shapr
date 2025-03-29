# Specify the causal ordering and confounding
causal_ordering <- list("trend",
                        c("cosyear", "sinyear"),
                        c("temp", "atemp", "windspeed", "hum"))

confounding <- c(FALSE, TRUE, FALSE)

explanation <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = NULL,
  seed = 1
)
