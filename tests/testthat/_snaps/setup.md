# error with custom model without providing predict_model

    Code
      explain(x_train_mixed, x_test_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0)
    Error <simpleError>
      You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# messages with missing detail in get_model_specs

    Code
      explain(x_train_mixed, x_test_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = NA)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      explain(x_train_mixed, x_test_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_labels)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      explain(x_train_mixed, x_test_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_classes)
    Message <simpleMessage>
      Note: Feature classes extracted from the model contains NA.
      Assuming feature classes from the data are correct.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      explain(x_train_mixed, x_test_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_factor_levels)
    Message <simpleMessage>
      Note: Feature factor levels extracted from the model contains NA.
      Assuming feature factor levels from the data are correct.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

# incorrect input: `prediction_zero` gives the correct error

    Code
      explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_non_numeric_1)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_non_numeric_2)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_too_long)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      explain(x_train_numeric, x_test_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_is_NA)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

