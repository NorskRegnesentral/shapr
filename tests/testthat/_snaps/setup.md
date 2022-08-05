# check_data gives correct messages

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

