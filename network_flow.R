generate_network_flow <- function(pre_test_probability, sensitivity, specificity, n = 1000) {
  
  true_positives <- n * pre_test_probability * sensitivity
  true_negatives <- n * (1-pre_test_probability) * specificity
  false_positives <- n * (1-pre_test_probability) * (1-specificity)
  false_negatives <- n * pre_test_probability * (1-sensitivity)
  
  
  return(list(
    tp = true_positives,
    tn = true_negatives,
    fp = false_positives,
    fn = false_negatives,
    n = n
  ))
}

