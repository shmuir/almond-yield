
compute_NPV = function(value, time, discount=0.12) {
  result = value / (1 + discount)**time
  return(result)
}
