library(plumber)
p = plumb("plumber.R")
p$run(host = '0.0.0.0', port = 8000)
