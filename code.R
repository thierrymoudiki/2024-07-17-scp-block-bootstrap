
# 0 - functions -----------------------------------------------------------

mbb2 <- function(r,
                 n,
                 b,
                 seed = 123,
                 return_indices = FALSE)
{
  n_obs <- dim(r)[1]
  n_series <- dim(r)[2]
  b <- floor(min(max(3L, b), n_obs - 1L))
  if(n >= n_obs)
    stop("forecasting horizon must be < number of observations")
  n <- min(n_obs, n)
  
  set.seed(seed) # important for base::sample below
  
  r_bt <- matrix(NA, nrow = n_obs, ncol = dim(r)[2])  # local vector for a bootstrap replication
  
  #cat("n_obs", n_obs, "\n")
  #cat("b", b, "\n")
  for (i in 1:ceiling(n_obs/b)) {
    #cat("i: ", i, "----- \n")
    endpoint <- sample(b:n_obs, size = 1)
    #cat("endpoint", endpoint, "\n")
    try(r_bt[(i - 1)*b + 1:b, ] <- r[endpoint - (b:1) + 1, ],
        silent = TRUE)
  }
  
  tmp <- matrix(r_bt[(1:n), ], nrow = n, ncol = n_series)
  
  if(return_indices == FALSE)
  {
    return(tmp)
  } else {
    return(arrayInd(match(tmp, r), .dim = dim(r))[1:n, 1])
  }
}

# (y <- matrix(rnorm(15), ncol=1))
# B <- 5
# sapply(1:B, function(i) mbb2(y, n=7, b=3, seed=i))

# 0 - Code -----------------------------------------------------------

x <- diff(forecast::BoxCox(AirPassengers, 
                           lambda = "auto"))

#x <- lynx

freq_x <- frequency(x)
  
#
n <- floor(0.9*length(x))
x_train <- ts(x[1:floor(n/2)], 
              start=start(x), 
              frequency = freq_x)
x_calib <- ts(x[(floor(n/2) + 1):n], 
              start=tsp(x_train)[2] + 1 / freq_x, 
              frequency = freq_x)
x_test <- ts(x[(n + 1):length(x)], 
             start=tsp(x_calib)[2] + 1 / freq_x, 
             frequency = freq_x)

# dynrmf
obj <- ahead::dynrmf(x_train, h=length(x_calib)) # train on training set 
calibrated_resids <- obj$mean - x_calib # obtain calibrated residuals
obj_fcast <- ahead::dynrmf(x_calib, 
                           h=length(x_test)) # train on calibration set 
# thetaf 
#obj <- forecast::thetaf(x_train, h=length(x_calib)) # train on training set
#calibrated_resids <- obj$mean - x_calib # obtain calibrated residuals
#obj_fcast <- forecast::thetaf(x_calib, h=length(x_test))
# snaive
#obj <- forecast::snaive(x_train, h=length(x_calib)) # train on training set
#calibrated_resids <- obj$mean - x_calib # obtain calibrated residuals
#obj_fcast <- forecast::snaive(x_calib, h=length(x_test))


scaled_calibrated_resids <- base::scale(calibrated_resids, 
                                       center = TRUE, scale = TRUE)
sd_scaled_calibrated_resids <- attr(scaled_calibrated_resids, "scaled:scale")

# Now bootstrap the residuals and add to the mean
B <- 100 
b <- 15
level <- 95
# Now bootstrap the residuals and add to the mean
sims <- sapply(1:B, function(i) mbb2(matrix(calibrated_resids, 
                                            ncol = 1), 
                                     n=length(x_test), 
                                     b=b, 
                                     seed=i))
sims <- ts(sims, 
           start = start(x_test), 
           frequency = frequency(x_test))
#preds <- obj_fcast$mean + sims * sd_scaled_calibrated_resids
preds <- obj_fcast$mean + sims

obj_fcast2 <- list()
obj_fcast2$level <- level 
obj_fcast2$x <- x_calib
obj_fcast2$mean <- ts(rowMeans(preds), 
  start = start(obj_fcast$mean), 
  frequency = frequency(obj_fcast$mean))
obj_fcast2$upper <- ts(apply(preds, 1, function(x)
  quantile(x, probs = 1 - (1 - level / 100) / 2)), 
  start = start(obj_fcast$mean), 
  frequency = frequency(obj_fcast$mean))
obj_fcast2$lower <- ts(apply(preds, 1, function(x)
    quantile(x, probs = (1 - level / 100) / 2)), 
    start = start(obj_fcast$mean), 
    frequency = frequency(obj_fcast$mean))
class(obj_fcast2) <- "forecast"


par(mfrow=c(2, 2))

plot(x, main="input data = black + green + blue \n calibration set = green  \n forecast on calibration set = red", 
     lwd=2)
lines(x_calib, col="green", lwd=2)
lines(obj$mean, col="red", lwd=2)
lines(x_test, col="blue", lwd=2)

plot(calibrated_resids, main="calibrated residuals \n = green - red = orange",
     col="orange", lwd=2)
     #ylim = c(min(x), max(x)))
abline(h = 0, lty=2)

matplot(as.numeric(time(sims)), 
        sims, type='l', xlab = "Time", 
        main = "calibrated residuals simulation \n (block bootstrap)")

# Not the correct graph 
plot(obj_fcast2, xlab = "Time", 
     main = "prediction intervals around the simulations")
lines(x_test, col='magenta', lty=2)
lines(x_calib, col="green", lwd=2)
# Now bootstrap the residuals and add to the mean

