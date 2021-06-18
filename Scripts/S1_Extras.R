##### Calculate Vincent average rather than average on linear pool ----------------------------------

# Calculate Vincent averages for height distribution
mean.vincent <- function(hBoot){
  
  # Calculate CDF from PDF
  cdf <- function(coldata){
    return(cumsum(coldata/sum(coldata)))}
  cdf_values <- as.data.frame(apply(X = hBoot, MARGIN = 2, FUN = cdf))
  
  # Get distance for each quantile
  quantiles <- function(coldata){
    quantile_data <- data.frame(cbind(seq(0, 8, by = 0.01), coldata))
    return(approx(quantile_data[, 2], quantile_data[, 1], seq(0, 1, by = 0.0001), yleft = 0, yright = 8)$y)}
  
  # Calculate Vincent average for distances across each quantile
  cdf_quantiles <- data.frame(apply(X = cdf_values, MARGIN = 2, FUN = quantiles))
  cdf_quantiles <- apply(X = cdf_quantiles, MARGIN = 1, FUN = mean)
  
  # Return average distribution   
  return(cdf_quantiles)}

# Redefine ds.pdf to put caps on dispersal kernels (only for plotting)
ds.pdf <- function(heights){
  hList <- WALD.h(100, heights)
  hList.dens <- approxfun(density(hList, from = 0, to = 8))
  return(hList.dens(seq(0, 8, by = 0.01)))}

# Plot dispersal kernels, linear pool, and Vincent average
hBoot_1 <- data.frame(replicate(1000, ds.pdf(ht_CN_NW$Height), simplify = "matrix"))
plot(seq(0, 8, by = 0.01), hBoot_1[, 1], type = "l", ylim = c(0, 0.5), xlab = "Distance (m)",
     ylab = "Probability Density", col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
for(i in 2:1000){
  lines(seq(0, 8, by = 0.01), hBoot_1[, i], col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))}
lines(density(mean.vincent(hBoot_1), from = 0.04, to = 8, bw = 0.05), col = "red", lwd = 1.8)
lines(seq(0, 8, by = 0.01), apply(X = hBoot_1, MARGIN = 1, FUN = mean), col = "lightblue", lwd = 1.8)

