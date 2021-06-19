##### Plot distribution of flower heights, including Vincent average  -------------------------------------

# Calculate Vincent averages for height distribution
mean.vincent <- function(hBoot){
  cdf <- function(coldata){
    return(cumsum(coldata/sum(coldata)))}
  cdf_values <- as.data.frame(apply(X = hBoot, MARGIN = 2, FUN = cdf))
  quantiles <- function(coldata){
    quantile_data <- data.frame(cbind(seq(0, 200, by = 0.1), coldata))
    return(approx(quantile_data[, 2], quantile_data[, 1], seq(0, 1, by = 0.0001), yleft = 0, yright = 200)$y)}
  cdf_quantiles <- data.frame(apply(X = cdf_values, MARGIN = 2, FUN = quantiles))
  cdf_quantiles <- apply(X = cdf_quantiles, MARGIN = 1, FUN = mean) 
  return(cdf_quantiles)}
ht.pdfBoot2 <- function(h1, h2){
  hBoot_1 <- data.frame(replicate(1000, ht.pdf(sample(h1, size = 500, replace = TRUE))))
  hBoot_2 <- data.frame(replicate(1000, ht.pdf(sample(h2, size = 500, replace = TRUE))))
  return(list(data.frame(seq(0, 200, by = 0.1),
                         apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                         apply(X = hBoot_2, MARGIN = 1, FUN = mean)),
              hBoot_1, hBoot_2,
              mean.vincent(hBoot_1),
              mean.vincent(hBoot_2)))}

# Function to plot bootstrapped height distributions
# Includes linear pool (blue) and Vincent average (yellow)
ht.pdfBoot.plot2 <- function(bootData, bottom){
  if(bottom == FALSE){
    plot(x = bootData[[1]][, 1], y = bootData[[2]][, 1], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xaxt = "n", yaxt = "n", ylab = "Probability Density",
         col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
    lines(bootData[[1]][, 1], bootData[[3]][, 1], col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))
    for(i in 2:1000){
      lines(bootData[[1]][, 1], bootData[[2]][, i], col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
      lines(bootData[[1]][, 1], bootData[[3]][, i], col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))}
    lines(density(bootData[[4]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(density(bootData[[5]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 2], col = "deepskyblue", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 3], col = "deepskyblue", lwd = 1.8)
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  if(bottom == TRUE){
    plot(x = bootData[[1]][, 1], y = bootData[[2]][, 1], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xlab = "Flower Height (cm)", yaxt = "n", ylab = "Probability Density",
         col = rgb(red = 0.4, green = 0, blue = 0, alpha = 0.03))
    lines(bootData[[1]][, 1], bootData[[3]][, 1], col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))
    for(i in 2:1000){
      lines(bootData[[1]][, 1], bootData[[2]][, i], col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
      lines(bootData[[1]][, 1], bootData[[3]][, i], col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))}
    lines(density(bootData[[4]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(density(bootData[[5]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 2], col = "deepskyblue", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 3], col = "deepskyblue", lwd = 1.8)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}}

# Bootstrap PDFs for NW/W CN and NW/W CA heights
hBoot_HD1_2 <- ht.pdfBoot2(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_HD2_2 <- ht.pdfBoot2(ht_CA_NW$Height, ht_CA_W$Height)

# Prepare graphics device
jpeg(filename = "Figure 2.2.jpeg", width = 826, height = 568, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1200, 800)
pushViewport(viewport(layout = gly))

# CN non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 1:600, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
ht.pdfBoot.plot2(hBoot_HD1_2, bottom = FALSE)
#abline(v = c(subset(ht_stats, Species == "CN" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CN" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ht.pdfBoot.plot2(hBoot_HD2_2, bottom = TRUE)
#abline(v = c(subset(ht_stats, Species == "CA" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CA" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# Note: commented out code above adds a vertical line at the mean

# Create legend
grid.text(label = c("Vincent Average", "Linear Average", "Warmed", "Not Warmed"), x = rep(0.84, 4),
          y = c(0.81, 0.84, 0.87, 0.90), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.808, 0.838, 0.868, 0.898), x1 = rep(0.833, 4),
              y1 = c(0.808, 0.838, 0.868, 0.898), gp = gpar(col = c("gold", "deepskyblue", "red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [WIP] Vincent average on dispersal kernels ----------------------------------------------------------

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

