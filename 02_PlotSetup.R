##### [F1] Calculate mean heights for each treatment/species combination ----------------------------------

# Put data into list to easily use vectorised functions
HeightList <- list(ht_CN_NW$Height, ht_CN_W$Height, ht_CA_NW$Height, ht_CA_W$Height)

# Create data frame of height statistics and populate it
ht_stats <- data.frame(matrix(rep(NA, 28), nrow = 4))
ht_stats[, 1] <- c("CN-NW", "CN-W", "CA-NW", "CA-W")
ht_stats[, 2] <- c(rep("CN", 2), rep("CA", 2))
ht_stats[, 3] <- rep(c("Not Warmed", "Warmed"), 2)
ht_stats[, 4] <- sapply(HeightList, length)
ht_stats[, 5] <- sapply(HeightList, mean)
ht_stats[, 6] <- sapply(HeightList, sd)
ht_stats[, 7] <- sapply(HeightList, function(x) {sd(x)/sqrt(length(x))})
names(ht_stats) <- c("ID", "Species", "Treatment", "n", "MeanHeight", "SDHeight", "SEHeight")
ht_stats$Species <- factor(ht_stats$Species, levels = c("CN", "CA"))

# Remove HeightList since it has served its purpose
remove(HeightList)





##### [F2] Estimate height distribution PDF with 95% bootstrap interval -----------------------------------

# Set up function to estimate PDF for a bootstrapped sample
ht.pdf <- function(heights){
  hList.dens <- approxfun(density(heights, from = 0, to = 200))
  return(hList.dens(seq(0, 200, by = 0.1)))}

# Function to bootstrap height distributions and estimate 95% bootstrap interval
# For each bootstrap sample 500 release heights; bootstrap 1000 times
ht.pdfBoot <- function(h1, h2){
  hBoot_1 <- data.frame(replicate(1000, ht.pdf(sample(h1, size = 500, replace = TRUE))))
  hBoot_2 <- data.frame(replicate(1000, ht.pdf(sample(h2, size = 500, replace = TRUE))))
  return(data.frame(seq(0, 200, by = 0.1),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped height distributions, with mean line and 95% band
ht.pdfBoot.plot <- function(bootData, bottom){
  if(bottom == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xaxt = "n", yaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  if(bottom == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xlab = "Flower Height (cm)", yaxt = "n", ylab = "Probability Density")
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Bootstrap PDFs for NW/W CN and NW/W CA heights
hBoot_HD1 <- ht.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_HD2 <- ht.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height)

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W height distributions display significant difference for CN and CA
ks.test(hBoot_HD1[, 2], hBoot_HD1[, 5], alt = "two.sided")
ks.test(hBoot_HD2[, 2], hBoot_HD2[, 5], alt = "two.sided")





##### [F3.1] Estimate warmed vs not warmed PDF (dispersal kernels) ----------------------------------------

# Set up function to create a single sample and estimate PDF
ds.pdf <- function(heights){
  hList <- WALD.h(100, heights)
  hList.dens <- approxfun(density(hList))
  return(hList.dens(seq(0, 8, by = 0.01)))}

# Function to bootstrap dispersal kernels and estimate 95% bootstrap interval
# For each kernel sample 100 release heights with 100 seed releases each, and repeat 1000 times
ds.pdfBoot <- function(h1, h2){
  hBoot_1 <- data.frame(replicate(1000, ds.pdf(h1), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.pdf(h2), simplify = "matrix"))
  return(data.frame(seq(0, 8, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped dispersal kernels, with mean line and 95% band
ds.pdfBoot.plot <- function(bootData, bottom){
  if(bottom == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(bottom == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xlab = "Dispersal Distance (m)", ylab = "Probability Density")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Bootstrap PDFs for NW/W CN and NW/W CA dispersal kernels
hBoot_WNW1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_WNW2_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height)

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_WNW1_pdf[, 2], hBoot_WNW1_pdf[, 5], alt = "two.sided")
ks.test(hBoot_WNW2_pdf[, 2], hBoot_WNW2_pdf[, 5], alt = "two.sided")





##### [F3.2] Estimate warmed vs not warmed CCDF -----------------------------------------------------------

# Set up function to create a single sample and estimate CCDF
ds.ccdf <- function(heights){
  hList <- sort(WALD.h(100, heights))
  hList.exceed <- function(cutoff){
    return(length(hList[hList >= cutoff])/length(hList))}
  hList.ccdf <- approxfun(hList, sapply(hList, hList.exceed), yleft = 1, yright = 0)
  return(hList.ccdf(seq(0, 60, by = 0.01)))}

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
ds.ccdfBoot <- function(h1, h2){
  hBoot_1 <- data.frame(replicate(1000, ds.ccdf(h1), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.ccdf(h2), simplify = "matrix"))
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped CCDF, with mean line and 95% band
ds.ccdfBoot.plot <- function(bootData, bottom){
  if(bottom == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 1),
         xaxt = "n", ylab = "Complement of Cumulative Probability")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bottom == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 1),
         xlab = "Dispersal Distance (m)", ylab = "Complement of Cumulative Probability")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Bootstrap CCDFs for NW/W CN and NW/W CA heights
hBoot_WNW1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_WNW2_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_W$Height)





##### [F3.3] Estimate warmed vs not warmed CCDF ratios ----------------------------------------------------

# Function to bootstrap CCDF ratio and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
ds.ccdfRatioBoot <- function(h1, h2){
  hBoot_1 <- data.frame(replicate(1000, ds.ccdf(h1), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.ccdf(h2), simplify = "matrix"))
  hBoot_a <- hBoot_2/hBoot_1
  mean.exinf <- function(x){
    return(mean(x[!is.infinite(x)]))}
  quantile.exinf <- function(x, probs){
    return(quantile(x[!is.infinite(x)], probs = probs, na.rm = TRUE))}
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_a, MARGIN = 1, FUN = mean.exinf),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.025),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.975)))}

# Function to plot bootstrapped CCDF ratio, with mean line and 95% band
ds.ccdfRatioBoot.plot <- function(bootData, bottom){
  if(bottom == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", ylab = "W/NW CCDF Ratio")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bottom == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xlab = "Dispersal Distance (m)", ylab = "W/NW CCDF Ratio")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)}

# Bootstrap CCDF ratios for NW/W CN and NW/W CA heights
hBoot_WNW1_CCDFRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_WNW2_CCDFRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_W$Height)





##### [F3.4] Estimate warmed vs not right tail dispersal percentile distances -----------------------------

# Set up function to create bootstrapped sample and canculate nth percentiles
ds.rtail <- function(heights){
  hList <- WALD.h(100, heights)
  return(quantile(hList, probs = seq(0.9500, 0.9999, by = 0.0001)))}

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
ds.rtailBootWNW <- function(h1, h2, Species){
  hBoot_1 <- data.frame(replicate(1000, ds.rtail(h1), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.rtail(h2), simplify = "matrix"))
  hBoot_dat <- data.frame(cbind(rep(seq(0.9500, 0.9999, by = 0.0001), 2), c(rep("Not Warmed", 500), rep("Warmed", 500))), 
                          rep(Species, 1000),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                            apply(X = hBoot_2, MARGIN = 1, FUN = mean)),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025)),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)),
                          row.names = NULL)
  names(hBoot_dat) <- c("DistancePercentile", "Treatment", "Species", "Mean", "Lower", "Upper")
  return(hBoot_dat)}

# Plot the dispersal percentiles and the corresponding distances
# Error bars indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.rtailBootWNW.plot <- function(bootData, bottom){
  bootData[, 1] <- as.numeric(as.character(bootData[, 1]))
  rtNW <- subset(bootData, Treatment == "Not Warmed")
  rtW <- subset(bootData, Treatment == "Warmed")
  if(bottom == FALSE){
    plot(x = rtNW[, 1], y = rtNW[, 4], type = "l", xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", ylab = "Dispersal Distance (m)")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = FALSE)}
  if(bottom == TRUE){
    plot(x = rtNW[, 1], y = rtNW[, 4], type = "l", xlim = c(0.95, 1), ylim = c(0, 80),
         xlab = "Dispersal Quantile", ylab = "Dispersal Distance (m)")}
  polygon(x = c(rtNW[, 1], rev(rtNW[, 1])), 
          y = c(rtNW[, 5], rev(rtNW[, 6])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = rtW[, 1], y = rtW[, 4], type = "l", col = "red")
  polygon(x = c(rtW[, 1], rev(rtW[, 1])), 
          y = c(rtW[, 5], rev(rtW[, 6])), col = alpha("red", alpha = 0.2), border = NA)}

# Bootstrap right tail dispersal percentile distances for NW/W CN and NW/W CA heights
hboot_WNW1_rtail <- ds.rtailBootWNW(ht_CN_NW$Height, ht_CN_W$Height, "CN")
hboot_WNW2_rtail <- ds.rtailBootWNW(ht_CA_NW$Height, ht_CA_W$Height, "CA")





##### [F4.1] Estimate max height vs height distribution PDF (dispersal kernels) ---------------------------

# Function to calculate dispersal kernels with mean height or distribution of heights
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 50000 seed release events and is repeated 1000 times

# Function to plot the dispersal kernels
# Bands indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.pdfBoot.plot2 <- function(bootData, LColour, PColour, bNum, bLab){
  if(bNum == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(bNum == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xlab = ifelse(bLab == TRUE, "Dispersal Distance (m)", NA), ylab = "Probability Density")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", lty = 3, col = PColour)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha(PColour, alpha = 0.2), border = NA)}

# Bootstrap PDFs for M/HD CN and M/HD CA dispersal kernels
hBoot_MHD1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_NW_max$max)
hBoot_MHD2_pdf <- ds.pdfBoot(ht_CN_W$Height, ht_CN_W_max$max)
hBoot_MHD3_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_NW_max$max)
hBoot_MHD4_pdf <- ds.pdfBoot(ht_CA_W$Height, ht_CA_W_max$max)

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_MHD1_pdf[, 2], hBoot_MHD1_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD2_pdf[, 2], hBoot_MHD2_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD3_pdf[, 2], hBoot_MHD3_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD4_pdf[, 2], hBoot_MHD4_pdf[, 5], alt = "two.sided")





##### [F4.2] Estimate max height vs height distribution CCDF ----------------------------------------------

# Plot the dispersal percentiles and the corresponding distances
# Error bars indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.ccdfBoot.plot2 <- function(bootData, LColour, PColour, bNum, bLab){
  if(bNum == FALSE){
    plot(x = bootData[, 1], y = bootData[, 4], type = "l", col = LColour, xlim = c(0, 60), ylim = c(0, 1),
         xaxt = "n", ylab = "Dispersal Distance (m)")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bNum == TRUE){
    plot(x = bootData[, 1], y = bootData[, 4], type = "l", col = LColour, xlim = c(0, 60), ylim = c(0, 1),
         xlab = ifelse(bLab == TRUE, "Dispersal Quantile", NA), ylab = "Dispersal Distance (m)")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 5], rev(bootData[, 6])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 4], type = "l", lty = 3, col = PColour)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 5], rev(bootData[, 6])), col = alpha(PColour, alpha = 0.2), border = NA)}

# Bootstrap CCDFs for M/HD CN and M/HD CA
hBoot_MHD1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_NW_max$max)
hBoot_MHD2_ccdf <- ds.ccdfBoot(ht_CN_W$Height, ht_CN_W_max$max)
hBoot_MHD3_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_NW_max$max)
hBoot_MHD4_ccdf <- ds.ccdfBoot(ht_CA_W$Height, ht_CA_W_max$max)





##### [F4.3] Plot max height vs height distribution CCDF ratio --------------------------------------------

# Function to plot bootstrapped CCDF, with mean line and 95% band
ds.ccdfRatioBoot.plot2 <- function(bootData, bNum, bLab){
  if(bNum == FALSE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", ylab = "M/HD CCDF Ratio")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bNum == TRUE){
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xlab = ifelse(bLab == TRUE, "Dispersal Distance (m)", NA), ylab = "M/HD CCDF Ratio")}
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)}

# Bootstrap CCDF ratios for M/HD CN and M/HD CA dispersal kernels
hBoot_MHD1_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_W$Height, ht_CN_W_max$max)
hBoot_MHD2_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_NW_max$max)
hBoot_MHD3_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_W$Height, ht_CA_W_max$max)
hBoot_MHD4_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_NW_max$max)





##### [F4.4] Estimate max height vs height distribution right tail dispersal percentile distances ---------

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
ds.rtailBootMHD <- function(distHeight, maxHeight, Species){
  hBoot_1 <- data.frame(replicate(1000, ds.rtail(distHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.rtail(maxHeight), simplify = "matrix"))
  hBoot_dat <- data.frame(cbind(rep(seq(0.9500, 0.9999, by = 0.0001), 2), c(rep("Dist. Height", 500), rep("Max. Height", 500))), 
                          rep(Species, 1000),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                            apply(X = hBoot_2, MARGIN = 1, FUN = mean)),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025)),
                          c(apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)),
                          row.names = NULL)
  names(hBoot_dat) <- c("DistancePercentile", "Heights", "Species", "Mean", "Lower", "Upper")
  return(hBoot_dat)}

# Plot the dispersal percentiles and the corresponding distances
# Error bars indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.rtailBootMHD.plot <- function(rtail_data, LColour, PColour, bNum, bLab){
  rtail_data[, 1] <- as.numeric(as.character(rtail_data[, 1]))
  rtDH <- subset(rtail_data, Heights == "Dist. Height")
  rtMH <- subset(rtail_data, Heights == "Max. Height")
  if(bNum == FALSE){
    plot(x = rtDH[, 1], y = rtDH[, 4], type = "l", col = LColour, xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", ylab = "Dispersal Distance (m)")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = FALSE)}
  if(bNum == TRUE){
    plot(x = rtDH[, 1], y = rtDH[, 4], type = "l", col = LColour, xlim = c(0.95, 1), ylim = c(0, 80),
         xlab = ifelse(bLab == TRUE, "Dispersal Quantile", NA), ylab = "Dispersal Distance (m)")}
  polygon(x = c(rtDH[, 1], rev(rtDH[, 1])), 
          y = c(rtDH[, 5], rev(rtDH[, 6])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = rtMH[, 1], y = rtMH[, 4], type = "l", lty = 3, col = PColour)
  polygon(x = c(rtMH[, 1], rev(rtMH[, 1])), 
          y = c(rtMH[, 5], rev(rtMH[, 6])), col = alpha(PColour, alpha = 0.2), border = NA)}

hBoot_MHD1_rtail <- ds.rtailBootMHD(ht_CN_NW$Height, ht_CN_NW_max$max, "CN")
hBoot_MHD2_rtail <- ds.rtailBootMHD(ht_CN_W$Height, ht_CN_W_max$max, "CN")
hBoot_MHD3_rtail <- ds.rtailBootMHD(ht_CA_NW$Height, ht_CA_NW_max$max, "CA")
hBoot_MHD4_rtail <- ds.rtailBootMHD(ht_CA_W$Height, ht_CA_W_max$max, "CA")

