##### Load libraries; initialise data ---------------------------------------------------------------------

# Load libraries
library(car)
library(xlsx)
library(tidyverse)
library(grid)
library(gridBase)
library(SuppDists)
library(lme4)
library(lmerTest)

# Load in raw weather data
data_ws1 <- read.csv("Data/Weather1.csv")
data_ws2 <- read.csv("Data/Weather2.csv")

# Load in terminal velocity data and remove rows with missing values
data_tv <- read.delim("Data/SeedDropData2.txt")
data_tv <- subset(data_tv, !is.na(drop.time))

# Load height data as xlsx
data_ht <- read.xlsx("Data/ThistleData.xlsx", sheetName = "Flowers")

# Load rosette as xlsx
data_rs <- read.xlsx("Data/ThistleData.xlsx", sheetName = "General")





##### Tidy data -------------------------------------------------------------------------------------------

# Remove rosette data rows with dead or non-flowering individuals
# Then merge initial rosette diameter with flower height data and keep only necessary columns
data_rs <- subset(data_rs, !is.na(LLL_t1) & !is.na(DM_t1) & F == 1)
data_ht <- merge(data_ht, data_rs, by = c("Row", "Group", "Plant", "Species"))
data_ht <- data_ht[, c(1:7, 9:10)]
names(data_ht)[7] <- "TRT"

# Note: we are only including flowers (f) and seed heads (s), not buds (b)
# We are also interested in only warmed (W) and not warmed (NW)

# Exclude partially-warmed (PW) treatments from analyses because we are not interested in them
# Examine heights for only flowers
data_ht <- subset(data_ht, TRT != "PW" & (Type == "f" | Type == "s"))

# Create vector of CN flower heights for each treatment group
ht_CN_NW <- subset(data_ht, Species == "CN" & TRT == "NW")
ht_CN_W <- subset(data_ht, Species == "CN" & TRT == "W")

# Create vector of CA flower heights for each treatment group
ht_CA_NW <- subset(data_ht, Species == "CA" & TRT == "NW")
ht_CA_W <- subset(data_ht, Species == "CA" & TRT == "W")

# Find maximum flower heights for each plant
data_ht %>% 
  subset(Type == "f" | Type == "s") %>% 
  group_by(Row, Group, Plant, Species, TRT, DM_t) %>% 
  summarise(Max = max(Height)) -> data_ht_max

# Create vector of CN max flower heights for each treatment group
ht_CN_NW_max <- subset(data_ht_max, Species == "CN" & TRT == "NW")
ht_CN_W_max <- subset(data_ht_max, Species == "CN" & TRT == "W")

# Create vector of CA max flower heights for each treatment group
ht_CA_NW_max <- subset(data_ht_max, Species == "CA" & TRT == "NW")
ht_CA_W_max <- subset(data_ht_max, Species == "CA" & TRT == "W")

# Calculate plot averages for maximum flower height
data_ht_max %>% 
  na.omit() %>% 
  group_by(Row, Group, Species, TRT) %>% 
  summarise(Max_PA = mean(Max),
            DM_t_PA = mean(DM_t)) -> data_ht_max_PA

# Calculate plot averages for mean flower height
data_ht %>% 
  na.omit() %>% 
  group_by(Row, Group, Species, TRT) %>% 
  summarise(Height_PA = mean(Height)) -> data_ht_PA
data_ht_PA <- cbind(data.frame(data_ht_PA), data_ht_max_PA$DM_t_PA)
names(data_ht_PA)[6] <- "DM_t_PA"





##### Set up dispersal framework --------------------------------------------------------------------------

# Create PDF of wind speeds at measurement height of 1 m
# Assume no seed release occurs for wind speeds of zero, so remove zero values
ws_values <- c(data_ws1$Wind1, data_ws2$Wind1)
ws_values <- ws_values[ws_values > 0]
ws_pdf <- density(ws_values, from = min(ws_values), to = max(ws_values), bw = 0.05)
ws_mean <- mean(ws_values)

# Create PDF of CN wind speeds
# Terminal velocity is drop tube length (1.25 m) divided by drop time
tv_values_CN <- na.omit(1.25/subset(data_tv, species == "n")$drop.time)
tv_pdf_CN <- density(tv_values_CN, from = min(tv_values_CN), to = max(tv_values_CN), bw = 0.05)
tv_mean_CN <- mean(tv_values_CN)

# Create PDF of CN wind speeds
# Terminal velocity is drop tube length (1.25 m) divided by drop time
tv_values_CA <- na.omit(1.25/subset(data_tv, species == "a")$drop.time)
tv_pdf_CA <- density(tv_values_CA, from = min(tv_values_CA), to = max(tv_values_CA), bw = 0.05)
tv_mean_CA <- mean(tv_values_CA)

# Function generating a dispersal kernel using WALD model (Katul et al. 2005)
# Code adapted from Skarpaas and Shea (2007)
WALD.b <- function(n, H, species){
  
  # Initialise physical constants
  K <- 0.4      # von Karman constant
  C0 <- 3.125   # Kolmogorov constant
  
  # Initialise other fixed quantities
  Aw <- 1.3     # Ratio of sigma_W to ustar
  h <- 0.15     # Grass cover height (m)
  d <- 0.7*h    # Zero-plane displacement (m)
  z0 <- 0.1*h   # Roughness length (m)
  zm <- 1       # Wind speed measurement height (m)
  
  # Let n be the number of simulation replications
  # Let H be the seed release height
  
  # Simulate wind speeds from empirical distribution of wind speeds
  Um <- rnorm(n, sample(ws_values, size = n, replace = TRUE), ws_pdf$bw)
  
  # Simulate terminal velocities from empirical distribution of terminal velocities
  if(species == "CN"){
    f <- rnorm(n, sample(tv_values_CN, size = n, replace = TRUE), tv_pdf_CN$bw)}
  if(species == "CA"){
    f <- rnorm(n, sample(tv_values_CA, size = n, replace = TRUE), tv_pdf_CA$bw)}
  
  # Calculate ustar, the friction velocity
  ustar <- K*Um*(log((zm - d)/z0))^(-1)
  
  # Set up integrand for wind speed between vegetation surface and drop height H
  integrand <- function(z){
    (1/K)*(log((z - d)/z0))}
  
  # Integrate to obtain U
  U <- (ustar/H)*integrate(integrand, lower = d + z0, upper = H)$value
  
  # Calculate instability parameter
  sigma <- 2*(Aw^2)*sqrt((K*(H - d)*ustar)/(C0*U))
  
  # Calculate scale parameter lambda
  lambda <- (H/sigma)^2
  
  # Calculate location parameter nu
  nu <- H*U/f
  
  # Generate inverse Gaussian distribution
  return(rinvGauss(n, nu = nu, lambda = lambda))}

# Function generating a dispersal kernel across a distribution of heights
WALD.h <- function(n, h, species){
  
  # Let n be the number of release heights
  # Let h be the observed distribution of flower heights
  
  # Use 100 replicates for each height
  nr <- 100
  
  # Get PDF of heights
  h_PDF <- density(h)
  
  # Generate release heights from empirical distribution of terminal velocities
  # Convert heights from cm to m since WALD model is coded in metres
  hRange <- rnorm(n, sample(h, size = n, replace = TRUE), h_PDF$bw)/100
  
  # Truncate heights below 15 cm (canopy height) since these plants will likely not flower
  # Also, WALD model does not cover below-canopy dispersal
  hRange <- hRange[hRange >= 0.15]
  
  # Simulate seed release events for each height
  # Erase attributes if generated by NA omission
  output <- na.omit(as.numeric(sapply(hRange, WALD.b, n = nr, species = species)))
  attributes(output) <- NULL
  return(output)}





##### Functions for height distribution PDF with 95% bootstrap interval -----------------------------------

# Set up function to estimate PDF for a bootstrapped sample
ht.pdf <- function(heights){
  hList.dens <- approxfun(density(heights, from = 0, to = 200))
  return(hList.dens(seq(0, 200, by = 0.1)))}

# Function to bootstrap height distributions and estimate 95% bootstrap interval
# For each bootstrap sample 500 release heights; bootstrap 1000 times (500000 replicates in total)
# Otherwise, only conduct Kolmogorov-Smirnov test between the two distributions
ht.pdfBoot <- function(h1, h2, ks.only = FALSE){
  if(ks.only == FALSE){
    hBoot_1 <- data.frame(replicate(1000, ht.pdf(sample(h1, size = 500, replace = TRUE))))
    hBoot_2 <- data.frame(replicate(1000, ht.pdf(sample(h2, size = 500, replace = TRUE))))
    return(data.frame(seq(0, 200, by = 0.1),
                      apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                      apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                      apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                      apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                      apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                      apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}
  if(ks.only == TRUE){
    ks.test(sample(h1, size = 500000, replace = TRUE), sample(h2, size = 500000, replace = TRUE),
            alternative = "two.sided")}}

# Function to plot bootstrapped height distributions, with mean line and 95% band
ht.pdfBoot.plot <- function(bootData, bottom){
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Flower head height (cm)", cex = 0.6)}
  axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability density", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Same as above, but with histogram of observations and no 95% BI
ht.pdfBoot.plot2 <- function(bootData, h1, h2, bottom){
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(h1, col = alpha("black", alpha = 0.2), freq = FALSE, xlim = c(0, 200), ylim = c(0, 0.03),
         main = "", xaxt = "n", yaxt = "n")
    plot(h2, col = alpha("red", alpha = 0.2), freq = FALSE, xlim = c(0, 200), ylim = c(0, 0.03),
         add = TRUE)
    lines(x = bootData[, 1], y = bootData[, 2])
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)
    box()}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(h1, col = alpha("black", alpha = 0.2), freq = FALSE, xlim = c(0, 200), ylim = c(0, 0.03),
         main = "", xaxt = "n", yaxt = "n")
    plot(h2, col = alpha("red", alpha = 0.2), freq = FALSE, xlim = c(0, 200), ylim = c(0, 0.03),
         add = TRUE)
    lines(x = bootData[, 1], y = bootData[, 2])
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Flower head height (cm)", cex = 0.6)
    box()}
  axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability density", cex = 0.6)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")}





##### Functions for warmed vs not warmed PDF (dispersal kernels) ------------------------------------------

# Set up function to create a single sample and estimate PDF
ds.pdf <- function(heights, species){
  hList <- WALD.h(100, heights, species)
  hList.dens <- approxfun(density(hList))
  return(hList.dens(seq(0, 8, by = 0.01)))}

# Function to calculate mean dispersal distance for entire kernel
# Also calculates 95% bootstrap interval on mean
ds.mean <- function(h, species){
  hList <- replicate(1000, mean(WALD.h(100, h, species)))
  return(c(mean(hList), quantile(hList, c(0.025, 0.975))))}

# Function to bootstrap dispersal kernels and estimate 95% bootstrap interval
# For each kernel sample 100 release heights with 100 seed releases each, and repeat 1000 times
# K-S test compares two kernels constructed from 10 million seed releases each
ds.pdfBoot <- function(h1, h2, type, species, ks.only = FALSE){
  if(ks.only == FALSE){
    hBoot_1 <- data.frame(replicate(1000, ds.pdf(h1, species), simplify = "matrix"))
    hBoot_2 <- data.frame(replicate(1000, ds.pdf(h2, species), simplify = "matrix"))
    hBoot_dat <- data.frame(seq(0, 8, by = 0.01),
                            apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                            apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                            apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                            apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                            apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975))
    if(type == "WNW"){
      names(hBoot_dat) <- c("Distance", "NW_Mean", "NW_Lower", "NW_Upper", "W_Mean", "W_Lower", "W_Upper")}
    if(type == "MHD"){
      names(hBoot_dat) <- c("Distance", "HD_Mean", "HD_Lower", "HD_Upper", "M_Mean", "M_Lower", "M_Upper")}
    return(hBoot_dat)}
  if(ks.only == TRUE){
    ks.test(WALD.h(10000, h1, species), WALD.h(10000, h2, species), alt = "two.sided")}}

# Function to plot bootstrapped dispersal kernels, with mean line and 95% band
ds.pdfBoot.plot <- function(bootData, bottom){
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = 0:7, labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = c(0, 0.2, 0.4, 0.6), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability density", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}





##### Functions for warmed vs not warmed CCDF -------------------------------------------------------------

# Set up function to create a single sample and estimate CCDF
ds.ccdf <- function(heights, species){
  hList <- sort(WALD.h(100, heights, species))
  hList.exceed <- function(cutoff){
    return(length(hList[hList >= cutoff])/length(hList))}
  hList.ccdf <- approxfun(hList, sapply(hList, hList.exceed), yleft = 1, yright = 0)
  return(hList.ccdf(seq(0, 60, by = 0.01)))}

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
ds.ccdfBoot <- function(h1, h2, type, species){
  hBoot_1 <- data.frame(replicate(1000, ds.ccdf(h1, species), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.ccdf(h2, species), simplify = "matrix"))
  hBoot_dat <- data.frame(seq(0, 60, by = 0.01),
                          apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                          apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                          apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                          apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                          apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                          apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975))
  if(type == "WNW"){
    names(hBoot_dat) <- c("Distance", "NW_Mean", "NW_Lower", "NW_Upper", "W_Mean", "W_Lower", "W_Upper")}
  if(type == "MHD"){
    names(hBoot_dat) <- c("Distance", "HD_Mean", "HD_Lower", "HD_Upper", "M_Mean", "M_Lower", "M_Upper")}
  return(hBoot_dat)}

# Function to plot bootstrapped CCDF, with mean line and 95% band
ds.ccdfBoot.plot <- function(bootData, bottom){
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 30), ylim = c(0, 1),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 30, by = 5), labels = FALSE)}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 30), ylim = c(0, 1),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 30, by = 5), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = c(0, 0.25, 0.5, 0.75, 1), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", col = "red")
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha("red", alpha = 0.2), border = NA)}





##### Functions for warmed vs not warmed CCDF ratios ------------------------------------------------------

# Function to bootstrap CCDF ratio and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
ds.ccdfRatioBoot <- function(h1, h2, type, species){
  hBoot_1 <- data.frame(replicate(1000, ds.ccdf(h1, species), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.ccdf(h2, species), simplify = "matrix"))
  hBoot_a <- hBoot_2/hBoot_1
  mean.exinf <- function(x){
    return(mean(x[!is.infinite(x)]))}
  quantile.exinf <- function(x, probs){
    return(quantile(x[!is.infinite(x)], probs = probs, na.rm = TRUE))}
  hBoot_dat <- data.frame(seq(0, 60, by = 0.01),
                          apply(X = hBoot_a, MARGIN = 1, FUN = mean.exinf),
                          apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.025),
                          apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.975))
  if(type == "WNW"){
    names(hBoot_dat) <- c("Distance", "WNW_MeanRatio", "WNW_LowerRatio", "WNW_UpperRatio")}
  if(type == "MHD"){
    names(hBoot_dat) <- c("Distance", "MHD_MeanRatio", "MHD_LowerRatio", "MHD_UpperRatio")}
  return(hBoot_dat)}

# Function to plot bootstrapped CCDF ratio, with mean line and 95% band
ds.ccdfRatioBoot.plot <- function(bootData, bottom){
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 60, by = 10), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = 0:4, labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "W/NW Risk Ratio", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  abline(h = 1, lty = 3)}





##### Functions for warmed vs not right tail dispersal percentile distances -------------------------------

# Set up function to create bootstrapped sample and canculate nth percentiles
ds.rtail <- function(heights, species){
  hList <- WALD.h(100, heights, species)
  return(quantile(hList, probs = seq(0.9500, 0.9999, by = 0.0001)))}

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For maximum, same as above, but sampling from maximum release heights rather than full distribution
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
ds.rtailBootWNW <- function(h1, h2, species){
  hBoot_1 <- data.frame(replicate(1000, ds.rtail(h1, species), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.rtail(h2, species), simplify = "matrix"))
  hBoot_dat <- data.frame(cbind(rep(seq(0.9500, 0.9999, by = 0.0001), 2),
                                c(rep("Not Warmed", 500), rep("Warmed", 500))), 
                          rep(species, 1000),
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
  par(cex.axis = 0.45, tcl = -0.2)
  if(bottom == FALSE){
    par(mar = c(1, 1.75, 0.75, 0.75))
    plot(x = rtNW[, 1], y = rtNW[, 4], type = "l", xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = FALSE)}
  if(bottom == TRUE){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = rtNW[, 1], y = rtNW[, 4], type = "l", xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal percentile", cex = 0.6)}
  axis(side = 2, at = c(0, 20, 40, 60, 80), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Dispersal distance (m)", cex = 0.6)
  polygon(x = c(rtNW[, 1], rev(rtNW[, 1])), 
          y = c(rtNW[, 5], rev(rtNW[, 6])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = rtW[, 1], y = rtW[, 4], type = "l", col = "red")
  polygon(x = c(rtW[, 1], rev(rtW[, 1])), 
          y = c(rtW[, 5], rev(rtW[, 6])), col = alpha("red", alpha = 0.2), border = NA)}





##### Functions for max height vs height distribution PDF (dispersal kernels) -----------------------------

# Function to plot the dispersal kernels
# Bands indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.pdfBoot.plot2 <- function(bootData, LColour, PColour, position){
  par(cex.axis = 0.45, tcl = -0.2)
  if(position == "top"){
    par(mar = c(0.75, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(position == "centre"){
    par(mar = c(1, 1.75, 0.25, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(position == "bottom"){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = 0:7, labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = c(0, 0.2, 0.4, 0.6), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability density", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 5], type = "l", lty = 3, col = PColour)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 6], rev(bootData[, 7])), col = alpha(PColour, alpha = 0.2), border = NA)}





##### Functions for max height vs height distribution CCDF ------------------------------------------------

# Plot the dispersal percentiles and the corresponding distances
# Error bars indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
ds.ccdfBoot.plot2 <- function(bootData, LColour, PColour, position){
  par(cex.axis = 0.45, tcl = -0.2)
  if(position == "top"){
    par(mar = c(0.75, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 4], type = "l", col = LColour, xlim = c(0, 30), ylim = c(0, 1),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 30, by = 5), labels = FALSE)}
  if(position == "centre"){
    par(mar = c(1, 1.75, 0.25, 0.75))
    plot(x = bootData[, 1], y = bootData[, 4], type = "l", col = LColour, xlim = c(0, 30), ylim = c(0, 1),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 30, by = 5), labels = FALSE)}
  if(position == "bottom"){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 4], type = "l", col = LColour, xlim = c(0, 30), ylim = c(0, 1),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 30, by = 5), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = c(0, 0.25, 0.5, 0.75, 1), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Probability", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 5], rev(bootData[, 6])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = bootData[, 1], y = bootData[, 4], type = "l", lty = 3, col = PColour)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 5], rev(bootData[, 6])), col = alpha(PColour, alpha = 0.2), border = NA)}





##### Functions for max height vs height distribution CCDF ratio ------------------------------------------

# Function to plot bootstrapped CCDF, with mean line and 95% band
ds.ccdfRatioBoot.plot2 <- function(bootData, position){
  par(cex.axis = 0.45, tcl = -0.2)
  if(position == "top"){
    par(mar = c(0.75, 1.75, 0.75, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(position == "centre"){
    par(mar = c(1, 1.75, 0.25, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(position == "bottom"){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = bootData[, 1], y = bootData[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0, 60, by = 10), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal distance (m)", cex = 0.6)}
  axis(side = 2, at = 0:4, labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "M/HD Risk Ratio", cex = 0.6)
  polygon(x = c(bootData[, 1], rev(bootData[, 1])), 
          y = c(bootData[, 3], rev(bootData[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  abline(h = 1, lty = 3)}





##### Functions for max height vs height distribution right tail dispersal percentile distances -----------

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For maximum, same as above, but sampling from maximum release heights rather than full distribution
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
ds.rtailBootMHD <- function(distHeight, maxHeight, species){
  hBoot_1 <- data.frame(replicate(1000, ds.rtail(distHeight, species), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, ds.rtail(maxHeight, species), simplify = "matrix"))
  hBoot_dat <- data.frame(cbind(rep(seq(0.9500, 0.9999, by = 0.0001), 2),
                                c(rep("Dist. Height", 500), rep("Max. Height", 500))), 
                          rep(species, 1000),
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
ds.rtailBootMHD.plot <- function(rtail_data, LColour, PColour, position){
  rtail_data[, 1] <- as.numeric(as.character(rtail_data[, 1]))
  rtDH <- subset(rtail_data, Heights == "Dist. Height")
  rtMH <- subset(rtail_data, Heights == "Max. Height")
  par(cex.axis = 0.45, tcl = -0.2)
  if(position == "top"){
    par(mar = c(0.75, 1.75, 0.75, 0.75))
    plot(x = rtDH[, 1], y = rtDH[, 4], type = "l", col = LColour, xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = FALSE)}
  if(position == "centre"){
    par(mar = c(1, 1.75, 0.25, 0.75))
    plot(x = rtDH[, 1], y = rtDH[, 4], type = "l", col = LColour, xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = FALSE)}
  if(position == "bottom"){
    par(mar = c(1.75, 1.75, 0, 0.75))
    plot(x = rtDH[, 1], y = rtDH[, 4], type = "l", col = LColour, xlim = c(0.95, 1), ylim = c(0, 80),
         xaxt = "n", yaxt = "n")
    axis(side = 1, at = seq(0.95, 1, by = 0.01), labels = TRUE, mgp = c(0, 0, 0))
    mtext(side = 1, line = 0.75, "Dispersal percentile", cex = 0.6)}
  axis(side = 2, at = c(0, 20, 40, 60, 80), labels = TRUE, mgp = c(0, 0.21, 0))
  mtext(side = 2, line = 0.95, "Dispersal distance (m)", cex = 0.6)
  polygon(x = c(rtDH[, 1], rev(rtDH[, 1])), 
          y = c(rtDH[, 5], rev(rtDH[, 6])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = rtMH[, 1], y = rtMH[, 4], type = "l", lty = 3, col = PColour)
  polygon(x = c(rtMH[, 1], rev(rtMH[, 1])), 
          y = c(rtMH[, 5], rev(rtMH[, 6])), col = alpha(PColour, alpha = 0.2), border = NA)}

