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
data_ws1 <- read.csv("Weather1.csv")
data_ws2 <- read.csv("Weather2.csv")

# Load in terminal velocity data
# Use ambient TVs since we're only examining warming effects on height distribution
data_tv <- read.csv("SeedDropData.csv")
data_tv <- subset(data_tv, Warming == "A")

# Load height data as xlsx
data_ht <- read.xlsx(file.choose(), sheetName = "Flowers")

# Note: we are only including flowers (f) and seed heads (s), not buds (b)
# Analyses below will focus on warmed (W) vs not warmed (NW)

# Exclude partially-warmed (PW) treatments from analyses because we are not interested in them
# Examine heights for only flowers
data_ht <- subset(data_ht, TRT != "PW" & (Type == "f" | Type == "s"))

# Grab values for CN flower heights, for each treatment group
ht_CN_NW <- subset(data_ht, Species == "CN" & TRT == "NW")
ht_CN_W <- subset(data_ht, Species == "CN" & TRT == "W")

# Grab values for CA flower heights, for each treatment group
ht_CA_NW <- subset(data_ht, Species == "CA" & TRT == "NW")
ht_CA_W <- subset(data_ht, Species == "CA" & TRT == "W")

# Get maximum flower heights for each plant
data_ht %>% 
  subset(Type == "f" | Type == "s") %>% 
  group_by(Row, Group, Plant, Species, TRT) %>% 
  summarise(max = max(Height)) -> data_ht_max

# Grab values for CN max flower heights, for each treatment group
ht_CN_NW_max <- subset(data_ht_max, Species == "CN" & TRT == "NW")
ht_CN_W_max <- subset(data_ht_max, Species == "CN" & TRT == "W")

# Grab values for CA max flower heights, for each treatment group
ht_CA_NW_max <- subset(data_ht_max, Species == "CA" & TRT == "NW")
ht_CA_W_max <- subset(data_ht_max, Species == "CA" & TRT == "W")





##### Statistical tests on CN height data -----------------------------------------------------------------

# Examine effect of treatment using LME model, with row and group as random effects
# Using Gaussian link function (default)
mod_CN <- lmer(Height ~ TRT + (1|Row) + (1|Group), data = subset(data_ht, Species == "CN"))

# Warming treatment fixed effect is significant
# Note: ANOVA and t-test on TRT are equivalent for only 2 levels (NW and W)
anova(mod_CN)
summary(mod_CN)

# Residuals seem to be normally distributed
# Shapiro-Wilk test disagrees, but it is sensitive to slight departures from normal in large samples
plot(mod_CN)
qqnorm(resid(mod_CN))
qqline(resid(mod_CN))
shapiro.test(resid(mod_CN))

# Random effect for Group is normally distributed
qqnorm(ranef(mod_CN)$Group$`(Intercept)`)
qqline(ranef(mod_CN)$Group$`(Intercept)`)
shapiro.test(ranef(mod_CN)$Group$`(Intercept)`)

# Random effect for Row is normally distributed
qqnorm(ranef(mod_CN)$Row$`(Intercept)`)
qqline(ranef(mod_CN)$Row$`(Intercept)`)
shapiro.test(ranef(mod_CN)$Row$`(Intercept)`)

# Residual variances are homogeneous at alpha of 0.05
leveneTest(residuals(mod_CN) ~ subset(data_ht, Species == "CN" )$TRT)

# Assumptions for the LME model are satisfied
# Now look at normality of each treatment group

# Normality: Not warmed treatment
shapiro.test(ht_CN_NW$Height)
qqnorm(ht_CN_NW$Height)
qqline(ht_CN_NW$Height)

# Normality: Warmed treatment
shapiro.test(ht_CN_W$Height)
qqnorm(ht_CN_W$Height)
qqline(ht_CN_W$Height)

# Shapiro-Wilk test is extremely sensitive to small departures from normality at large sample sizes
# Heights are pretty much normally distributed... some deviation at the tails, but this is acceptable

# Use Levene's Test to assess if there are differences in variance between groups
# Test indicates that there is not significant difference in variance
leveneTest(Height ~ TRT, data = subset(data_ht, Species == "CN"))

# We will perform Student's t-test (equal variance) on warmed vs non-warmed treatments
# The test should be fine given only minor deviations from normality
t.test(ht_CN_W$Height, ht_CN_NW$Height, alt = "two.sided", var.equal = TRUE)

# There is a significant height difference between warmed and unwarmed groups





##### Statistical tests on CA height data -----------------------------------------------------------------

# Examine effect of treatment using LME model, with row and group as random effects
# Using Gaussian link function (default)
mod_CA <- lmer(Height ~ TRT + (1|Row) + (1|Group), data = subset(data_ht, Species == "CA"))

# Singular fit; Row random effect makes no contribution, so remove it
summary(mod_CA)
mod_CA <- lmer(Height ~ TRT + (1|Group), data = subset(data_ht, Species == "CA"))

# Warming treatment fixed effect is significant
# Note: ANOVA and t-test on TRT are equivalent for only 2 levels (NW and W)
anova(mod_CA)
summary(mod_CA)

# Residuals seem to be normally distributed
# Shapiro-Wilk test disagrees, but it is sensitive to slight departures from normal in large samples
plot(mod_CA)
qqnorm(resid(mod_CA))
qqline(resid(mod_CA))
shapiro.test(resid(mod_CA))

# Random effect for Group is normally distributed
qqnorm(ranef(mod_CA)$Group$`(Intercept)`)
qqline(ranef(mod_CA)$Group$`(Intercept)`)
shapiro.test(ranef(mod_CA)$Group$`(Intercept)`)

# Residual variances are not quite homogeneous at alpha of 0.05
# Slightly problematic for the LME model
leveneTest(residuals(mod_CA) ~ subset(data_ht, Species == "CA")$TRT)

# Now look at normality of each treatment group

# Normality: Not warmed treatment
shapiro.test(ht_CA_NW$Height)
qqnorm(ht_CA_NW$Height)
qqline(ht_CA_NW$Height)

# Normality: Warmed treatment
shapiro.test(ht_CA_W$Height)
qqnorm(ht_CA_W$Height)
qqline(ht_CA_W$Height)

# Shapiro-Wilk test is extremely sensitive to small departures from normality at large sample sizes
# Heights are pretty much normally distributed... some deviation at the tails, but this is acceptable

# Use Levene's Test to assess if there are differences in variance between groups
# Test indicates that there is significant difference in variance
leveneTest(Height ~ TRT, data = subset(data_ht, Species == "CA"))

# We will perform Welch's t-test (unequal variance) on warmed vs non-warmed treatments
# The test should be fine given only minor deviations from normality
t.test(ht_CA_W$Height, ht_CA_NW$Height, alt = "two.sided", var.equal = FALSE)

# There is a significant height difference between warmed and unwarmed groups





##### Mean height plots -----------------------------------------------------------------------------------

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

# Prepare graphics device
jpeg(filename = "Figure 1.jpeg", width = 826, height = 568, units = "px")

# Graph mean heights
ggplot(ht_stats, aes(x = Species, y = MeanHeight, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = MeanHeight - SEHeight, ymax = MeanHeight + SEHeight), 
                width = 0.3, position = position_dodge(0.7)) +
  coord_cartesian(ylim = c(70, 120)) +
  scale_fill_manual(values = c("gray30", "red")) +
  xlab("Species") +
  ylab("Mean Flower Height (cm)") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"),
        legend.background	= element_blank(),
        legend.position = c(0.08, 0.925),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14))

# Finalise graphics save
dev.off()





##### Height distribution plots ---------------------------------------------------------------------------

# Set up function to estimate PDF for a bootstrapped sample
HT.eval <- function(heights){
  hList.dens <- approxfun(density(heights, from = 0, to = 200))
  return(hList.dens(seq(0, 200, by = 0.1)))}

# Function to bootstrap height distributions and estimate 95% bootstrap interval
# For each bootstrap sample 500 release heights; bootstrap 1000 times
HT.pdf <- function(NWHeight, WHeight){
  hBoot_1 <- data.frame(replicate(1000, HT.eval(sample(NWHeight, size = 500, replace = TRUE))))
  hBoot_2 <- data.frame(replicate(1000, HT.eval(sample(WHeight, size = 500, replace = TRUE))))
  return(data.frame(seq(0, 200, by = 0.1),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped height distributions, with mean line and 95% band
HT.pdfPlot <- function(HT_data, bottom){
  if(bottom == FALSE){
    plot(x = HT_data[, 1], y = HT_data[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xaxt = "n", yaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  if(bottom == TRUE){
    plot(x = HT_data[, 1], y = HT_data[, 2], type = "l", xlim = c(0, 200), ylim = c(0, 0.03),
         xlab = "Flower Height (cm)", yaxt = "n", ylab = "Probability Density")
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  polygon(x = c(HT_data[, 1], rev(HT_data[, 1])), 
          y = c(HT_data[, 3], rev(HT_data[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = HT_data[, 1], y = HT_data[, 5], type = "l", col = "red")
  polygon(x = c(HT_data[, 1], rev(HT_data[, 1])), 
          y = c(HT_data[, 6], rev(HT_data[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 2.jpeg", width = 826, height = 568, units = "px")

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
hBoot_dat_HD1 <- HT.pdf(ht_CN_NW$Height, ht_CN_W$Height)
HT.pdfPlot(hBoot_dat_HD1, bottom = FALSE)
#abline(v = c(subset(ht_stats, Species == "CN" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CN" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_HD2 <- HT.pdf(ht_CA_NW$Height, ht_CA_W$Height)
HT.pdfPlot(hBoot_dat_HD2, bottom = TRUE)
#abline(v = c(subset(ht_stats, Species == "CA" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CA" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# Note: commented out code above adds a vertical line at the mean

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("A", "B"), x = rep(0.11, 2), y = c(0.90, 0.47),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W height distributions display significant difference for CN and CA
ks.test(hBoot_dat_HD1[, 2], hBoot_dat_HD1[, 5], alt = "two.sided")
ks.test(hBoot_dat_HD2[, 2], hBoot_dat_HD2[, 5], alt = "two.sided")





##### Set up dispersal framework --------------------------------------------------------------------------

# Create PDF of wind speeds
# Assume no seed release occurs for wind speeds of zero, so remove zero values
WSValues <- c(data_ws1$Wind1, data_ws2$Wind1)
WSValues <- WSValues[WSValues > 0]
WSValues_PDF <- density(WSValues, from = min(WSValues), to = max(WSValues), bw = 0.05)
WSValuesMean <- mean(WSValues)

# Create PDF of wind speeds
# Terminal velocity is drop tube length divided by drop time
TVValues <- na.omit(1.25/data_tv$DT.Avg)
TVValues_PDF <- density(TVValues, from = min(TVValues), to = max(TVValues), bw = 0.05)
TVValuesMean <- mean(TVValues)

# Function generating a dispersal kernel using WALD model (Katul et al. 2005)
# Code adapted from Skarpaas and Shea (2007)
WALD.b <- function(n, H){
  
  # Initialise physical constants
  K <- 0.4      # von Karman constant
  C0 <- 3.125   # Kolmogorov constant
  
  # Initialise other fixed quantities
  Aw <- 1.3     # Ratio of sigmaw to ustar
  h <- 0.15     # Grass cover height
  d <- 0.7*h    # Zero-plane displacement
  z0 <- 0.1*h   # Roughness length
  zm <- 1       # Wind speed measurement height
  
  # Let n be the number of simulation replications
  # Let H be the seed release height
  
  # Simulate wind speeds from empirical distribution of wind speeds
  Um <- rnorm(n, sample(WSValues, size = n, replace = TRUE), WSValues_PDF$bw)
  
  # Simulate terminal velocities from empirical distribution of terminal velocities
  f <- rnorm(n, sample(TVValues, size = n, replace = TRUE), TVValues_PDF$bw)
  
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
WALD.h <- function(n, h){
  
  # Let n be the number of release heights
  # Let h be the distribution of heights
  
  # Use 100 replicates for each height
  nr <- 100
  
  # Get PDF of heights
  h_PDF <- density(h)
  
  # Generate normally-distributed release heights, and convert cm to m too
  hRange <- rnorm(n, sample(h, size = n, replace = TRUE), h_PDF$bw)/100
  
  # Truncate heights below 15 cm since these plants will likely not flower
  hRange <- hRange[hRange >= 0.15]
  
  # Simulate seed release events for each height
  # Erase attributes if generated by NA omission
  output <- na.omit(as.numeric(sapply(hRange, WALD.b, n = nr)))
  attributes(output) <- NULL
  return(output)}





##### Plot dispersal kernels: warmed vs not warmed PDF ----------------------------------------------------

# Set up function to create a single sample and estimate PDF
WALD.hBoot <- function(heights){
  hList <- WALD.h(100, heights)
  hList.dens <- approxfun(density(hList))
  return(hList.dens(seq(0, 8, by = 0.01)))}

# Function to bootstrap dispersal kernels and estimate 95% bootstrap interval
# For each kernel sample 100 release heights with 100 seed releases each, and repeat 1000 times
WNW.pdf <- function(NWHeight, WHeight){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBoot(NWHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBoot(WHeight), simplify = "matrix"))
  return(data.frame(seq(0, 8, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped dispersal kernels, with mean line and 95% band
WNW.pdfPlot <- function(WNW_data, bottom){
  if(bottom == FALSE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(bottom == TRUE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 7), ylim = c(0, 0.6),
         xlab = "Dispersal Distance (m)", ylab = "Probability Density")}
  polygon(x = c(WNW_data[, 1], rev(WNW_data[, 1])), 
          y = c(WNW_data[, 3], rev(WNW_data[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = WNW_data[, 1], y = WNW_data[, 5], type = "l", col = "red")
  polygon(x = c(WNW_data[, 1], rev(WNW_data[, 1])), 
          y = c(WNW_data[, 6], rev(WNW_data[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 3.jpeg", width = 826, height = 568, units = "px")

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
hBoot_dat_WNW1 <- WNW.pdf(ht_CN_NW$Height, ht_CN_W$Height)
WNW.pdfPlot(hBoot_dat_WNW1, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_WNW2 <- WNW.pdf(ht_CA_NW$Height, ht_CA_W$Height)
WNW.pdfPlot(hBoot_dat_WNW2, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("A", "B"), x = rep(0.11, 2), y = c(0.90, 0.47),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_dat_WNW1[, 2], hBoot_dat_WNW1[, 5], alt = "two.sided")
ks.test(hBoot_dat_WNW2[, 2], hBoot_dat_WNW2[, 5], alt = "two.sided")





##### Plot dispersal kernels: warmed vs not warmed CCDF ---------------------------------------------------

# Set up function to create a single sample and estimate CCDF
WALD.hBootCCDF <- function(heights){
  hList <- sort(WALD.h(100, heights))
  hList.exceed <- function(cutoff){
    return(length(hList[hList >= cutoff])/length(hList))}
  hList.ccdf <- approxfun(hList, sapply(hList, hList.exceed), yleft = 1, yright = 0)
  return(hList.ccdf(seq(0, 60, by = 0.01)))}

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
WNW.CCDF <- function(NWHeight, WHeight){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBootCCDF(NWHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBootCCDF(WHeight), simplify = "matrix"))
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot bootstrapped CCDF, with mean line and 95% band
WNW.CCDFPlot <- function(WNW_data, bottom){
  if(bottom == FALSE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 1),
         xaxt = "n", ylab = "Complement of Cumulative Probability")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bottom == TRUE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 1),
         xlab = "Dispersal Distance (m)", ylab = "Complement of Cumulative Probability")}
  polygon(x = c(WNW_data[, 1], rev(WNW_data[, 1])), 
          y = c(WNW_data[, 3], rev(WNW_data[, 4])), col = alpha("black", alpha = 0.2), border = NA)
  lines(x = WNW_data[, 1], y = WNW_data[, 5], type = "l", col = "red")
  polygon(x = c(WNW_data[, 1], rev(WNW_data[, 1])), 
          y = c(WNW_data[, 6], rev(WNW_data[, 7])), col = alpha("red", alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 3.1.jpeg", width = 826, height = 568, units = "px")

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
hBoot_dat_WNW1CCDF <- WNW.CCDF(ht_CN_NW$Height, ht_CN_W$Height)
WNW.CCDFPlot(hBoot_dat_WNW1CCDF, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_WNW2CCDF <- WNW.CCDF(ht_CA_NW$Height, ht_CA_W$Height)
WNW.CCDFPlot(hBoot_dat_WNW2CCDF, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("A", "B"), x = rep(0.11, 2), y = c(0.90, 0.47),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_dat_WNW1[, 2], hBoot_dat_WNW1[, 5], alt = "two.sided")
ks.test(hBoot_dat_WNW2[, 2], hBoot_dat_WNW2[, 5], alt = "two.sided")





##### Plot dispersal kernels: warmed vs not warmed CCDF ratio ---------------------------------------------

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
WNW.CCDFRatio <- function(NWHeight, WHeight){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBootCCDF(NWHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBootCCDF(WHeight), simplify = "matrix"))
  hBoot_a <- hBoot_2/hBoot_1
  mean.exinf <- function(x){
    return(mean(x[!is.infinite(x)]))}
  quantile.exinf <- function(x, probs){
    return(quantile(x[!is.infinite(x)], probs = probs))}
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_a, MARGIN = 1, FUN = mean.exinf),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.025),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.975)))}

# Function to plot bootstrapped CCDF, with mean line and 95% band
WNW.CCDFRatioPlot <- function(WNW_data, bottom){
  if(bottom == FALSE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", ylab = "W/NW CCDF Ratio")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bottom == TRUE){
    plot(x = WNW_data[, 1], y = WNW_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xlab = "Dispersal Distance (m)", ylab = "W/NW CCDF Ratio")}
  polygon(x = c(WNW_data[, 1], rev(WNW_data[, 1])), 
          y = c(WNW_data[, 3], rev(WNW_data[, 4])), col = alpha("black", alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 3.2.jpeg", width = 826, height = 568, units = "px")

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
hBoot_dat_WNW1CCDFRatio <- WNW.CCDFRatio(ht_CN_NW$Height, ht_CN_W$Height)
WNW.CCDFRatioPlot(hBoot_dat_WNW1CCDFRatio, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_WNW2CCDFRatio <- WNW.CCDFRatio(ht_CA_NW$Height, ht_CA_W$Height)
WNW.CCDFRatioPlot(hBoot_dat_WNW2CCDFRatio, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("A", "B"), x = rep(0.11, 2), y = c(0.90, 0.47),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_dat_WNW1[, 2], hBoot_dat_WNW1[, 5], alt = "two.sided")
ks.test(hBoot_dat_WNW2[, 2], hBoot_dat_WNW2[, 5], alt = "two.sided")




##### Analyse dispersal kernels: warmed vs not warmed tail-end dispersal distances ------------------------

# Set up function to create bootstrapped sample and canculate nth percentiles
WALD.hBoot2 <- function(heights){
  hList <- WALD.h(100, heights)
  return(quantile(hList, probs = seq(0.9500, 0.9999, by = 0.0001)))}

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
WNW.rtail <- function(NWHeight, WHeight, Species){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBoot2(NWHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBoot2(WHeight), simplify = "matrix"))
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
WNW.rtailPlot <- function(rtail_data, bottom){
  rtail_data[, 1] <- as.numeric(as.character(rtail_data[, 1]))
  rtNW <- subset(rtail_data, Treatment == "Not Warmed")
  rtW <- subset(rtail_data, Treatment == "Warmed")
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

# Prepare graphics device
jpeg(filename = "Figure 4.jpeg", width = 826, height = 568, units = "px")

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
hboot_dat_RT1 <- WNW.rtail(ht_CN_NW$Height, ht_CN_W$Height, "CN")
WNW.rtailPlot(hboot_dat_RT1, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hboot_dat_RT2 <- WNW.rtail(ht_CA_NW$Height, ht_CA_W$Height, "CA")
WNW.rtailPlot(hboot_dat_RT2, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("A", "B"), x = rep(0.11, 2), y = c(0.90, 0.47),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot dispersal kernels: max height vs height distribution -------------------------------------------

# Function to calculate dispersal kernels with mean height or distribution of heights
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 50000 seed release events and is repeated 1000 times
MHD.pdf <- function(heights, maxheights){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBoot(heights), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBoot(maxheights), simplify = "matrix"))
  return(data.frame(seq(0, 8, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Function to plot the dispersal kernels
# Bands indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
MHD.pdfPlot <- function(PDF_Data, LColour, PColour, bNum, bLab){
  if(bNum == FALSE){
    plot(x = PDF_Data[, 1], y = PDF_Data[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xaxt = "n", ylab = "Probability Density")
    axis(side = 1, at = 0:7, labels = FALSE)}
  if(bNum == TRUE){
    plot(x = PDF_Data[, 1], y = PDF_Data[, 2], type = "l", col = LColour, xlim = c(0, 7), ylim = c(0, 0.6),
         xlab = ifelse(bLab == TRUE, "Dispersal Distance (m)", NA), ylab = "Probability Density")}
  polygon(x = c(PDF_Data[, 1], rev(PDF_Data[, 1])), 
          y = c(PDF_Data[, 3], rev(PDF_Data[, 4])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = PDF_Data[, 1], y = PDF_Data[, 5], type = "l", lty = 3, col = PColour)
  polygon(x = c(PDF_Data[, 1], rev(PDF_Data[, 1])), 
          y = c(PDF_Data[, 6], rev(PDF_Data[, 7])), col = alpha(PColour, alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 5.jpeg", width = 826, height = 825, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(2400, 800)
pushViewport(viewport(layout = gly))

# CN non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1:600, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(1.1, 4, 2.9, 1))
hBoot_dat_MHD1 <- MHD.pdf(ht_CN_NW$Height, ht_CN_NW_max$max)
MHD.pdfPlot(hBoot_dat_MHD1, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
hBoot_dat_MHD2 <- MHD.pdf(ht_CN_W$Height, ht_CN_W_max$max)
MHD.pdfPlot(hBoot_dat_MHD2, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
hBoot_dat_MHD3 <- MHD.pdf(ht_CA_NW$Height, ht_CA_NW_max$max)
MHD.pdfPlot(hBoot_dat_MHD3, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_MHD4 <- MHD.pdf(ht_CA_W$Height, ht_CA_W_max$max)
MHD.pdfPlot(hBoot_dat_MHD4, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("A", "B", "C", "D"), x = rep(0.11, 4), y = c(0.92, 0.71, 0.44, 0.23),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_dat_MHD1[, 2], hBoot_dat_MHD1[, 5], alt = "two.sided")
ks.test(hBoot_dat_MHD2[, 2], hBoot_dat_MHD2[, 5], alt = "two.sided")
ks.test(hBoot_dat_MHD3[, 2], hBoot_dat_MHD3[, 5], alt = "two.sided")
ks.test(hBoot_dat_MHD4[, 2], hBoot_dat_MHD4[, 5], alt = "two.sided")





##### Plot dispersal kernels: max height vs height distribution CCDF --------------------------------------

# Set up function to create a single sample and estimate CCDF
WALD.hBootCCDF <- function(heights){
  hList <- sort(WALD.h(100, heights))
  hList.exceed <- function(cutoff){
    return(length(hList[hList >= cutoff])/length(hList))}
  hList.ccdf <- approxfun(hList, sapply(hList, hList.exceed), yleft = 1, yright = 0)
  return(hList.ccdf(seq(0, 60, by = 0.01)))}

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
MHD.CCDF <- function(distHeight, maxHeight){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBootCCDF(distHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBootCCDF(maxHeight), simplify = "matrix"))
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_1, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_1, MARGIN = 1, FUN = quantile, probs = 0.975),
                    apply(X = hBoot_2, MARGIN = 1, FUN = mean),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.025),
                    apply(X = hBoot_2, MARGIN = 1, FUN = quantile, probs = 0.975)))}

# Plot the dispersal percentiles and the corresponding distances
# Error bars indicate the range in which 95% of the bootstrapped simulations are
# Lines indicate mean values
MHD.rtailPlot <- function(rtail_data, LColour, PColour, bNum, bLab){
  if(bNum == FALSE){
    plot(x = rtail_data[, 1], y = rtail_data[, 4], type = "l", col = LColour, xlim = c(0, 60), ylim = c(0, 1),
         xaxt = "n", ylab = "Dispersal Distance (m)")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bNum == TRUE){
    plot(x = rtail_data[, 1], y = rtail_data[, 4], type = "l", col = LColour, xlim = c(0, 60), ylim = c(0, 1),
         xlab = ifelse(bLab == TRUE, "Dispersal Quantile", NA), ylab = "Dispersal Distance (m)")}
  polygon(x = c(rtail_data[, 1], rev(rtail_data[, 1])), 
          y = c(rtail_data[, 5], rev(rtail_data[, 6])), col = alpha(LColour, alpha = 0.2), border = NA)
  lines(x = rtail_data[, 1], y = rtail_data[, 4], type = "l", lty = 3, col = PColour)
  polygon(x = c(rtail_data[, 1], rev(rtail_data[, 1])), 
          y = c(rtail_data[, 5], rev(rtail_data[, 6])), col = alpha(PColour, alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 6.jpeg", width = 826, height = 825, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(2400, 800)
pushViewport(viewport(layout = gly))

# CN non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1:600, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(1.1, 4, 2.9, 1))
hBoot_dat_rtMHD1 <- MHD.CCDF(ht_CN_NW$Height, ht_CN_NW_max$max)
MHD.rtailPlot(hBoot_dat_rtMHD1, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
hBoot_dat_rtMHD2 <- MHD.CCDF(ht_CN_W$Height, ht_CN_W_max$max)
MHD.rtailPlot(hBoot_dat_rtMHD2, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
hBoot_dat_rtMHD3 <- MHD.CCDF(ht_CA_NW$Height, ht_CA_NW_max$max)
MHD.rtailPlot(hBoot_dat_rtMHD3, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_rtMHD4 <- MHD.CCDF(ht_CA_W$Height, ht_CA_W_max$max)
MHD.rtailPlot(hBoot_dat_rtMHD4, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("A", "B", "C", "D"), x = rep(0.11, 4), y = c(0.92, 0.71, 0.44, 0.23),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot dispersal kernels: warmed vs not warmed CCDF ratio ---------------------------------------------

# Function to bootstrap CCDF and estimate 95% bootstrap interval
# For each CCDF sample 100 release heights with 100 seed releases each, and repeat 1000 times
MHD.CCDFRatio <- function(DistH, MaxH){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBootCCDF(DistH), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBootCCDF(MaxH), simplify = "matrix"))
  hBoot_a <- hBoot_2/hBoot_1
  mean.exinf <- function(x){
    return(mean(x[!is.infinite(x)]))}
  quantile.exinf <- function(x, probs){
    return(quantile(x[!is.infinite(x)], probs = probs))}
  return(data.frame(seq(0, 60, by = 0.01),
                    apply(X = hBoot_a, MARGIN = 1, FUN = mean.exinf),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.025),
                    apply(X = hBoot_a, MARGIN = 1, FUN = quantile.exinf, probs = 0.975)))}

# Function to plot bootstrapped CCDF, with mean line and 95% band
MHD.CCDFRatioPlot <- function(rtail_data, bNum, bLab){
  if(bNum == FALSE){
    plot(x = rtail_data[, 1], y = rtail_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xaxt = "n", ylab = "M/HD CCDF Ratio")
    axis(side = 1, at = seq(0, 60, by = 10), labels = FALSE)}
  if(bNum == TRUE){
    plot(x = rtail_data[, 1], y = rtail_data[, 2], type = "l", xlim = c(0, 60), ylim = c(0, 4),
         xlab = ifelse(bLab == TRUE, "Dispersal Distance (m)", NA), ylab = "M/HD CCDF Ratio")}
  polygon(x = c(rtail_data[, 1], rev(rtail_data[, 1])), 
          y = c(rtail_data[, 3], rev(rtail_data[, 4])), col = alpha("black", alpha = 0.2), border = NA)}

# Prepare graphics device
jpeg(filename = "Figure 6.jpeg", width = 826, height = 825, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(2400, 800)
pushViewport(viewport(layout = gly))

# CN non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1:600, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(1.1, 4, 2.9, 1))
hBoot_dat_rtMHD1 <- MHD.CCDFRatio(ht_CN_NW$Height, ht_CN_NW_max$max)
MHD.CCDFRatioPlot(hBoot_dat_rtMHD1, bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
hBoot_dat_rtMHD2 <- MHD.CCDFRatio(ht_CN_W$Height, ht_CN_W_max$max)
MHD.CCDFRatioPlot(hBoot_dat_rtMHD2, bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
hBoot_dat_rtMHD3 <- MHD.CCDFRatio(ht_CA_NW$Height, ht_CA_NW_max$max)
MHD.CCDFRatioPlot(hBoot_dat_rtMHD3, bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_rtMHD4 <- MHD.CCDFRatio(ht_CA_W$Height, ht_CA_W_max$max)
MHD.CCDFRatioPlot(hBoot_dat_rtMHD4, bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("A", "B", "C", "D"), x = rep(0.11, 4), y = c(0.92, 0.71, 0.44, 0.23),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Analyse dispersal kernels: tail-end dispersal distances ---------------------------------------------

# Function to calculate nth percentile dispersal distances, particularly on right tail
# For distribution, sample 100 release heights with 100 seed releases each, and repeat 1000 times
# For mean, simulate 10000 seed release events, and repeat 1000 times
# Either way, each bootstrap has 10000 seed release events and is repeated 1000 times
MHD.rtail <- function(distHeight, maxHeight, Species){
  hBoot_1 <- data.frame(replicate(1000, WALD.hBoot2(distHeight), simplify = "matrix"))
  hBoot_2 <- data.frame(replicate(1000, WALD.hBoot2(maxHeight), simplify = "matrix"))
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
MHD.rtailPlot <- function(rtail_data, LColour, PColour, bNum, bLab){
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

# Prepare graphics device
jpeg(filename = "Figure 6.jpeg", width = 826, height = 825, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(2400, 800)
pushViewport(viewport(layout = gly))

# CN non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1:600, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(1.1, 4, 2.9, 1))
hBoot_dat_rtMHD1 <- MHD.rtail(ht_CN_NW$Height, ht_CN_NW_max$max, "CN")
MHD.rtailPlot(hBoot_dat_rtMHD1, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
hBoot_dat_rtMHD2 <- MHD.rtail(ht_CN_W$Height, ht_CN_W_max$max, "CN")
MHD.rtailPlot(hBoot_dat_rtMHD2, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
hBoot_dat_rtMHD3 <- MHD.rtail(ht_CA_NW$Height, ht_CA_NW_max$max, "CA")
MHD.rtailPlot(hBoot_dat_rtMHD3, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
hBoot_dat_rtMHD4 <- MHD.rtail(ht_CA_W$Height, ht_CA_W_max$max, "CA")
MHD.rtailPlot(hBoot_dat_rtMHD4, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("A", "B", "C", "D"), x = rep(0.11, 4), y = c(0.92, 0.71, 0.44, 0.23),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

