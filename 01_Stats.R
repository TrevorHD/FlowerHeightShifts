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
# We are also interested in only warmed (W) and not warmed (NW)

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





##### Set up dispersal framework --------------------------------------------------------------------------

# Create PDF of wind speeds
# Assume no seed release occurs for wind speeds of zero, so remove zero values
ws_values <- c(data_ws1$Wind1, data_ws2$Wind1)
ws_values <- ws_values[ws_values > 0]
ws_pdf <- density(ws_values, from = min(ws_values), to = max(ws_values), bw = 0.05)
ws_mean <- mean(ws_values)

# Create PDF of wind speeds
# Terminal velocity is drop tube length divided by drop time
tv_values <- na.omit(1.25/data_tv$DT.Avg)
tv_pdf <- density(tv_values, from = min(tv_values), to = max(tv_values), bw = 0.05)
tv_mean <- mean(tv_values)

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
  Um <- rnorm(n, sample(ws_values, size = n, replace = TRUE), ws_pdf$bw)
  
  # Simulate terminal velocities from empirical distribution of terminal velocities
  f <- rnorm(n, sample(tv_values, size = n, replace = TRUE), tv_pdf$bw)
  
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

