##### Statistical tests on CN height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested plant position within group position within block (row) 
mod_CN_HD <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Row/Group/Plant),
               data = subset(data_ht, Species == "CN"))
summary(mod_CN_HD)

# Perform backward selection using AIC
step(mod_CN_HD)

# Backward selection indicates that the interaction term should be removed
# Random effect is also reduced to (1|Plant:(Group:Row)) + (1|Group:Row)
mod_CN_HD <- lmer(Height ~ TRT + DM_t + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(data_ht, Species == "CN"))
summary(mod_CN_HD)

# Variance appears to be homogeneous
# Plotting/visual inspection is often useful to show this (see Zuur et al. 2010)
plot(mod_CN_HD)

# Residuals seem to be close to normally distributed, though tails are skewed
# Will not affect parameter estimates, but may inflate standard errors and p-values
# P-values are already extremely low, though, so this is likely not a problem
qqnorm(resid(mod_CN_HD))
qqline(resid(mod_CN_HD))

# Shapiro-Wilk test rejects normality of residuals
# However, it is sensitive to slight departures from normal in large samples
# Also note that normality is a one of the weaker assumptions of linear model
# Linear models are pretty robust to violation of normality (see Fitzmaurice 2004, Schielzeth 2020)
shapiro.test(resid(mod_CN_HD))

# Random effects are normally distributed
qqnorm(ranef(mod_CN_HD)$`Plant:(Group:Row)`$`(Intercept)`)
qqline(ranef(mod_CN_HD)$`Plant:(Group:Row)`$`(Intercept)`)
shapiro.test(ranef(mod_CN_HD)$`Plant:(Group:Row)`$`(Intercept)`)
qqnorm(ranef(mod_CN_HD)$`Group:Row`$`(Intercept)`)
qqline(ranef(mod_CN_HD)$`Group:Row`$`(Intercept)`)
shapiro.test(ranef(mod_CN_HD)$`Group:Row`$`(Intercept)`)

# Data within each treatment group are approximately normal
# Again, Shapiro-Wilk test is sensitive to slight departures from normal in large samples
qqnorm(ht_CN_NW$Height)
qqline(ht_CN_NW$Height)
shapiro.test(ht_CN_NW$Height)
qqnorm(ht_CN_W$Height)
qqline(ht_CN_W$Height)
shapiro.test(ht_CN_W$Height)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CN height data: maximum --------------------------------------------------------

# Model maximum flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested group position within block (row) 
mod_CN_M <- lmer(max ~ TRT + DM_t + TRT*DM_t + (1|Row/Group),
               data = subset(data_ht_max, Species == "CN"))
summary(mod_CN_M)

# Perform backward selection using AIC
step(mod_CN_M)

# Backward selection indicates that the interaction term should be removed
# Random effect is also reduced to (1|Group:Row)
mod_CN_M <- lmer(max ~ TRT + DM_t + (1 | Group:Row),
               data = subset(data_ht_max, Species == "CN"))
summary(mod_CN_M)

# Variance appears to be homogeneous
# Plotting/visual inspection is often useful to show this (see Zuur et al. 2010)
plot(mod_CN_M)

# Residuals are approximately normally distributed; Shapiro-Wilk test agrees
qqnorm(resid(mod_CN_M))
qqline(resid(mod_CN_M))
shapiro.test(resid(mod_CN_M))

# Random effects are normally distributed; Shapiro-Wilk test agrees
qqnorm(ranef(mod_CN_M)$`Group:Row`$`(Intercept)`)
qqline(ranef(mod_CN_M)$`Group:Row`$`(Intercept)`)
shapiro.test(ranef(mod_CN_M)$`Group:Row`$`(Intercept)`)

# Data within each treatment group are approximately normal
qqnorm(ht_CN_NW_max$max)
qqline(ht_CN_NW_max$max)
shapiro.test(ht_CN_NW_max$max)
qqnorm(ht_CN_W_max$max)
qqline(ht_CN_W_max$max)
shapiro.test(ht_CN_W_max$max)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CA height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested plant position within group position within block (row)
mod_CA_HD <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Row/Group/Plant),
               data = subset(na.omit(data_ht), Species == "CA"))
summary(mod_CA_HD)

# Row random effect causes convergence issues and is almost zero; remove it
mod_CA_HD <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(na.omit(data_ht), Species == "CA"))
summary(mod_CA_HD)

# Perform backward selection using AIC
step(mod_CA_HD)

# Backward selection indicates that the interaction and DM_t terms should be removed
mod_CA_HD <- lmer(Height ~ TRT + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(na.omit(data_ht), Species == "CA"))
summary(mod_CA_HD)

# Variance appears to be homogeneous
# Plotting/visual inspection is often useful to show this (see Zuur et al. 2010)
plot(mod_CA_HD)

# Residuals seem to be close to normally distributed, though tails are skewed
# Will not affect parameter estimates, but may inflate standard errors and p-values
# P-values are already extremely low, though, so this is likely not a problem
qqnorm(resid(mod_CA_HD))
qqline(resid(mod_CA_HD))

# Shapiro-Wilk test rejects normality of residuals
# However, it is sensitive to slight departures from normal in large samples
# Also note that normality is a one of the weaker assumptions of linear model
# Linear models are pretty robust to violation of normality (see Fitzmaurice 2004, Schielzeth 2020)
shapiro.test(resid(mod_CA_HD))

# Random effects are approximately normally distributed
qqnorm(ranef(mod_CA_HD)$`Plant:(Group:Row)`$`(Intercept)`)
qqline(ranef(mod_CA_HD)$`Plant:(Group:Row)`$`(Intercept)`)
shapiro.test(ranef(mod_CA_HD)$`Plant:(Group:Row)`$`(Intercept)`)
qqnorm(ranef(mod_CA_HD)$`Group:Row`$`(Intercept)`)
qqline(ranef(mod_CA_HD)$`Group:Row`$`(Intercept)`)
shapiro.test(ranef(mod_CA_HD)$`Group:Row`$`(Intercept)`)

# Data within each treatment group are approximately normal
# Again, Shapiro-Wilk test is sensitive to slight departures from normal in large samples
qqnorm(ht_CA_NW$Height)
qqline(ht_CA_NW$Height)
shapiro.test(ht_CA_NW$Height)
qqnorm(ht_CA_W$Height)
qqline(ht_CA_W$Height)
shapiro.test(ht_CA_W$Height)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CA height data: maximum --------------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested group position within block (row) 
mod_CA_M <- lmer(max ~ TRT + DM_t + TRT*DM_t + (1|Row/Group),
               data = subset(na.omit(data_ht_max), Species == "CA"))
summary(mod_CA_M)

# Perform backward selection using AIC
step(mod_CA_M)

# Backward selection indicates that the interaction and DM_t terms should be removed
# Random effect is also reduced to (1|Group:Row)
mod_CA_M <- lmer(max ~ TRT + (1|Group:Row),
               data = subset(na.omit(data_ht_max), Species == "CA"))
summary(mod_CA_M)

# Variance appears to be homogeneous
# Plotting/visual inspection is often useful to show this (see Zuur et al. 2010)
plot(mod_CA_M)

# Residuals are approximately normally distributed; Shapiro-Wilk test agrees
qqnorm(resid(mod_CA_M))
qqline(resid(mod_CA_M))
shapiro.test(resid(mod_CA_M))

# Random effects are normally distributed; Shapiro-Wilk test agrees
qqnorm(ranef(mod_CA_M)$`Group:Row`$`(Intercept)`)
qqline(ranef(mod_CA_M)$`Group:Row`$`(Intercept)`)
shapiro.test(ranef(mod_CA_M)$`Group:Row`$`(Intercept)`)

# Data within each treatment group are approximately normal
qqnorm(ht_CA_NW_max$max)
qqline(ht_CA_NW_max$max)
shapiro.test(ht_CA_NW_max$max)
qqnorm(ht_CA_W_max$max)
qqline(ht_CA_W_max$max)
shapiro.test(ht_CA_W_max$max)

# There are no egregious violations of the LME model assumptions





##### [F2] Estimate height distribution PDF with 95% bootstrap interval -----------------------------------

# Set seed for RNG, then bootstrap PDFs for NW/W CN and NW/W CA heights
set.seed(19854)
hBoot_HD1 <- ht.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_HD2 <- ht.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height)

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W height distributions display significant difference for CN and CA
set.seed(19854)
ht.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, ks.only = TRUE)
ht.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, ks.only = TRUE)





##### [F3.1] Estimate warmed vs not warmed PDF (dispersal kernels) ----------------------------------------

# Set seed for RNG, then bootstrap PDFs for NW/W CN and NW/W CA dispersal kernels
set.seed(18364)
hBoot_WNW1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")

# Set seed for RNG, then find Mean dispersal distance and 95% bootstrap interval on mean
set.seed(18364)
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
set.seed(18364)
ds.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA", ks.only = TRUE)





##### [F3.2] Estimate warmed vs not warmed CCDF -----------------------------------------------------------

# Set seed for RNG, then bootstrap CCDFs for NW/W CN and NW/W CA heights
set.seed(62947)
hBoot_WNW1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")





##### [F3.3] Estimate warmed vs not warmed CCDF ratios ----------------------------------------------------

# Set seed for RNG, then bootstrap CCDF ratios for NW/W CN and NW/W CA heights
set.seed(56284)
hBoot_WNW1_CCDFRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_CCDFRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")

# Ratios at 10 m for CN and CA
hBoot_WNW1_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 10, ]
hBoot_WNW2_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 10, ]

# Ratios at 50 m for CN and CA
hBoot_WNW1_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]
hBoot_WNW2_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]





##### [F3.4] Estimate warmed vs not right tail dispersal percentile distances -----------------------------

# First, set seed for RNG
# Then bootstrap right tail dispersal percentile distances for NW/W CN and NW/W CA heights
set.seed(70472)
hboot_WNW1_rtail <- ds.rtailBootWNW(ht_CN_NW$Height, ht_CN_W$Height, "CN")
hboot_WNW2_rtail <- ds.rtailBootWNW(ht_CA_NW$Height, ht_CA_W$Height, "CA")

# Mean 95th and 99th percentiles for CN
subset(hboot_WNW1_rtail, DistancePercentile == 0.95 & Treatment == "Not Warmed")
subset(hboot_WNW1_rtail, DistancePercentile == 0.95 & Treatment == "Warmed")
subset(hboot_WNW1_rtail, DistancePercentile == 0.99 & Treatment == "Not Warmed")
subset(hboot_WNW1_rtail, DistancePercentile == 0.99 & Treatment == "Warmed")

# Mean 95th and 99th percentiles for CA
subset(hboot_WNW2_rtail, DistancePercentile == 0.95 & Treatment == "Not Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.95 & Treatment == "Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.99 & Treatment == "Not Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.99 & Treatment == "Warmed")





##### [F4.1] Estimate max height vs height distribution PDF (dispersal kernels) ---------------------------

# Set seed for RNG, then bootstrap PDFs for M/HD CN and M/HD CA dispersal kernels
set.seed(28472)
hBoot_MHD1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_NW_max$max, "MHD", "CN")
hBoot_MHD2_pdf <- ds.pdfBoot(ht_CN_W$Height, ht_CN_W_max$max, "MHD", "CN")
hBoot_MHD3_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_NW_max$max, "MHD", "CA")
hBoot_MHD4_pdf <- ds.pdfBoot(ht_CA_W$Height, ht_CA_W_max$max, "MHD", "CA")

# Set seed for RNG, then find Mean dispersal distance and 95% bootstrap interval on mean
set.seed(28472)
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")
ds.mean(ht_CN_NW_max$max, "CN")
ds.mean(ht_CN_W_max$max, "CN")
ds.mean(ht_CA_NW_max$max, "CA")
ds.mean(ht_CA_W_max$max, "CA")

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
set.seed(28472)
ds.pdfBoot(ht_CN_NW$Height, ht_CN_NW_max$max, "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CN_W$Height, ht_CN_W_max$max, "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CA_NW$Height, ht_CA_NW_max$max, "MHD", "CA", ks.only = TRUE)
ds.pdfBoot(ht_CA_W$Height, ht_CA_W_max$max, "MHD", "CA", ks.only = TRUE)





##### [F4.2] Estimate max height vs height distribution CCDF ----------------------------------------------

# Set seed for RNG, then bootstrap CCDFs for M/HD CN and M/HD CA
set.seed(93748)
hBoot_MHD1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_NW_max$max, "MHD", "CN")
hBoot_MHD2_ccdf <- ds.ccdfBoot(ht_CN_W$Height, ht_CN_W_max$max, "MHD", "CN")
hBoot_MHD3_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_NW_max$max, "MHD", "CA")
hBoot_MHD4_ccdf <- ds.ccdfBoot(ht_CA_W$Height, ht_CA_W_max$max, "MHD", "CA")





##### [F4.3] Plot max height vs height distribution CCDF ratio --------------------------------------------

# Set seed for RNG, then bootstrap CCDF ratios for M/HD CN and M/HD CA dispersal kernels
set.seed(44492)
hBoot_MHD1_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_W$Height, ht_CN_W_max$max, "MHD", "CN")
hBoot_MHD2_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_NW_max$max, "MHD", "CN")
hBoot_MHD3_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_W$Height, ht_CA_W_max$max, "MHD", "CA")
hBoot_MHD4_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_NW_max$max, "MHD", "CA")

# Ratios at 10 m for CN and CA
hBoot_MHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 10, ]
hBoot_MHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 10, ]
hBoot_MHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 10, ]
hBoot_MHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 10, ]

# Ratios at 50 m for CN and CA
hBoot_MHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 50, ]
hBoot_MHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 50, ]
hBoot_MHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 50, ]
hBoot_MHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 50, ]





##### [F4.4] Estimate max height vs height distribution right tail dispersal percentile distances ---------

# First, set seed for RNG
# Then bootstrap right tail dispersal percentile distances for M/HD CN and M/HD CA heights
set.seed(91748)
hBoot_MHD1_rtail <- ds.rtailBootMHD(ht_CN_NW$Height, ht_CN_NW_max$max, "CN")
hBoot_MHD2_rtail <- ds.rtailBootMHD(ht_CN_W$Height, ht_CN_W_max$max, "CN")
hBoot_MHD3_rtail <- ds.rtailBootMHD(ht_CA_NW$Height, ht_CA_NW_max$max, "CA")
hBoot_MHD4_rtail <- ds.rtailBootMHD(ht_CA_W$Height, ht_CA_W_max$max, "CA")

# Mean 95th and 99th percentiles for CN unwarmed
subset(hBoot_MHD1_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD1_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD1_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD1_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

# Mean 95th and 99th percentiles for CN warmed
subset(hBoot_MHD2_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

# Mean 95th and 99th percentiles for CA unwarmed
subset(hBoot_MHD3_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

# Mean 95th and 99th percentiles for CA warmed
subset(hBoot_MHD4_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

