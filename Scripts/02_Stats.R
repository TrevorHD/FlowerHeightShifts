##### Statistical tests on CN height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested plant position within group position within block (row) 
mod_CN <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Row/Group/Plant),
               data = subset(data_ht, Species == "CN"))
summary(mod_CN)

# Perform backward selection using AIC
step(mod_CN)

# Backward selection indicates that the interaction term should be removed
# Random effect is also reduced to (1|Plant:(Group:Row)) + (1|Group:Row)
mod_CN <- lmer(Height ~ TRT + DM_t + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(data_ht, Species == "CN"))
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





##### Statistical tests on CN height data: maximum --------------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested group position within block (row) 
mod_CN <- lmer(max ~ TRT + DM_t + TRT*DM_t + (1|Row/Group),
               data = subset(data_ht_max, Species == "CN"))
summary(mod_CN)

# Perform backward selection using AIC
step(mod_CN)

# Backward selection indicates that the interaction term should be removed
# Random effect is also reduced to (1|Group:Row)
mod_CN <- lmer(max ~ TRT + DM_t + (1 | Group:Row),
               data = subset(data_ht_max, Species == "CN"))
summary(mod_CN)





##### Statistical tests on CA height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested plant position within group position within block (row)
mod_CA <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Row/Group/Plant),
               data = subset(na.omit(data_ht), Species == "CA"))
summary(mod_CA)

# Row random effect causes convergence issues and is almost zero; remove it
mod_CA <- lmer(Height ~ TRT + DM_t + TRT*DM_t + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(na.omit(data_ht), Species == "CA"))
summary(mod_CA)

# Perform backward selection using AIC
step(mod_CA)

# Backward selection indicates that the interaction and DM_t terms should be removed
mod_CA <- lmer(Height ~ TRT + (1|Plant:(Group:Row)) + (1|Group:Row),
               data = subset(na.omit(data_ht), Species == "CA"))
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





##### Statistical tests on CA height data: maximum --------------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is nested group position within block (row) 
mod_CA <- lmer(max ~ TRT + DM_t + TRT*DM_t + (1|Row/Group),
               data = subset(na.omit(data_ht_max), Species == "CA"))
summary(mod_CA)

# Perform backward selection using AIC
step(mod_CA)

# Backward selection indicates that the interaction and DM_t terms should be removed
# Random effect is also reduced to (1|Group:Row)
mod_CA <- lmer(max ~ TRT + (1|Group:Row),
               data = subset(na.omit(data_ht_max), Species == "CA"))
summary(mod_CA)





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

