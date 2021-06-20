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





##### [F2] Estimate height distribution PDF with 95% bootstrap interval -----------------------------------

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W height distributions display significant difference for CN and CA
ks.test(hBoot_HD1[, 2], hBoot_HD1[, 5], alt = "two.sided")
ks.test(hBoot_HD2[, 2], hBoot_HD2[, 5], alt = "two.sided")





##### [F3.1] Estimate warmed vs not warmed PDF (dispersal kernels) ----------------------------------------

# Set seed for (pseudo) RNG
set.seed(18364)

# Mean dispersal distance for kernel, along with 95% bootstrap interval on mean
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_WNW1_pdf[, 2], hBoot_WNW1_pdf[, 5], alt = "two.sided")
ks.test(hBoot_WNW2_pdf[, 2], hBoot_WNW2_pdf[, 5], alt = "two.sided")





##### [F3.3] Estimate warmed vs not warmed CCDF ratios ----------------------------------------------------

# Set seed for (pseudo) RNG
set.seed(56284)

# Ratios at 50 m for CN and CA
hBoot_WNW1_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]
hBoot_WNW2_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]





##### [F3.4] Estimate warmed vs not right tail dispersal percentile distances -----------------------------

# Set seed for (pseudo) RNG
set.seed(70472)

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

# Set seed for (pseudo) RNG
set.seed(28472)

# Mean dispersal distance for kernel, along with 95% bootstrap interval on mean
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")
ds.mean(ht_CN_NW_max$max, "CN")
ds.mean(ht_CN_W_max$max, "CN")
ds.mean(ht_CA_NW_max$max, "CA")
ds.mean(ht_CA_W_max$max, "CA")

# Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
ks.test(hBoot_MHD1_pdf[, 2], hBoot_MHD1_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD2_pdf[, 2], hBoot_MHD2_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD3_pdf[, 2], hBoot_MHD3_pdf[, 5], alt = "two.sided")
ks.test(hBoot_MHD4_pdf[, 2], hBoot_MHD4_pdf[, 5], alt = "two.sided")





##### [F4.3] Plot max height vs height distribution CCDF ratio --------------------------------------------

# Set seed for (pseudo) RNG
set.seed(44492)

# Ratios at 50 m for CN and CA
hBoot_MHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 50, ]
hBoot_MHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 50, ]
hBoot_MHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 50, ]
hBoot_MHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 50, ]





##### [F4.4] Estimate max height vs height distribution right tail dispersal percentile distances ---------

# Set seed for (pseudo) RNG
set.seed(91748)

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

