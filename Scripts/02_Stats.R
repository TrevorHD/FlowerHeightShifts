##### Statistical tests on CN height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is block (Group)
mod_CN_HD <- lmer(Height_PA ~ TRT + scale(DM_t_PA) + TRT*scale(DM_t_PA) + (1|Group),
                  data = subset(data_ht_PA, Species == "CN"))
summary(mod_CN_HD)

# Perform backward selection using AIC
step(mod_CN_HD)

# Backward selection indicates that the interaction term and covariate should be removed
mod_CN_HD <- lmer(Height_PA ~ TRT + (1|Group),
                  data = subset(data_ht_PA, Species == "CN"))
summary(mod_CN_HD)

# Variance appears to be homogeneous
plot(mod_CN_HD)

# Residuals seem to be normally distributed
# Shapiro-Wilk test fails to reject normality of residuals
qqnorm(resid(mod_CN_HD))
qqline(resid(mod_CN_HD))
shapiro.test(resid(mod_CN_HD))

# Data within each treatment group are approximately normal
# Shapiro-Wilk test fails to reject normality of data
qqnorm(subset(data_ht_PA, Species == "CN" & TRT == "NW")$Height)
qqline(subset(data_ht_PA, Species == "CN" & TRT == "NW")$Height)
shapiro.test(subset(data_ht_PA, Species == "CN" & TRT == "NW")$Height)
qqnorm(subset(data_ht_PA, Species == "CN" & TRT == "W")$Height)
qqline(subset(data_ht_PA, Species == "CN" & TRT == "W")$Height)
shapiro.test(subset(data_ht_PA, Species == "CN" & TRT == "W")$Height)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CN height data: maximum --------------------------------------------------------

# Model maximum flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is block (Group)
mod_CN_M <- lmer(Max_PA ~ TRT + scale(DM_t_PA) + TRT*scale(DM_t_PA) + (1|Group),
                 data = subset(data_ht_max_PA, Species == "CN"))
summary(mod_CN_M)

# Perform backward selection using AIC
step(mod_CN_M)

# Backward selection indicates that the interaction term should be removed
mod_CN_M <- lmer(Max_PA ~ TRT + scale(DM_t_PA) + (1|Group),
                 data = subset(data_ht_max_PA, Species == "CN"))
summary(mod_CN_M)

# Variance appears to be homogeneous
plot(mod_CN_M)

# Residuals seem to be normally distributed
# Shapiro-Wilk test fails to reject normality of residuals
qqnorm(resid(mod_CN_M))
qqline(resid(mod_CN_M))
shapiro.test(resid(mod_CN_M))

# Data within each treatment group are approximately normal
# Shapiro-Wilk test fails to reject normality of data
qqnorm(subset(data_ht_max_PA, Species == "CN" & TRT == "NW")$Max_PA)
qqline(subset(data_ht_max_PA, Species == "CN" & TRT == "NW")$Max_PA)
shapiro.test(subset(data_ht_max_PA, Species == "CN" & TRT == "NW")$Max_PA)
qqnorm(subset(data_ht_max_PA, Species == "CN" & TRT == "W")$Max_PA)
qqline(subset(data_ht_max_PA, Species == "CN" & TRT == "W")$Max_PA)
shapiro.test(subset(data_ht_max_PA, Species == "CN" & TRT == "W")$Max_PA)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CA height data: distribution ---------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is block (Group)
mod_CA_HD <- lmer(Height_PA ~ TRT + scale(DM_t_PA) + TRT*scale(DM_t_PA) + (1|Group),
                  data = subset(data_ht_PA, Species == "CA"))
summary(mod_CA_HD)

# Perform backward selection using AIC
step(mod_CA_HD)

# Backward selection indicates that the interaction term and covariate should be removed
mod_CA_HD <- lmer(Height_PA ~ TRT + (1|Group),
                  data = subset(data_ht_PA, Species == "CA"))
summary(mod_CA_HD)

# Variance appears to be homogeneous
plot(mod_CA_HD)

# Residuals seem to be normally distributed, though tails are a bit off
# Shapiro-Wilk test fails to reject normality of residuals
qqnorm(resid(mod_CA_HD))
qqline(resid(mod_CA_HD))
shapiro.test(resid(mod_CA_HD))

# Data within each treatment group are approximately normal
# Shapiro-Wilk test fails to reject normality of data
qqnorm(subset(data_ht_PA, Species == "CA" & TRT == "NW")$Height)
qqline(subset(data_ht_PA, Species == "CA" & TRT == "NW")$Height)
shapiro.test(subset(data_ht_PA, Species == "CA" & TRT == "NW")$Height)
qqnorm(subset(data_ht_PA, Species == "CA" & TRT == "W")$Height)
qqline(subset(data_ht_PA, Species == "CA" & TRT == "W")$Height)
shapiro.test(subset(data_ht_PA, Species == "CA" & TRT == "W")$Height)

# There are no egregious violations of the LME model assumptions





##### Statistical tests on CA height data: maximum --------------------------------------------------------

# Model population-level distribution of flower head heights

# Examine effect of treatment using LME model
# Fixed effect is treatment (TRT) with post-transplant diameter (DM_t) as covariate
# Random effect is block (Group)
mod_CA_M <- lmer(Max_PA ~ TRT + scale(DM_t_PA) + TRT*scale(DM_t_PA) + (1|Group),
                 data = subset(data_ht_max_PA, Species == "CA"))
summary(mod_CA_M)

# Perform backward selection using AIC
step(mod_CA_M)

# Backward selection indicates that the interaction term and covariate should be removed
mod_CA_M <- lmer(Max_PA ~ TRT + (1|Group),
                 data = subset(data_ht_max_PA, Species == "CA"))
summary(mod_CA_M)

# Variance appears to be homogeneous
plot(mod_CA_M)

# Residuals seem to be normally distributed
# Shapiro-Wilk test fails to reject normality of residuals
qqnorm(resid(mod_CA_M))
qqline(resid(mod_CA_M))
shapiro.test(resid(mod_CA_M))

# Data within each treatment group are approximately normal
# Shapiro-Wilk test fails to reject normality of data
qqnorm(subset(data_ht_max_PA, Species == "CA" & TRT == "NW")$Max_PA)
qqline(subset(data_ht_max_PA, Species == "CA" & TRT == "NW")$Max_PA)
shapiro.test(subset(data_ht_max_PA, Species == "CA" & TRT == "NW")$Max_PA)
qqnorm(subset(data_ht_max_PA, Species == "CA" & TRT == "W")$Max_PA)
qqline(subset(data_ht_max_PA, Species == "CA" & TRT == "W")$Max_PA)
shapiro.test(subset(data_ht_max_PA, Species == "CA" & TRT == "W")$Max_PA)

# There are no egregious violations of the LME model assumptions





##### [F1] Estimate overall height distribution PDF with 95% bootstrap interval ---------------------------

# Set seed for RNG, then bootstrap PDFs for NW/W CN and NW/W CA heights
set.seed(19854)
hBoot_HD1 <- ht.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_HD2 <- ht.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height)

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W height distributions display significant difference for CN and CA
set.seed(19854)
ht.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, ks.only = TRUE)
ht.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, ks.only = TRUE)

# Note: not bootstrapping the distributions gives nearly identical test results
ks.test(ht_CN_NW$Height, ht_CN_W$Height)
ks.test(ht_CA_NW$Height, ht_CA_W$Height)





##### [F2] Estimate warmed vs not warmed PDF (dispersal kernels) ------------------------------------------

# Set seed for RNG, then bootstrap PDFs for NW/W CN and NW/W CA dispersal kernels
set.seed(19854)
hBoot_WNW1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
set.seed(19854)
ds.pdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA", ks.only = TRUE)





##### [F3] Estimate warmed vs not warmed CCDF ratios ------------------------------------------------------

# Set seed for RNG, then bootstrap CCDF ratios for NW/W CN and NW/W CA heights
set.seed(19854)
hBoot_WNW1_CCDFRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_CCDFRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")





##### [F4] Estimate max height point source vs overall height distribution PDF (dispersal kernels) --------

# Set seed for RNG, then bootstrap PDFs for M/HD CN and M/HD CA dispersal kernels
set.seed(19854)
hBoot_MPHD1_pdf <- ds.pdfBoot(ht_CN_NW$Height, rep(mean(ht_CN_W_max$Max), 100), "MHD", "CN")
hBoot_MPHD2_pdf <- ds.pdfBoot(ht_CN_W$Height, rep(mean(ht_CN_W_max$Max), 100), "MHD", "CN")
hBoot_MPHD3_pdf <- ds.pdfBoot(ht_CA_NW$Height, rep(mean(ht_CA_NW_max$Max), 100), "MHD", "CA")
hBoot_MPHD4_pdf <- ds.pdfBoot(ht_CA_W$Height, rep(mean(ht_CA_W_max$Max), 100), "MHD", "CA")

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
set.seed(19854)
ds.pdfBoot(ht_CN_NW$Height, rep(mean(ht_CN_W_max$Max), 100), "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CN_W$Height, rep(mean(ht_CN_W_max$Max), 100), "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CA_NW$Height, rep(mean(ht_CA_NW_max$Max), 100), "MHD", "CA", ks.only = TRUE)
ds.pdfBoot(ht_CA_W$Height, rep(mean(ht_CA_W_max$Max), 100), "MHD", "CA", ks.only = TRUE)





##### [F5] Estimate max height point source vs overall height distribution CCDF ratio ---------------------

# Set seed for RNG, then bootstrap CCDF ratios for M/HD CN and M/HD CA dispersal kernels
set.seed(19854)
hBoot_MPHD1_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_W$Height, rep(mean(ht_CN_W_max$Max), 100), "MHD", "CN")
hBoot_MPHD2_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, rep(mean(ht_CN_NW_max$Max), 100), "MHD", "CN")
hBoot_MPHD3_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_W$Height, rep(mean(ht_CA_W_max$Max), 100), "MHD", "CA")
hBoot_MPHD4_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, rep(mean(ht_CA_NW_max$Max), 100), "MHD", "CA")





##### [FS2] Estimate max height distribution vs overall height distribution PDF (dispersal kernels) -------

# Set seed for RNG, then bootstrap PDFs for M/HD CN and M/HD CA dispersal kernels
set.seed(19854)
hBoot_MHD1_pdf <- ds.pdfBoot(ht_CN_NW$Height, ht_CN_NW_max$Max, "MHD", "CN")
hBoot_MHD2_pdf <- ds.pdfBoot(ht_CN_W$Height, ht_CN_W_max$Max, "MHD", "CN")
hBoot_MHD3_pdf <- ds.pdfBoot(ht_CA_NW$Height, ht_CA_NW_max$Max, "MHD", "CA")
hBoot_MHD4_pdf <- ds.pdfBoot(ht_CA_W$Height, ht_CA_W_max$Max, "MHD", "CA")

# Set seed for RNG, then conduct Kolmogorov-Smirnov test for NW/W CN and NW/W CA
# The NW and W dispersal kernels display significant difference for CN and CA
set.seed(19854)
ds.pdfBoot(ht_CN_NW$Height, ht_CN_NW_max$Max, "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CN_W$Height, ht_CN_W_max$Max, "MHD", "CN", ks.only = TRUE)
ds.pdfBoot(ht_CA_NW$Height, ht_CA_NW_max$Max, "MHD", "CA", ks.only = TRUE)
ds.pdfBoot(ht_CA_W$Height, ht_CA_W_max$Max, "MHD", "CA", ks.only = TRUE)





##### [FS3] Estimate max height distribution vs overall height distribution CCDF ratio --------------------

# Set seed for RNG, then bootstrap CCDF ratios for M/HD CN and M/HD CA dispersal kernels
set.seed(19854)
hBoot_MHD1_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_W$Height, ht_CN_W_max$Max, "MHD", "CN")
hBoot_MHD2_ccdfRatio <- ds.ccdfRatioBoot(ht_CN_NW$Height, ht_CN_NW_max$Max, "MHD", "CN")
hBoot_MHD3_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_W$Height, ht_CA_W_max$Max, "MHD", "CA")
hBoot_MHD4_ccdfRatio <- ds.ccdfRatioBoot(ht_CA_NW$Height, ht_CA_NW_max$Max, "MHD", "CA")





##### [TS1] Select quantities for warmed vs unwarmed treatments --------------------------------------------

# Mean dispersal distances with 95% BI
set.seed(19854)
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")

# Bootstrap right tail dispersal percentile distances for NW/W CN and NW/W CA heights
set.seed(19854)
hboot_WNW1_rtail <- ds.rtailBootWNW(ht_CN_NW$Height, ht_CN_W$Height, "CN")
hboot_WNW2_rtail <- ds.rtailBootWNW(ht_CA_NW$Height, ht_CA_W$Height, "CA")

# 95th percentile dispersal distances with 95% BI
subset(hboot_WNW1_rtail, DistancePercentile == 0.95 & Treatment == "Not Warmed")
subset(hboot_WNW1_rtail, DistancePercentile == 0.95 & Treatment == "Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.95 & Treatment == "Not Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.95 & Treatment == "Warmed")

# 99th percentile dispersal distances with 95% BI
subset(hboot_WNW1_rtail, DistancePercentile == 0.99 & Treatment == "Not Warmed")
subset(hboot_WNW1_rtail, DistancePercentile == 0.99 & Treatment == "Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.99 & Treatment == "Not Warmed")
subset(hboot_WNW2_rtail, DistancePercentile == 0.99 & Treatment == "Warmed")

# 10m warmed/unwarmed risk ratio with 95% BI
hBoot_WNW1_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 10, ]
hBoot_WNW2_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 10, ]

# 50m warmed/unwarmed risk ratio with 95% BI
hBoot_WNW1_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]
hBoot_WNW2_CCDFRatio[hBoot_WNW1_CCDFRatio[, 1] == 50, ]





##### [TS2] Select quantities for max height point source vs overall height distribution ------------------

# Mean dispersal distances with 95% BI
set.seed(19854)
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(rep(mean(ht_CN_NW_max$Max), 100), "CN")
ds.mean(rep(mean(ht_CN_W_max$Max), 100), "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")
ds.mean(rep(mean(ht_CA_NW_max$Max), 100), "CA")
ds.mean(rep(mean(ht_CA_W_max$Max), 100), "CA")

# Bootstrap right tail dispersal percentile distances for M/HD CN and M/HD CA heights
set.seed(19854)
hBoot_MPHD1_rtail <- ds.rtailBootMHD(ht_CN_NW$Height, rep(mean(ht_CN_NW_max$Max), 100), "CN")
hBoot_MPHD2_rtail <- ds.rtailBootMHD(ht_CN_W$Height, rep(mean(ht_CN_W_max$Max), 100), "CN")
hBoot_MPHD3_rtail <- ds.rtailBootMHD(ht_CA_NW$Height, rep(mean(ht_CA_NW_max$Max), 100), "CA")
hBoot_MPHD4_rtail <- ds.rtailBootMHD(ht_CA_W$Height, rep(mean(ht_CA_W_max$Max), 100), "CA")

# 95th percentile dispersal distances with 95% BI
subset(hBoot_MPHD1_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MPHD2_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MPHD1_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MPHD2_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MPHD3_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MPHD4_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MPHD3_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MPHD4_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")

# 99th percentile dispersal distances with 95% BI
subset(hBoot_MPHD1_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MPHD2_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MPHD1_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MPHD2_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MPHD3_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MPHD4_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MPHD3_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MPHD4_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

# 10m maximum/distribution risk ratio with 95% BI
hBoot_MPHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 10, ]
hBoot_MPHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 10, ]
hBoot_MPHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 10, ]
hBoot_MPHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 10, ]

# 50m maximum/distribution risk ratio with 95% BI
hBoot_MPHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 50, ]
hBoot_MPHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 50, ]
hBoot_MPHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 50, ]
hBoot_MPHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 50, ]





##### [TS3] Select quantities for max height distribution vs overall height distribution ------------------

# Mean dispersal distances with 95% BI
set.seed(19854)
ds.mean(ht_CN_NW$Height, "CN")
ds.mean(ht_CN_W$Height, "CN")
ds.mean(ht_CN_NW_max$Max, "CN")
ds.mean(ht_CN_W_max$Max, "CN")
ds.mean(ht_CA_NW$Height, "CA")
ds.mean(ht_CA_W$Height, "CA")
ds.mean(ht_CA_NW_max$Max, "CA")
ds.mean(ht_CA_W_max$Max, "CA")

# Bootstrap right tail dispersal percentile distances for M/HD CN and M/HD CA heights
set.seed(19854)
hBoot_MHD1_rtail <- ds.rtailBootMHD(ht_CN_NW$Height, ht_CN_NW_max$Max, "CN")
hBoot_MHD2_rtail <- ds.rtailBootMHD(ht_CN_W$Height, ht_CN_W_max$Max, "CN")
hBoot_MHD3_rtail <- ds.rtailBootMHD(ht_CA_NW$Height, ht_CA_NW_max$Max, "CA")
hBoot_MHD4_rtail <- ds.rtailBootMHD(ht_CA_W$Height, ht_CA_W_max$Max, "CA")

# 95th percentile dispersal distances with 95% BI
subset(hBoot_MHD1_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD1_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.95 & Heights == "Dist. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.95 & Heights == "Max. Height")

# 99th percentile dispersal distances with 95% BI
subset(hBoot_MHD1_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD1_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MHD2_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.99 & Heights == "Dist. Height")
subset(hBoot_MHD3_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")
subset(hBoot_MHD4_rtail, DistancePercentile == 0.99 & Heights == "Max. Height")

# 10m maximum/distribution risk ratio with 95% BI
hBoot_MHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 10, ]
hBoot_MHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 10, ]
hBoot_MHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 10, ]
hBoot_MHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 10, ]

# 50m maximum/distribution risk ratio with 95% BI
hBoot_MHD1_ccdfRatio[hBoot_MHD1_ccdfRatio[, 1] == 50, ]
hBoot_MHD2_ccdfRatio[hBoot_MHD2_ccdfRatio[, 1] == 50, ]
hBoot_MHD3_ccdfRatio[hBoot_MHD3_ccdfRatio[, 1] == 50, ]
hBoot_MHD4_ccdfRatio[hBoot_MHD4_ccdfRatio[, 1] == 50, ]

