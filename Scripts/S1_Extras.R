##### [Unused] Plot mean heights for each treatment/species combination -----------------------------------

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
jpeg(filename = "Figure U1.jpeg", width = 826, height = 568, units = "px")

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





##### [Unused] Plot warmed vs not warmed CCDF -------------------------------------------------------------

# Set seed for RNG, then bootstrap CCDFs for NW/W CN and NW/W CA heights
set.seed(62947)
hBoot_WNW1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_W$Height, "WNW", "CN")
hBoot_WNW2_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_W$Height, "WNW", "CA")

# Prepare graphics device
tiff(filename = "Figure U2.tif", width = 3304, height = 2272, units = "px",
     res = 800, compression = "lzw")

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
ds.ccdfBoot.plot(hBoot_WNW1_ccdf, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfBoot.plot(hBoot_WNW2_ccdf, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Not warmed", "Warmed"), x = rep(0.834, 2), y = c(0.918, 0.885),
          hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.806, 2), y0 = c(0.918, 0.885), x1 = rep(0.826, 2), y1 = c(0.918, 0.885),
              gp = gpar(col = c("black", "red")))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Unused] Plot warmed vs not right tail dispersal percentile distances -------------------------------

# Prepare graphics device
tiff(filename = "Figure U3.tif", width = 3304, height = 2272, units = "px",
     res = 800, compression = "lzw")

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
ds.rtailBootWNW.plot(hboot_WNW1_rtail, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.rtailBootWNW.plot(hboot_WNW2_rtail, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Not warmed", "Warmed"), x = rep(0.834, 2), y = c(0.635, 0.602),
          hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.806, 2), y0 = c(0.635, 0.602), x1 = rep(0.826, 2), y1 = c(0.635, 0.602),
              gp = gpar(col = c("black", "red")))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Unused] Plot max height vs height distribution CCDF ------------------------------------------------

# Set seed for RNG, then bootstrap CCDFs for M/HD CN and M/HD CA
set.seed(93748)
hBoot_MHD1_ccdf <- ds.ccdfBoot(ht_CN_NW$Height, ht_CN_NW_max$Max, "MHD", "CN")
hBoot_MHD2_ccdf <- ds.ccdfBoot(ht_CN_W$Height, ht_CN_W_max$Max, "MHD", "CN")
hBoot_MHD3_ccdf <- ds.ccdfBoot(ht_CA_NW$Height, ht_CA_NW_max$Max, "MHD", "CA")
hBoot_MHD4_ccdf <- ds.ccdfBoot(ht_CA_W$Height, ht_CA_W_max$Max, "MHD", "CA")

# Prepare graphics device
tiff(filename = "Figure U4.tif", width = 3304, height = 4544, units = "px",
     res = 800, compression = "lzw")

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
ds.ccdfBoot.plot2(hBoot_MHD1_ccdf, "black", "gray48", position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfBoot.plot2(hBoot_MHD2_ccdf, "red2", "indianred1", position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfBoot.plot2(hBoot_MHD3_ccdf, "black", "gray48", position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfBoot.plot2(hBoot_MHD4_ccdf, "red2", "indianred1", position = "bottom")
popViewport()

# Create legend
grid.text(label = c("Dist. height (W)", "Dist. height (NW)", "Max height (W)", "Max height (NW)"), 
          x = rep(0.79, 4), y = c(0.897, 0.917, 0.937 , 0.957), hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.759, 4), y0 = c(0.897, 0.917, 0.937 , 0.957), 
              x1 = rep(0.779, 4), y1 = c(0.897, 0.917, 0.937 , 0.957),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans,") ~ "unwarmed"),
                    expression(italic("C. nutans,") ~ "warmed"),
                    expression(italic("C. acanthoides,") ~ "unwarmed"),
                    expression(italic("C. acanthoides,") ~ "warmed")), 
          x = rep(0.096, 4), y = c(0.957, 0.725, 0.475, 0.234),
          hjust = 0, gp = gpar(cex = 0.5))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Unused] Estimate max height vs height distribution right tail dispersal percentile distances -------

# Prepare graphics device
tiff(filename = "Figure U5.tif", width = 3304, height = 4544, units = "px",
     res = 800, compression = "lzw")

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
ds.rtailBootMHD.plot(hBoot_MHD1_rtail, "black", "gray48", position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.rtailBootMHD.plot(hBoot_MHD2_rtail, "red2", "indianred1", position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.rtailBootMHD.plot(hBoot_MHD3_rtail, "black", "gray48", position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.rtailBootMHD.plot(hBoot_MHD4_rtail, "red2", "indianred1", position = "bottom")
popViewport()

# Create legend
grid.text(label = c("Dist. height (W)", "Dist. height (NW)", "Max height (W)", "Max height (NW)"), 
          x = rep(0.127, 4), y = c(0.857, 0.877, 0.897 , 0.917), hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.096, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.116, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans,") ~ "unwarmed"),
                    expression(italic("C. nutans,") ~ "warmed"),
                    expression(italic("C. acanthoides,") ~ "unwarmed"),
                    expression(italic("C. acanthoides,") ~ "warmed")), 
          x = rep(0.096, 4), y = c(0.957, 0.725, 0.475, 0.234),
          hjust = 0, gp = gpar(cex = 0.5))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Deprecated] Plot distribution of flower heights, including Vincent average  ------------------------

# Calculate Vincent averages for height distribution
mean.vincent <- function(hBoot){
  cdf <- function(coldata){
    return(cumsum(coldata/sum(coldata)))}
  cdf_values <- as.data.frame(apply(X = hBoot, MARGIN = 2, FUN = cdf))
  quantiles <- function(coldata){
    quantile_data <- data.frame(cbind(seq(0, 200, by = 0.1), coldata))
    return(approx(quantile_data[, 2], quantile_data[, 1], seq(0, 1, by = 0.0001),
                  yleft = 0, yright = 200)$y)}
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
    plot(x = bootData[[1]][, 1], y = bootData[[2]][, 1], type = "l",
         xlim = c(0, 200), ylim = c(0, 0.03), xaxt = "n", yaxt = "n",
         ylab = "Probability Density",  col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
    lines(bootData[[1]][, 1], bootData[[3]][, 1],
          col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))
    for(i in 2:1000){
      lines(bootData[[1]][, 1], bootData[[2]][, i],
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
      lines(bootData[[1]][, 1], bootData[[3]][, i],
            col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))}
    lines(density(bootData[[4]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(density(bootData[[5]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 2], col = "deepskyblue", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 3], col = "deepskyblue", lwd = 1.8)
    axis(side = 1, at = c(0, 50, 100, 150, 200), labels = FALSE)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}
  if(bottom == TRUE){
    plot(x = bootData[[1]][, 1], y = bootData[[2]][, 1], type = "l",
         xlim = c(0, 200), ylim = c(0, 0.03),  xlab = "Flower Height (cm)", yaxt = "n", 
         ylab = "Probability Density", col = rgb(red = 0.4, green = 0, blue = 0, alpha = 0.03))
    lines(bootData[[1]][, 1], bootData[[3]][, 1],
          col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))
    for(i in 2:1000){
      lines(bootData[[1]][, 1], bootData[[2]][, i],
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0.03))
      lines(bootData[[1]][, 1], bootData[[3]][, i],
            col = rgb(red = 1, green = 0.23, blue = 0.23, alpha = 0.03))}
    lines(density(bootData[[4]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(density(bootData[[5]], from = 0, to = 200), col = "gold", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 2], col = "deepskyblue", lwd = 1.8)
    lines(bootData[[1]][, 1], bootData[[1]][, 3], col = "deepskyblue", lwd = 1.8)
    axis(side = 2, at = c(0, 0.01, 0.02, 0.03), labels = TRUE)}}

# Bootstrap PDFs for NW/W CN and NW/W CA heights
hBoot_HD1_2 <- ht.pdfBoot2(ht_CN_NW$Height, ht_CN_W$Height)
hBoot_HD2_2 <- ht.pdfBoot2(ht_CA_NW$Height, ht_CA_W$Height)

# Prepare graphics device
jpeg(filename = "Figure D1.jpeg", width = 826, height = 568, units = "px")

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
              y1 = c(0.808, 0.838, 0.868, 0.898),
              gp = gpar(col = c("gold", "deepskyblue", "red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"),
          x = rep(0.08, 2), y = c(0.93, 0.48), hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Deprecated] Vincent average on dispersal kernels ---------------------------------------------------

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

