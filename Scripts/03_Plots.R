##### [F1] Plot height distribution PDF with 95% bootstrap interval ---------------------------------------

# Prepare graphics device
tiff(filename = "Figure 1.tif", width = 3304, height = 2272, units = "px",
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
ht.pdfBoot.plot(hBoot_HD1, bottom = FALSE)
#abline(v = c(subset(ht_stats, Species == "CN" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CN" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ht.pdfBoot.plot(hBoot_HD2, bottom = TRUE)
#abline(v = c(subset(ht_stats, Species == "CA" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CA" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# Note: commented out code above adds a vertical line at the mean

# Create legend
grid.text(label = c("Not warmed", "Warmed"), x = rep(0.834, 2), y = c(0.918, 0.885),
          hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.806, 2), y0 = c(0.918, 0.885), x1 = rep(0.826, 2), y1 = c(0.918, 0.885),
              gp = gpar(col = c("black", "red")))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F2] Estimate warmed vs not warmed PDF (dispersal kernels) ------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 2.tif", width = 3304, height = 2272, units = "px",
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
ds.pdfBoot.plot(hBoot_WNW1_pdf, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot(hBoot_WNW2_pdf, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Not warmed", "Warmed"), x = rep(0.834, 2), y = c(0.918, 0.885),
          hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.806, 2), y0 = c(0.918, 0.885), x1 = rep(0.826, 2), y1 = c(0.918, 0.885),
              gp = gpar(col = c("black", "red")))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F3] Plot warmed vs not warmed CCDF ratios ----------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.tif", width = 3304, height = 2272, units = "px",
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
ds.ccdfRatioBoot.plot(hBoot_WNW1_CCDFRatio, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot(hBoot_WNW2_CCDFRatio, bottom = TRUE)
popViewport()

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F4] Plot max height vs height distribution PDF (dispersal kernels) ---------------------------------

# Prepare graphics device
tiff(filename = "Figure 4.tif", width = 3304, height = 4544, units = "px",
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
ds.pdfBoot.plot2(hBoot_MHD1_pdf, "black", "gray48", position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MHD2_pdf, "red2", "indianred1", position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MHD3_pdf, "black", "gray48", position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MHD4_pdf, "red2", "indianred1", position = "bottom")
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





##### [F5] Plot max height vs height distribution CCDF ratio ----------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 5.tif", width = 3304, height = 4544, units = "px",
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
ds.ccdfRatioBoot.plot2(hBoot_MHD1_ccdfRatio, position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MHD2_ccdfRatio, position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MHD3_ccdfRatio, position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MHD4_ccdfRatio, position = "bottom")
popViewport()

# Create figure labels
grid.text(label = c(expression(italic("C. nutans,") ~ "unwarmed"),
                    expression(italic("C. nutans,") ~ "warmed"),
                    expression(italic("C. acanthoides,") ~ "unwarmed"),
                    expression(italic("C. acanthoides,") ~ "warmed")), 
          x = rep(0.096, 4), y = c(0.957, 0.725, 0.475, 0.234),
          hjust = 0, gp = gpar(cex = 0.5))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [FS1] Plot height distribution PDF with 95% bootstrap interval, including observations --------------

# Plot histograms before inserting into grid
par(mar = c(1, 1.75, 0.75, 0.75))
ht_CN_NW_hist <- hist(ht_CN_NW$Height, breaks = seq(0, 200, length.out = 40), freq = FALSE,
                      ylim = c(0, 0.03), xaxt = "n", yaxt = "n")
ht_CN_W_hist <- hist(ht_CN_W$Height, breaks = seq(0, 200, length.out = 40), freq = FALSE,
                     ylim = c(0, 0.03), xaxt = "n", yaxt = "n")
par(mar = c(1.75, 1.75, 0, 0.75))
ht_CA_NW_hist <- hist(ht_CA_NW$Height, breaks = seq(0, 200, length.out = 40), freq = FALSE,
                      ylim = c(0, 0.03), xaxt = "n", yaxt = "n")
ht_CA_W_hist <- hist(ht_CA_W$Height, breaks = seq(0, 200, length.out = 40), freq = FALSE,
                     ylim = c(0, 0.03), xaxt = "n", yaxt = "n")

# Function to plot data and bootstrapped height distributions, with mean line and 95% band
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

# Prepare graphics device
tiff(filename = "Figure S1.tif", width = 3304, height = 2272, units = "px",
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
ht.pdfBoot.plot2(hBoot_HD1, ht_CN_NW_hist, ht_CN_W_hist, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ht.pdfBoot.plot2(hBoot_HD2, ht_CA_NW_hist, ht_CA_W_hist, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Not warmed", "Warmed"), x = rep(0.834, 2), y = c(0.918, 0.885),
          hjust = 0, gp = gpar(cex = 0.5))
grid.segments(x0 = rep(0.806, 2), y0 = c(0.918, 0.885), x1 = rep(0.826, 2),
              y1 = c(0.918, 0.885), gp = gpar(col = c("black", "red")))

# Create figure labels
grid.text(label = c(expression(italic("C. nutans")), expression(italic("C. acanthoides"))),
          x = rep(0.096, 2), y = c(0.918, 0.471), hjust = 0, gp = gpar(cex = 0.5))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [FS2] Plot max height point source vs height distribution PDF (dispersal kernels) -------------------

# Prepare graphics device
tiff(filename = "Figure 4.tif", width = 3304, height = 4544, units = "px",
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
ds.pdfBoot.plot2(hBoot_MPHD1_pdf, "black", "gray48", position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MPHD2_pdf, "red2", "indianred1", position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MPHD3_pdf, "black", "gray48", position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.pdfBoot.plot2(hBoot_MPHD4_pdf, "red2", "indianred1", position = "bottom")
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





##### [FS3] Plot max height point source vs height distribution CCDF ratio --------------------------------

# Prepare graphics device
tiff(filename = "Figure 5.tif", width = 3304, height = 4544, units = "px",
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
ds.ccdfRatioBoot.plot2(hBoot_MPHD1_ccdfRatio, position = "top")
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MPHD2_ccdfRatio, position = "centre")
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MPHD3_ccdfRatio, position = "centre")
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
ds.ccdfRatioBoot.plot2(hBoot_MPHD4_ccdfRatio, position = "bottom")
popViewport()

# Create figure labels
grid.text(label = c(expression(italic("C. nutans,") ~ "unwarmed"),
                    expression(italic("C. nutans,") ~ "warmed"),
                    expression(italic("C. acanthoides,") ~ "unwarmed"),
                    expression(italic("C. acanthoides,") ~ "warmed")), 
          x = rep(0.096, 4), y = c(0.957, 0.725, 0.475, 0.234),
          hjust = 0, gp = gpar(cex = 0.5))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

