##### [F1] Plot mean heights for each treatment/species combination ---------------------------------------

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





##### [F2] Plot height distribution PDF with 95% bootstrap interval ---------------------------------------

# Prepare graphics device
tiff(filename = "Figure 2.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

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





##### [F3.1] Estimate warmed vs not warmed PDF (dispersal kernels) ----------------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.1.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

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





##### [F3.2] Plot warmed vs not warmed CCDF ---------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.2.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

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





##### [F3.3] Plot warmed vs not warmed CCDF ratios --------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.3.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

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





##### [F3.4] Plot warmed vs not right tail dispersal percentile distances ---------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.4.tif", width = 3304, height = 2272, units = "px", res = 800, compression = "lzw")

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





##### [F4.1] Plot max height vs height distribution PDF (dispersal kernels) -------------------------------

# Prepare graphics device
tiff(filename = "Figure 4.1.tif", width = 3304, height = 4544, units = "px", res = 800, compression = "lzw")

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





##### [F4.2] Plot max height vs height distribution CCDF --------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 4.2.tif", width = 3304, height = 4544, units = "px", res = 800, compression = "lzw")

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





##### [F4.3] Plot max height vs height distribution CCDF ratio --------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 4.3.tif", width = 3304, height = 4544, units = "px", res = 800, compression = "lzw")

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





##### [F4.4] Estimate max height vs height distribution right tail dispersal percentile distances ---------

# Prepare graphics device
tiff(filename = "Figure 4.4.tif", width = 3304, height = 4544, units = "px", res = 800, compression = "lzw")

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

