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





##### [F2.1] Plot height distribution PDF with 95% bootstrap interval -------------------------------------

# Prepare graphics device
jpeg(filename = "Figure 2.1.jpeg", width = 826, height = 568, units = "px")

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
ht.pdfBoot.plot(hBoot_HD1, bottom = FALSE)
#abline(v = c(subset(ht_stats, Species == "CN" & Treatment == "Not Warmed")$MeanHeight,
#             subset(ht_stats, Species == "CN" & Treatment == "Warmed")$MeanHeight),
#       col = c("black", "red"), lty = c(2, 2))
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ht.pdfBoot.plot(hBoot_HD2, bottom = TRUE)
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
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F2.2] Same as above, but plot individual replicates and include Vincent average --------------------

# Prepare graphics device
jpeg(filename = "Figure 2.2.jpeg", width = 826, height = 568, units = "px")

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
              y1 = c(0.808, 0.838, 0.868, 0.898), gp = gpar(col = c("gold", "deepskyblue", "red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F3.1] Estimate warmed vs not warmed PDF (dispersal kernels) ----------------------------------------

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
ds.pdfBoot.plot(hBoot_WNW1_pdf, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.pdfBoot.plot(hBoot_WNW2_pdf, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F3.2] Plot warmed vs not warmed CCDF ---------------------------------------------------------------

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
ds.ccdfBoot.plot(hBoot_WNW1_ccdf, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.ccdfBoot.plot(hBoot_WNW2_ccdf, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F3.3] Plot warmed vs not warmed CCDF ratios --------------------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure 3.3.jpeg", width = 826, height = 568, units = "px")

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
ds.ccdfRatioBoot.plot(hBoot_WNW1_CCDFRatio, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.ccdfRatioBoot.plot(hBoot_WNW2_CCDFRatio, bottom = TRUE)
popViewport()

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F3.4] Plot warmed vs not right tail dispersal percentile distances ---------------------------------

# Prepare graphics device
jpeg(filename = "Figure 3.4.jpeg", width = 826, height = 568, units = "px")

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
ds.rtailBootWNW.plot(hboot_WNW1_rtail, bottom = FALSE)
popViewport()

# CA non-warmed vs warmed
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.rtailBootWNW.plot(hboot_WNW2_rtail, bottom = TRUE)
popViewport()

# Create legend
grid.text(label = c("Warmed", "Not Warmed"), x = rep(0.84, 2), y = c(0.87, 0.90),
          hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.813, 2), y0 = c(0.868, 0.898), x1 = rep(0.833, 2), y1 = c(0.868, 0.898),
              gp = gpar(col = c("red", "black")))

# Create figure labels
grid.text(label = c("(A) Carduus nutans", "(B) Carduus acanthoides"), x = rep(0.08, 2), y = c(0.93, 0.48),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F4.1] Plot max height vs height distribution PDF (dispersal kernels) -------------------------------

# Prepare graphics device
jpeg(filename = "Figure 4.1.jpeg", width = 826, height = 825, units = "px")

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
ds.pdfBoot.plot2(hBoot_MHD1_pdf, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
ds.pdfBoot.plot2(hBoot_MHD2_pdf, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
ds.pdfBoot.plot2(hBoot_MHD3_pdf, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.pdfBoot.plot2(hBoot_MHD4_pdf, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("(A) Carduus nutans, unwarmed", "(B) Carduus nutans, warmed",
                    "(C) Carduus acanthoides, unwarmed", "(D) Carduus acanthoides, warmed"), 
          x = rep(0.08, 4), y = c(0.933, 0.715, 0.45, 0.235),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F4.2] Plot max height vs height distribution CCDF --------------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure 4.2.jpeg", width = 826, height = 825, units = "px")

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
ds.ccdfBoot.plot2(hBoot_MHD1_ccdf, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
ds.ccdfBoot.plot2(hBoot_MHD2_ccdf, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
ds.ccdfBoot.plot2(hBoot_MHD3_ccdf, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.ccdfBoot.plot2(hBoot_MHD4_ccdf, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("(A) Carduus nutans, unwarmed", "(B) Carduus nutans, warmed",
                    "(C) Carduus acanthoides, unwarmed", "(D) Carduus acanthoides, warmed"), 
          x = rep(0.08, 4), y = c(0.933, 0.715, 0.45, 0.235),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F4.3] Plot max height vs height distribution CCDF ratio --------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure 4.3.jpeg", width = 826, height = 825, units = "px")

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
ds.ccdfRatioBoot.plot2(hBoot_MHD1_ccdfRatio, bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
ds.ccdfRatioBoot.plot2(hBoot_MHD2_ccdfRatio, bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
ds.ccdfRatioBoot.plot2(hBoot_MHD3_ccdfRatio, bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.ccdfRatioBoot.plot2(hBoot_MHD4_ccdfRatio, bNum = TRUE, bLab = TRUE)
popViewport()

# Create figure labels
grid.text(label = c("(A) Carduus nutans, unwarmed", "(B) Carduus nutans, warmed",
                    "(C) Carduus acanthoides, unwarmed", "(D) Carduus acanthoides, warmed"), 
          x = rep(0.08, 4), y = c(0.933, 0.715, 0.45, 0.235),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [F4.4] Estimate max height vs height distribution right tail dispersal percentile distances ---------

# Prepare graphics device
jpeg(filename = "Figure 4.4.jpeg", width = 826, height = 825, units = "px")

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
ds.rtailBootMHD.plot(hBoot_MHD1_rtail, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CN warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 600:1200, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(3, 4, 1, 1))
ds.rtailBootMHD.plot(hBoot_MHD2_rtail, "red2", "indianred1", bNum = TRUE, bLab = FALSE)
popViewport()

# CA non-warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1200:1800, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(2, 4, 2, 1))
ds.rtailBootMHD.plot(hBoot_MHD3_rtail, "black", "gray48", bNum = FALSE, bLab = FALSE)
popViewport()

# CA warmed: mean vs distribution
pushViewport(vp = viewport(layout.pos.row = 1800:2400, layout.pos.col = 1:800))
par(fig = gridFIG())
par(new = TRUE)
par(mar = c(4, 4, 0, 1))
ds.rtailBootMHD.plot(hBoot_MHD4_rtail, "red2", "indianred1", bNum = TRUE, bLab = TRUE)
popViewport()

# Create legend
grid.text(label = c("Dist. Height (W)", "Dist. Height (NW)", "Max Height (W)", "Max Height (NW)"), 
          x = rep(0.83, 4), y = c(0.86, 0.88, 0.90 , 0.92), hjust = 0, gp = gpar(cex = 0.8))
grid.segments(x0 = rep(0.803, 4), y0 = c(0.857, 0.877, 0.897 , 0.917), 
              x1 = rep(0.823, 4), y1 = c(0.857, 0.877, 0.897 , 0.917),
              gp = gpar(col = rep(c("red2", "black"), 2), lty = c(1, 1, 3, 3)))

# Create figure labels
grid.text(label = c("(A) Carduus nutans, unwarmed", "(B) Carduus nutans, warmed",
                    "(C) Carduus acanthoides, unwarmed", "(D) Carduus acanthoides, warmed"), 
          x = rep(0.08, 4), y = c(0.933, 0.715, 0.45, 0.235),
          hjust = 0, gp = gpar(cex = 1.1))
popViewport()

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

