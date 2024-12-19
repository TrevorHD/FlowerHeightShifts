# Overview

Climate change may significantly alter how organisms disperse, with implications for population spread and species management. Wind dispersed plants have emerged as a useful study system for investigating how climate change affects dispersal, though studies modeling wind dispersal often assume propagules are released from a single point on an individual. This simplifying assumption, while useful, may misestimate dispersal. Here, we investigate effects of climate change on dispersal distances and spread rates, examining how these quantities shift when accounting for all points of seed release on an individual. Using the wind-dispersed invasive thistles *Carduus nutans* and *Carduus acanthoides*, we quantify temperature-driven shifts in the distribution of flower head heights using a passive warming field experiment, and estimate how these shifts affect dispersal using the Wald analytical long distance (WALD) model; for *C. nutans*, we use existing demographic data to simulate how these shifts affect population spread rates. We also compare dispersal distances for both warmed and ambient temperature plants, considering the entire distribution of flower head heights versus the common assumption of point-source seed release at the maximum height. For experimentally-grown individuals, an approximately 0.6 °C higher growing temperature increased mean and maximum flower head height by 14.1 cm (15.0%) and 14.0 cm (13.2%), respectively, in *C. nutans* and by 21.2 cm (26.6%) and 31.8 cm (36.7%), respectively, in *C. acanthoides*. Seeds from warmed individuals were more likely to exceed a given dispersal distance than those from their unwarmed counterparts; warmed *C. nutans* and *C. acanthoides* seeds were on average 1.36 and 1.71 times as likely, respectively, to travel 10 m or more in dispersal simulations, with this disparity becoming higher at longer dispersal distances. For *C. nutans*, increased growing temperatures boosted simulated rates of population spread by 42.2%, while assuming dispersal from a maximum-height point source rather than the true distribution of flower head heights increased simulated spread by up to 28.5%. Our results not only demonstrate faster population spread under increased temperatures, but also have substantial implications for modeling such spread, as the common simplifying assumption of dispersal from a single maximum-height source may substantially overestimate spread rates.

*The corresponding publication for this repository can be found [here](https://doi.org/10.1002/ecy.4201). Note that the raw manuscripts and appendices in this repository may differ slightly from the published version.*

<br/>

# Files

## Data

**ThistleData** *(.xlsx)* - Data including flower heights and stages from the experimental thistles.

**SeedDropData** *(.csv)* - Data on seed terminal velocities, used to model seed dispersal (old).

**SeedDropData2** *(.txt)* - Data on seed terminal velocities, used to model seed dispersal (current).

**Weather1** *(.csv)* - Weather data from the study site, with wind speeds used to model seed dispersal.

**Weather2** *(.csv)* -  Weather data from the study site, with wind speeds used to model seed dispersal.

## Figures

**Figure 1** *(.tif)* - Distribution of observed flower head heights for warmed/unwarmed *C. nutans* and *C. acanthoides*, with histograms.

**Figure 2** *(.tif)* - Dispersal kernels for warmed/unwarmed *C. nutans* and *C. acanthoides*, with bootstrap intervals.

**Figure 3** *(.tif)* - Relative risk of a seed exceeding a given distance for warmed maternal plants relative to unwarmed maternal plants.

**Figure 4** *(.tif)* - Relative risk of a seed exceeding a given distance for max height dispersal simulations relative to distributed height dispersal simulations.

**Figure S1** *(.tif)* - Distribution of observed flower head heights for warmed/unwarmed *C. nutans* and *C. acanthoides*, with bootstrap intervals.

**Figure S2** *(.tif)* - Dispersal kernels for warmed/unwarmed x distributed/max height for *C. nutans* and *C. acanthoides*, with bootstrap intervals.


## Media

**Presentation** *(.pptx)* - A PowerPoint presentation (for ESA 2021) regarding this research.

**Presentation** *(.pdf)* - A PDF version of the above PowerPoint slides.

**Transcript** *(.docx)* - A transcript of the recorded presentation in docx format.

**Transcript** *(.pdf)* - A transcript of the recorded presentation in PDF format.

## Scripts

**01_Setup** *(.R)* - Code used to load in data and define functions for analysis and plotting.

**02_Stats** *(.R)* - Code used for various statistical analyses on the data.

**03_Plots** *(.R)* - Code used for plotting figures.

**04_Wavespeeds** *(.R)* - Code used for simulating wavespeeds; adapted from Zhang *et al*. (2011) and Teller *et al*. (2016).

**S1_Extras** *(.R)* - Supplementary code not used in the main analyses.

## Other

**WarmingHeightsMS_v11_Ecology** *(.docx)* - Latest version of the manuscript for this research, submitted to *Ecology*.

**WarmingHeightsMS_v11_Ecology_Appendix_S1** *(.docx)* - Supplemental material containing a brief description of methodology used to estimate *C. nutans* spread rates, and tables for select spread rate statistics.

**WarmingHeightsMS_v11_Ecology_Appendix_S2** *(.docx)* - Supplemental material containing an alternative representation of Figure 1, dispersal kernels for various combinations of treatment and species, and tables with select statistics comparing dispersal simulations between both warmed/unwarmed treatments and maximum/distributed flower head height.
