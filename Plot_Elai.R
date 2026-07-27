###############################################################
## Ancestry plotting script                                  ##
##  anc1 = peri, anc2 = sand                                 ##
###############################################################

setwd("/Users/lfitzger/Desktop/PhD/Chapter4_Hybrid/ELAI/")

library(tidyverse)
library(data.table)
library(ggrastr)     
library(patchwork)

snps <- fread("HYB_elai_allchr_mg1_123.snpinfo.txt") %>%
  mutate(chr = as.character(chr)) %>%
  arrange(chr, pos)

ancestry <- scan("HYB_elai_allchr_mean_ps21.txt")

# anc1 = perideraion, anc2 = sandaracinos
anc1 <- ancestry[seq(1, length(ancestry), 2)]
anc2 <- ancestry[seq(2, length(ancestry), 2)]

stopifnot(length(anc1) == nrow(snps))

df <- tibble(
  chr = snps$chr,
  pos = snps$pos,
  peri = anc1,
  sand = anc2
) %>%
  mutate(
    state = case_when(
      peri > 1.8 ~ "homo_peri",
      sand > 1.8 ~ "homo_sand",
      peri > 0.8 & peri < 1.2 ~ "hetero",
      TRUE ~ "other"
    ),
    state = factor(state, levels = c("homo_sand", "hetero", "homo_peri", "other")),
    chr = factor(chr, levels = as.character(24:1))  
  )

###############################################################
## Panel B: Chromosome-level local ancestry (karyotype plot) ##
###############################################################

p <- ggplot(df, aes(x = pos, y = chr, fill = state)) +
  geom_tile(height = 0.8) +
  scale_fill_manual(
    values = c(
      homo_sand = "#04EAB8FF",
      hetero    = "#D8D643FF",
      homo_peri = "#00A5FFFF",
      other     = "grey80"
    ),
    labels = c(
      homo_sand = expression(paste("Homozygous ", italic("A. sandaracinos"), " sites")),
      hetero    = "Heterozygous sites",
      homo_peri = expression(paste("Homozygous ", italic("A. perideraion"), " sites")),
      other     = "Other / uncertain"
    ),
    name = "Local Ancestry"
  ) +
  theme_minimal() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Chromosome")


ggsave("HYB_karyotype_corrected.png", p, width = 14, height = 10, dpi = 300)
ggsave("HYB_karyotype_corrected.pdf", p, width = 14, height = 10)

p_raster <- ggrastr::rasterize(p, dpi = 300)

###############################################################
## Panel A: Genome-wide ancestry composition (stacked bar)   ##
###############################################################

df_prop <- df %>%
  count(state) %>%
  mutate(
    prop = n / sum(n),
    percent_label = scales::percent(prop, accuracy = 0.1)
  )

other_prop <- df_prop %>% filter(state == "other") %>% pull(percent_label)

df_prop_main <- df_prop %>%
  filter(state != "other") %>%
  mutate(state = factor(state, levels = c("homo_sand", "hetero", "homo_peri")))

pA <- ggplot(df_prop_main, aes(x = "Genome", y = prop, fill = state)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(
    data = df_prop_main %>% filter(state != "homo_peri"),
    aes(label = percent_label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = df_prop_main %>% filter(state == "homo_peri"),
    aes(y = 0.035, label = percent_label),
    color = "#00A5FFFF",
    size = 4
  ) +
  annotate(
    "text",
    x = 1,
    y = -0.035,
    label = paste0("Other / uncertain: ", other_prop),
    color = "grey40",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      homo_sand = "#04EAB8FF",
      hetero    = "#D8D643FF",
      homo_peri = "#00A5FFFF"
    ),
    labels = c(
      homo_sand = expression(paste("Homozygous ", italic("A. sandaracinos"), " sites")),
      hetero    = "Heterozygous sites",
      homo_peri = expression(paste("Homozygous ", italic("A. perideraion"), " sites"))
    ),
    name = "Local Ancestry"
  ) +
  scale_y_continuous(
    limits = c(-0.06, 1.10),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  theme(
    legend.position = "blank",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


ggsave("HYB_genomewide_proportion.png", pA, width = 6, height = 5, dpi = 300)
ggsave("HYB_genomewide_proportion.pdf", pA, width = 6, height = 5)

###############################################################
## Combine Panel A and rasterized Panel B                    ##
###############################################################

design <- "
AABBB
AABBB
"

final_fig <-
  pA + p_raster +
  plot_layout(
    design = design,
    widths = c(1, 1, 1, 1, 1)
  ) +
  plot_annotation(
    tag_levels = "A",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(
      size = 14,
      face = "plain",
      hjust = 0,
      vjust = 1.5
    ),
    plot.tag.position = c(0, 1)
  )

final_fig

###############################################################
## Export final combined figure                              ##
###############################################################

ggsave(
  filename = "HYB_PanelA_PanelB_combined.png",
  plot = final_fig,
  width = 14,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "HYB_PanelA_PanelB_combined.pdf",
  plot = final_fig,
  width = 14,
  height = 7,
  dpi = 300
)
