# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(fishualize)
library(ggrepel)

setwd("/Users/lfitzger/Desktop/PhD/Chapter4_Hybrid/PCA/Final_Dataset")

# -----------------------------
# Input files
# -----------------------------
evec <- read_table2("HYB_FinalDataset_results.eigenvec", col_names = FALSE)
eval <- read_table2("HYB_FinalDataset_results.eigenval", col_names = FALSE)

# -----------------------------
# Format eigenvec
# -----------------------------
colnames(evec) <- c("FID", "IID", paste0("PC", 1:(ncol(evec) - 2)))

# -----------------------------
# Species labels
# -----------------------------
species_map <- c(
  CH100 = "A. chrysopterus",
  CH131 = "A. chrysopterus",
  CH167 = "A. chrysopterus",
  CH176 = "A. chrysopterus",
  CH29  = "A. chrysopterus",
  GB060 = "A. perideraion",
  GB086 = "A. perideraion",
  GB174_SAN = "A. sandaracinos",
  GB206_SAN = "A. sandaracinos",
  GB227_SAN = "A. sandaracinos",
  Hybrid = "Hybrid",
  LU009 = "A. leucokranos",
  LU050 = "A. leucokranos",
  LU130 = "A. leucokranos",
  LU131 = "A. leucokranos",
  LU14  = "A. leucokranos",
  LU62  = "A. leucokranos",
  NC005 = "A. perideraion",
  NC019 = "A. perideraion",
  PRDHC_18 = "A. perideraion",
  PRDHC_25 = "A. perideraion",
  PRDHC_9 = "A. perideraion",
  PRDHM_11 = "A. perideraion",
  PRDHM_12 = "A. perideraion",
  PRDHM_7  = "A. perideraion",
  PRD_11 = "A. perideraion",
  PRD_323 = "A. perideraion",
  PRD_342 = "A. perideraion",
  PRD_41 = "A. perideraion",
  SA173 = "A. sandaracinos",
  SA188 = "A. sandaracinos",
  SA200 = "A. sandaracinos",
  SA238 = "A. sandaracinos",
  SA260 = "A. sandaracinos",
  SA262 = "A. sandaracinos",
  SANSM_22 = "A. sandaracinos",
  SANSM_23 = "A. sandaracinos",
  SANSM_4 = "A. sandaracinos",
  SANSM_8 = "A. sandaracinos"
)

evec$species <- species_map[evec$IID]
evec$species <- factor(evec$species)

# -----------------------------
# Variance explained
# -----------------------------
var_explained <- eval$X1 / sum(eval$X1)
pc1_var <- round(var_explained[1] * 100, 2)
pc2_var <- round(var_explained[2] * 100, 2)

# -----------------------------
# PCA plot
# -----------------------------
p <- ggplot(evec, aes(x = PC1, y = PC2,  color = species, label = IID)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  
  theme_bw(base_size = 16) +
  
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 14),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(face = "italic"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  
  labs(
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    color = "Species"
  )

p

p_facet <- ggplot(evec, aes(PC1, PC2, label = IID)) +
  geom_point(aes(colour = species), size = 4) +
  geom_text_repel(
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  facet_wrap(~species) +
  coord_cartesian(
    xlim = range(evec$PC1),
    ylim = range(evec$PC2)
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p_facet

# -----------------------------
# Save plot
# -----------------------------
ggsave(
  "PCA_plot_PC1_PC2.pdf",
  p,
  width = 8,
  height = 6,
  dpi = 300
)

