library(ggplot2)
library(reshape2)
library(dplyr)

setwd("/Users/lfitzger/Desktop/PhD/Chapter4_Hybrid/Admixout/Final_Dataset/")

# ------------------------
# 1. Read sample IDs
# ------------------------
fam <- read.table("HYB_FinalDataset_pruned.fam", stringsAsFactors = FALSE)
samples <- fam$V2   # IID column

# ------------------------
# 2. Read metadata (IID + Species)
# ------------------------
metadata <- read.csv("hybrid_check.csv", stringsAsFactors = FALSE)

# ------------------------
# 3. Clean species names
# ------------------------
# Convert A_chrysopterus → A. chrysopterus
metadata$Species <- gsub("_", " ", metadata$Species)     # underscores → spaces
metadata$Species <- gsub("^A ", "A. ", metadata$Species) # add dot after A
metadata$Species <- trimws(metadata$Species)             # remove hidden spaces

# Normalize Hybrid (in case of weird formatting)
metadata$Species[grepl("^Hybrid", metadata$Species, ignore.case = TRUE)] <- "Hybrid"

# Shorten Hybrid label
metadata$Species[metadata$Species == "Hybrid"] <- "Hyb."

# Match metadata to sample order
metadata <- metadata[match(samples, metadata$IID), ]

# ------------------------
# 4. Reorder species in desired order
# ------------------------
metadata$Species <- factor(
  metadata$Species,
  levels = c(
    "A. chrysopterus",
    "A. leucokranos",
    "A. sandaracinos",
    "Hyb.",
    "A. perideraion"
  )
)

# ------------------------
# 5. Define custom ancestry colors for K = 4
# ------------------------
ancestry_colors <- c(
  Ancestry1 = "#A790DBFF",
  Ancestry2 =  "#00C3F3FF",
  Ancestry3 = "#268DECFF",
  Ancestry4 = "#04EAB8FF"
)

# ------------------------
# 6. Identify Q file for K = 4
# ------------------------
q_files <- list.files(pattern = "HYB_FinalDataset_pruned.*\\.Q")

for (qfile in q_files) {
  
  # Extract K from filename
  K <- as.numeric(gsub(".*pruned\\.(\\d+)\\.Q", "\\1", qfile))
  
  # Only plot K = 4
  if (K != 4) next
  
  # ------------------------
  # 7. Read Q matrix
  # ------------------------
  Q <- read.table(qfile)
  colnames(Q) <- paste0("Ancestry", 1:K)
  
  # Add sample and metadata
  Q$Sample  <- samples
  Q$Species <- metadata$Species
  
  # Melt for ggplot
  Q_melt <- melt(
    Q,
    id.vars = c("Sample", "Species"),
    variable.name = "Ancestry",
    value.name = "Proportion"
  )
  
  # ------------------------
  # 8. Plot ADMIXTURE K = 4
  # ------------------------
  p_admix <- ggplot(Q_melt, aes(x = Sample, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity") +
    facet_grid(~ Species, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = ancestry_colors) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      strip.text = element_text(size = 8, margin = margin(t = 4, b = 4)),
      strip.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    ) +
    labs(
      title = "ADMIXTURE K = 4",
      x = "Individuals",
      y = "Ancestry proportion",
      fill = "Ancestry"
    )
  
  p_admix
  
  # ------------------------
  # 9. Save PDF
  # ------------------------
  pdf("Figure2_ADMIXTURE_K4_Species.pdf", width = 12, height = 6)
  print(p_admix)
  dev.off()
  
}
