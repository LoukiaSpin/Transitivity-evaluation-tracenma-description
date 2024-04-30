#*******************************************************************************
#*
#*
#*                     Creating Figure 3 of the manuscript
#*     (Bar plot on dataset frequency per characteristic type and subtype)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the development version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries ----
list.of.packages <- c("tracenma", "plyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of vectors with unique characteristic subtypes per dataset
subtype_chars <-
  factor(unlist(lapply(pmid_index,
                       function(x) unique(get.dataset(pmid = x, show.type = TRUE)$Characteristics_index[, 3]))),
         levels = c("Participant", "Intervention", "Outcome", "Age", "Sex", "Ethnicity", "Study design", "Study setting", "Risk of bias", "Withdrawals"))



## Grouped barplot of frequency of datasets by subtypes ----
# Prepare dataset
dataset_plot <- as.data.frame(table(subtype_chars))
dataset_plot$type <- rep(c("Clinical", "Demographic", "Methodological"), c(3, 3, 4))
dataset_plot$subtype_chars <- revalue(dataset_plot$subtype_chars, c("Intervention" = "Treatment"))

# Create Bar plot
tiff("./Figures/Figure 3.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(dataset_plot,
       aes(x = subtype_chars,
           y = Freq,
           fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = subtype_chars,
                y = Freq,
                label = paste0(Freq, " ", "(", round((Freq / 217) * 100, 0), "%)")),
            size = 4.5,
            vjust = -0.3) +
  labs(x = "Characteristic subtypes",
       y = "Number of datasets",
       fill = "Characteristic types") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  ylim(0, 217) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))
dev.off()

