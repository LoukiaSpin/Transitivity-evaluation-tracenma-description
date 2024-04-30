#*******************************************************************************
#*
#*
#*                       Creating Supplementary Figure S1
#*         (Bar plot on dataset frequency per characteristic frequency)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the developmental version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries
list.of.packages <- c("tracenma", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of data-frames on the characteristic types and subtypes
type_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)

# Merge the ethnicities into the same *one* label
type_chars[[177]] <- type_chars[[177]][c(-4, -5), ]



## Prepare dataset for ggplot2 ----
# Number of extracted characteristics (regardless of type) per dataset
num_chars <- sapply(type_chars, function(x) length(unlist(x[, 1])))
summary(num_chars)

# Number of extracted *Clinical* characteristics per dataset
num_chars_clin <- sapply(type_chars, function(x) length(which(x[, 2] == "Clinical")))

# Number of extracted *Demographic* characteristics per dataset
num_chars_demo <- sapply(type_chars, function(x) length(which(x[, 2] == "Demographic")))

# Number of extracted *Methodological* characteristics per dataset
num_chars_metho <- sapply(type_chars, function(x) length(which(x[, 2] == "Methodological")))

# Number of datasets per number of extracted characterstics based on their type
datasets_char <- rbind(data.frame(table(num_chars[num_chars > 0])),
                       data.frame(table(num_chars_clin[num_chars_clin > 0])),
                       data.frame(table(num_chars_demo[num_chars_demo > 0])),
                       data.frame(table(num_chars_metho[num_chars_metho > 0])))
colnames(datasets_char) <- c("num_chars", "Freq")
datasets_char$type <- rep(c("All types", "Clinical", "Demographic", "Methodological"),
                          c(length(table(num_chars[num_chars > 0])),
                            length(table(num_chars_clin[num_chars_clin > 0])),
                            length(table(num_chars_demo[num_chars_demo > 0])),
                            length(table(num_chars_metho[num_chars_metho > 0]))))
datasets_char$num_chars <- factor(datasets_char$num_chars, levels = as.character(1:35))



## Bar plot ----
tiff("./30_Analysis & Results/Figure S1.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(data = datasets_char,
       aes(x = num_chars,
           y = Freq,
           group = type,
           colour = type)) +
  geom_line(alpha = ifelse(datasets_char$type == "All types", 1, 0.2),
            size = 1.2) +
  geom_point(alpha = ifelse(datasets_char$type == "All types", 1, 0.2),
             size = 2.5) +
  geom_text(data = subset(datasets_char, type == "All types"),
            aes(x = num_chars,
                y = Freq,
                label = paste0(Freq, " ", "(", round((Freq / 217) * 100, 1), "%)") ),
            fontface = "bold",
            colour = "black",
            vjust = -0.5,
            size = 3.1,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("grey60", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y = "Number of datasets",
       x = "Number of characteristics",
       colour = "Characteristic types",
       alpha = " ") +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))
dev.off()

