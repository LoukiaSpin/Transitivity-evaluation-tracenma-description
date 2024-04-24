#*******************************************************************************
#*
#*
#*                     Creating Figure 1 of the manuscript
#*         (Box plots with integrated dots on characteristics per type)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("tracenma", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of data-frames on the characteristic types and subtypes
type_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)

# Merge the ethnicities into the same *one* label
type_chars[[177]] <- type_chars[[177]][c(-4, -5), ]

# Distribution of number of characteristics (regardless of type) across datasets
num_chars <- sapply(type_chars, function(x) length(unlist(x[, 1])))
distr_chars <- as.matrix(summary(num_chars))

# Distribution of number of *Clinical* characteristics across datasets
num_clin_chars <- sapply(type_chars, function(x) length(which(x[, 2] == "Clinical")))
distr_clin_chars <- as.matrix(summary(num_clin_chars[num_clin_chars > 0]))

# Distribution of number of *Demographic* characteristics across datasets
num_demo_chars <- sapply(type_chars, function(x) length(which(x[, 2] == "Demographic")))
distr_demo_chars <- as.matrix(summary(num_demo_chars[num_demo_chars > 0]))

# Distribution of number of *Methodological* characteristics across datasets
num_metho_chars <- sapply(type_chars, function(x) length(which(x[, 2] == "Methodological")))
distr_metho_chars <- as.matrix(summary(num_metho_chars[num_metho_chars > 0]))

# Bring together
distr_chars_type <- as.data.frame(rbind(distr_chars, distr_clin_chars, distr_demo_chars, distr_metho_chars))
distr_chars_type$statistic <- rep(c("min", "q1", "median", "mean", "q3", "max"), 4)
distr_chars_type$type <- rep(c("All types", "Clinical", "Demographic", "Methodological"), each = dim(distr_chars_type)[1] / 4)
rownames(distr_chars_type) <- NULL
colnames(distr_chars_type)[1] <- "value"
distr_chars_type[distr_chars_type$statistic == "q3" & distr_chars_type$type == "Demographic", ] <- NA


## Distribution of characteristics per *type* ----
# Number of characteristics per type in each dataset
chars_type_dataset <- lapply(type_chars, function(x) as.data.frame(table(x[, 2])))

# Data-frame for the ggplot
dataset_chars_type <- rbind(data.frame(Characteristic.type = rep("All types", length(num_chars)),
                                       Freq = num_chars),
                            do.call(rbind, chars_type_dataset))
colnames(dataset_chars_type) <- c("type", "frequency")
dataset_chars_type$type <- factor(dataset_chars_type$type, levels = c("All types", "Clinical", "Demographic", "Methodological"))

# Add number of datasets below each box plot
num_datasets <- data.frame(num = c(length(num_chars), length(which(num_clin_chars > 0)), length(which(num_demo_chars > 0)), length(which(num_metho_chars > 0))),
                           type = c("All types", "Clinical", "Demographic", "Methodological"))
num_datasets$perc <- round((num_datasets$num / 217) * 100, 0)
num_datasets$type <- factor(num_datasets$type, levels = c("All types", "Clinical", "Demographic", "Methodological"))

# Box plots with integrated dot
tiff("./30_Analysis & Results/Figure 1.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(dataset_chars_type,
       aes(x = type,
           y = frequency,
           color = type)) +
  geom_boxplot(size = 1,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_chars_type, statistic == "median"),
            aes(x = type,
                y = value,
                label = paste0("Q2 = ", value),
                hjust = ifelse(value < 10 , -2.7, -2.25)),
            #hjust = -2.7, # -3.5
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type, statistic == "q1"),
            aes(x = type,
                y = value,
                label = paste0("Q1 = ", value)),
            hjust = -2.7, # -3.5
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type, statistic == "q3"),
            aes(x = type,
                y = value,
                label = paste0("Q3 = ", value),
                hjust = ifelse(value < 10 , -2.7, -2.25)),
            #hjust = -2.7, # -3.5
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type, statistic == "min"),
            aes(x = type,
                y = value,
                label = paste0("minimum = ", value)),
            hjust = 0.5,
            vjust = 2.3,
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_chars_type, statistic == "max"),
            aes(x = type,
                y = value,
                label = paste0("maximum = ", value)),
            hjust = 0.5,
            vjust = -1.5,
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = num_datasets,
            aes(x = type,
                y = 0,
                label = paste0(num, " (", perc, "%) datasets")),
            hjust = 0.5,
            vjust = 2.5,
            colour = "grey45",
            size = 4,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("black", "#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y = "Number of characteristics",
       x = "Characteristic types",
       color =  "Characteristic types") +
  ylim(0, 40) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")
dev.off()
