#*******************************************************************************
#*
#*
#*                     Creating Figure 6 of the manuscript
#*        (Distribution of characteristics with missing data per *type*)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the development version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries ----
list.of.packages <- c("tracenma", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Process the datasets to collect characteristics with at least one missing data ----
# Collect all datasets including the characteristic abbreviation (show.index = TRUE), type and subtype (show.type = TRUE)
collect_datasets <- lapply(index$PMID, function(x) get.dataset(pmid = x, show.index = TRUE, show.type = TRUE))

# Restrict only to columns with at least one missing value and preserve the number of missing data
num_NA_column <- lapply(collect_datasets, function(x) data.frame(freq = colSums(is.na(x$Dataset))[colSums(is.na(x$Dataset)) > 0]))

# Extract the names of columns with at least one missing value
names_column_NA00 <- lapply(num_NA_column, function(x) rownames(x))

# When a name ends with numbers, remove the numbers
names_column_NA0 <- lapply(names_column_NA00, function(x) gsub("[0-9]+$", "", x))

# Then, remove also specific strings from the character name
names_column_NA <- lapply(names_column_NA0, function(x) unique(trimws(sub(paste(c("mean.", "median.", "SD.", "|min.|", "max."), collapse = "|"), "", x))))

# Do the same for the characteristic(s) in 'Characteristics_index'
chars_index <- lapply(collect_datasets, function(x) sub(".begin.+", ".end",
                                                         sub("^mean.", "",
                                                              sub("median.", "",
                                                                   sub("..(SD).", "",
                                                                        sub("^min.", "",
                                                                             sub("..(max).", "", unlist(x$Characteristics_index[, 1]))))))))

# Find position of characteristic(s) in the first column of 'Characteristics_index'
id_char <- mapply(function(x, y) which(!is.na(match(x, y))), x = chars_index, y = names_column_NA)

# Find these characteristics in the first column of 'Characteristics_index'
find_char <- lapply(1:length(collect_datasets), function(x) collect_datasets[[x]]$Characteristics_index[id_char[[x]], 1])

# Find type of these characteristics in the third column of 'Characteristics_index'
type_char <- lapply(1:length(collect_datasets), function(x) unlist(collect_datasets[[x]]$Characteristics_index[id_char[[x]], 3]))



## Prepare the datasets for ggplot2 ----
# A1: Percentage of missing data across datasets
perc_NA_total <- unlist(lapply(collect_datasets, function(x) round(length(which(is.na(x$Dataset[, -c(1:5)]) == TRUE)) / prod(dim(x$Dataset[, -c(1:5)])) * 100, 1)))
length(perc_NA_total[perc_NA_total > 0]) # 182 datasets with missing data
summary(perc_NA_total[perc_NA_total > 0])

# B1: Number of characteristics with at least one missing data across the datasets
num_chars_with_NA <- unlist(lapply(num_NA_column, function(x) length(rownames(x))))
summary(num_chars_with_NA[num_chars_with_NA > 0])

# B2: Percentage of characteristics with at least one missing data across the datasets
perc_chars_with_NA <- unlist(lapply(1:length(collect_datasets), function(x) round((num_chars_with_NA[[x]] / dim(collect_datasets[[x]]$Dataset[, -c(1:5)])[2] ) * 100, 1)))
summary(perc_chars_with_NA[perc_chars_with_NA > 0])

# C1: Number of characteristics per characteristic *type*
num_chars_type <- lapply(collect_datasets, function(x) length(unlist(x$Characteristics_index[, 3])))

# C2: Number of missing characteristics per characteristic *type*
num_chars_NA_type <- lapply(type_char, function(x) data.frame(table(x)))

# C3: Percentage of missing characteristics per characteristic *type*
perc_chars_NA_type <- lapply(1:length(collect_datasets), function(x)  num_chars_NA_type[[x]]$Freq / num_chars_type[[x]])

# C4: Data-frame with the number and % of characteristics with at least one missing data per *type*
chars_NA_type <- rbind(data.frame(x = rep("All types", length(which(perc_chars_with_NA > 0))),
                                  Freq = num_chars_with_NA[num_chars_with_NA > 0],
                                  perc = perc_chars_with_NA[perc_chars_with_NA > 0]),
                       data.frame(do.call(rbind, lapply(type_char, function(x) data.frame(table(x)))),
                                  perc = round(unlist(perc_chars_NA_type) *100, 1)) )
colnames(chars_NA_type)[1:2] <- c("type", "number")

# Distribution of % characteristics with missing data (regardless of type) across datasets
distr_chars_NA <- as.matrix(summary(subset(chars_NA_type, type == "All types")[, 3]))

# Distribution of % *Clinical* characteristics with missing data across datasets
distr_clin_chars_NA <- as.matrix(summary(subset(chars_NA_type, type == "Clinical")[, 3]))

# Distribution of % *Demographic* characteristics with missing data across datasets
distr_demo_chars_NA <- as.matrix(summary(subset(chars_NA_type, type == "Demographic")[, 3]))

# Distribution of % *Methodological* characteristics with missing data across datasets
distr_metho_chars_NA <- as.matrix(summary(subset(chars_NA_type, type == "Methodological")[, 3]))

# Bring together
distr_chars_type_NA <- as.data.frame(rbind(distr_chars_NA, distr_clin_chars_NA, distr_demo_chars_NA, distr_metho_chars_NA))
distr_chars_type_NA$statistic <- rep(c("min", "q1", "median", "mean", "q3", "max"), 4)
distr_chars_type_NA$type <- rep(c("All types", "Clinical", "Demographic", "Methodological"), each = dim(distr_chars_type_NA)[1] / 4)
rownames(distr_chars_type_NA) <- NULL
colnames(distr_chars_type_NA)[1] <- "value"

# Add number of datasets below each box plot
num_datasets <- data.frame(type = c("All types", "Clinical", "Demographic", "Methodological"),
                           num = c(length(which(chars_NA_type$type == "All types") == TRUE),
                                   length(which(chars_NA_type$type == "Clinical") == TRUE),
                                   length(which(chars_NA_type$type == "Demographic") == TRUE),
                                   length(which(chars_NA_type$type == "Methodological") == TRUE)))
num_datasets$perc <- round((num_datasets$num / 217) * 100, 0)
num_datasets$type <- factor(num_datasets$type, levels = c("All types", "Clinical", "Demographic", "Methodological"))



## Obtain box plots with integrated dots ----
tiff("./Figures/Figure 6.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(chars_NA_type,
       aes(x = type,
           y = perc,
           color = type)) +
  geom_boxplot(size = 1,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_chars_type_NA, statistic == "median"),
            aes(x = type,
                y = value,
                label = paste0("Q2 = ", sprintf("%0.1f", value))),
            hjust = -1.85, # -1.95
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type_NA, statistic == "q1"),
            aes(x = type,
                y = value,
                label = paste0("Q1 = ", sprintf("%0.1f", value)),
                hjust = ifelse(value < 10, -2.15, -1.85)),  # -2.25, -1.95
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type_NA, statistic == "q3"),
            aes(x = type,
                y = value,
                label = paste0("Q3 = ", sprintf("%0.1f", value))),
            hjust = -1.85, # -1.95
            size = 4,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_type_NA, statistic == "min"),
            aes(x = type,
                y = value,
                label = paste0("minimum = ", sprintf("%0.1f", value))),
            hjust = 0.5,
            vjust = 2.3,
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_chars_type_NA, statistic == "max"),
            aes(x = type,
                y = value,
                label = paste0("maximum = ", sprintf("%0.1f", value))),
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
  labs(y = "Percentage of characteristics with missing values (%)",
       x = "Characteristic types",
       color =  "Characteristic types") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")
dev.off()

