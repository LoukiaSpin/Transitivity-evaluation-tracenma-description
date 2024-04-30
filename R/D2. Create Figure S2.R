#*******************************************************************************
#*
#*
#*                       Creating Supplementary Figure S2
#*      (Distribution of characteristics with missing data per *subtype*)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the development version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries ----
list.of.packages <- c("tracenma", "plyr", "ggplot2", "ggpubr")
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

# Find subtype of these characteristics in the fourth column of 'Characteristics_index'
subtype_char <- lapply(1:length(collect_datasets), function(x) unlist(collect_datasets[[x]]$Characteristics_index[id_char[[x]], 4]))



## Prepare the datasets for ggplot2 ----
# Number of characteristics per characteristic *subtype*
num_chars_subtype <- lapply(collect_datasets, function(x) length(unlist(x$Characteristics_index[, 3])))

# Number of missing characteristics per characteristic *subtype*
num_chars_NA_subtype <- lapply(subtype_char, function(x) data.frame(table(x)))

# Percentage of missing characteristics per characteristic *subtype*
perc_chars_NA_subtype <- lapply(1:length(collect_datasets), function(x) num_chars_NA_subtype[[x]]$Freq / num_chars_subtype[[x]])

# Data-frame with the number and % of characteristics with at least one missing data per *subtype*
chars_NA_subtype <- rbind(data.frame(do.call(rbind, lapply(subtype_char, function(x) data.frame(table(x)))),
                                     round(unlist(perc_chars_NA_subtype) *100, 1)) )
colnames(chars_NA_subtype) <- c("subtype", "number", "perc")
chars_NA_subtype$subtype <- revalue(chars_NA_subtype$subtype, c("Intervention" = "Treatment"))
chars_NA_subtype$subtype <- factor(chars_NA_subtype$subtype, levels = c("Participant", "Treatment", "Outcome",
                                                                        "Sex", "Age", "Ethnicity",
                                                                        "Study design", "Study setting", "Risk of bias", "Withdrawals"))


# Distribution of % *Participant* characteristics (*Clinical* subtype) with missing data across datasets
distr_parti_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Participant")[, 3]))

# Distribution of % *Treatment* characteristics (*Clinical* subtype) with missing data across datasets
distr_treat_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Treatment")[, 3]))

# Distribution of % *Outcome* characteristics (*Clinical* subtype) with missing data across datasets
distr_outco_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Outcome")[, 3]))

# Distribution of % *Study design* characteristics (*Methodological* subtype) with missing data across datasets
distr_design_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Study design")[, 3]))

# Distribution of % *Study setting* characteristics (*Methodological* subtype) with missing data across datasets
distr_setting_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Study setting")[, 3]))

# Distribution of % *Risk of bias* characteristics (*Methodological* subtype) with missing data across datasets
distr_rob_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Risk of bias")[, 3]))

# Distribution of % *Withdrawals* characteristics (*Methodological* subtype) with missing data across datasets
distr_withd_chars_NA <- as.matrix(summary(subset(chars_NA_subtype, subtype == "Withdrawals")[, 3]))

# Bring together
distr_chars_subtype_NA <- as.data.frame(rbind(distr_parti_chars_NA, distr_treat_chars_NA, distr_outco_chars_NA,
                                              distr_design_chars_NA, distr_setting_chars_NA, distr_rob_chars_NA, distr_withd_chars_NA))
distr_chars_subtype_NA$statistic <- rep(c("min", "q1", "median", "mean", "q3", "max"), 7)
distr_chars_subtype_NA$subtype <- rep(c("Participant", "Treatment", "Outcome",
                                        "Study design", "Study setting", "Risk of bias", "Withdrawals"),
                                      each = dim(distr_chars_subtype_NA)[1] / 7)
rownames(distr_chars_subtype_NA) <- NULL
colnames(distr_chars_subtype_NA)[1] <- "value"

# Add number of datasets below each box plot
num_datasets <- data.frame(subtype = c("Participant", "Treatment", "Outcome",
                                       "Study design", "Study setting", "Risk of bias", "Withdrawals"),
                           num = c(length(which(chars_NA_subtype$subtype == "Participant") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Treatment") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Outcome") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Study design") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Study setting") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Risk of bias") == TRUE),
                                   length(which(chars_NA_subtype$subtype == "Withdrawals") == TRUE)))
num_datasets$perc <- round((num_datasets$num / 217) * 100, 0)
num_datasets$subtype <- factor(num_datasets$subtype, levels = c("Participant", "Treatment", "Outcome",
                                                                "Study design", "Study setting", "Risk of bias", "Withdrawals"))



## Obtain box plots with integrated dots ----
# *Clinical* subtypes
plot_clin <-
  ggplot(subset(chars_NA_subtype, is.element(subtype, c("Participant", "Treatment", "Outcome"))),
         aes(x = subtype,
             y = perc,
             color = subtype)) +
  geom_boxplot(size = 1,
               width = 0.65,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "median" & is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q2 = ", sprintf("%0.1f", value))),
            hjust = -1.5,
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "q1" & is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q1 = ", sprintf("%0.1f", value)),
                hjust = ifelse(value < 10, -1.7, -1.5)),
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "q3" & is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q3 = ", sprintf("%0.1f", value))),
            hjust = -1.5,
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "min" & is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = value,
                label = paste0("minimum = ", sprintf("%0.1f", value))),
            hjust = 0.5,
            vjust = 2.3,
            size = 2.6,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "max" & is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = value,
                label = paste0("maximum = ", sprintf("%0.1f", value))),
            hjust = 0.5,
            vjust = -1.5,
            size = 2.6,
            inherit.aes = FALSE) +
  geom_text(data = subset(num_datasets, is.element(subtype, c("Participant", "Treatment", "Outcome"))),
            aes(x = subtype,
                y = 0,
                label = paste0(num, " (", perc, "%) datasets")),
            hjust = 0.5,
            vjust = 2.5,
            colour = "grey45",
            size = 3.5,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(y = "Percentage of characteristics with missing values (%)",
       x = "Clinical characteristic subtypes",
       color =  "Clinical characteristic subtypes") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

# *Methodological* subtypes
plot_metho <-
  ggplot(subset(chars_NA_subtype, is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
         aes(x = subtype,
             y = perc,
             color = subtype)) +
  geom_boxplot(size = 1,
               width = 0.57,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "median" & is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q2 = ", sprintf("%0.1f", value)),
                hjust = ifelse(value < 10, -1.16, -1.0)),
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "q1" & is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q1 = ", sprintf("%0.1f", value)),
                hjust = ifelse(value < 10, -1.16, -1.0)),
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "q3" & is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = value,
                label = paste0("Q3 = ", sprintf("%0.1f", value))),
            hjust = -1.0,
            size = 2.6,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "min" & is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = value,
                label = paste0("minimum = ", sprintf("%0.1f", value))),
            hjust = 0.5,
            vjust = 2.3,
            size = 2.6,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_chars_subtype_NA, statistic == "max" & is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = value,
                label = paste0("maximum = ", sprintf("%0.1f", value))),
            hjust = 0.5,
            vjust = -1.5,
            size = 2.6,
            inherit.aes = FALSE) +
  geom_text(data = subset(num_datasets, is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
            aes(x = subtype,
                y = 0,
                label = paste0(num, " (", perc, "%) datasets")),
            hjust = 0.5,
            vjust = 2.5,
            colour = "grey45",
            size = 3.5,
            inherit.aes = FALSE) +
  scale_color_manual(values = c("#E7298A", "#66A61E", "#E6AB02", "#A6761D")) +
  labs(y = " ",
       x = "Methodological characteristic subtypes",
       color =  "Methodological characteristic subtypes") +
  ylim(0, 100) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

# Bring together
tiff("./Figures/Figure S2.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggarrange(plot_clin, plot_metho,
          labels = c("a)", "b)"))
dev.off()
