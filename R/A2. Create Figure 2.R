#*******************************************************************************
#*
#*
#*                     Creating Figure 2 of the manuscript
#*       (Box plots with dots on *Clinical* characteristics per subtype;
#*        Box plots with dots on *Methodological* characteristics per subtype)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************


#' Note: Adding also a box plot on the number of characteristics regardless of
#' characteristic subtype would make the Figure S1 cluttered; hence, it was omitted.


## Load libraries ----
list.of.packages <- c("tracenma", "plyr", "dplyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of data-frames on the characteristic types and subtypes
subtype_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)



## Working on *Clinical* subtypes ----
# Distribution of number of *Participant* characteristics
num_parti_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Participant")))
distr_parti_chars <- as.matrix(summary(num_parti_chars[num_parti_chars > 0]))

# Distribution of number of *Intervention* characteristics
num_treat_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Intervention")))
distr_treat_chars <- as.matrix(summary(num_treat_chars[num_treat_chars > 0]))

# Distribution of number of *Outcome* characteristics
num_outco_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Outcome")))
distr_outco_chars <- as.matrix(summary(num_outco_chars[num_outco_chars > 0]))

# Bring together
distr_clin_subtype <- as.data.frame(rbind(distr_parti_chars, distr_treat_chars, distr_outco_chars))
distr_clin_subtype$statistic <- rep(c("min", "q1", "median", "mean", "q3", "max"), 3)
distr_clin_subtype$subtype <- rep(c("Participant", "Intervention", "Outcome"),
                                  each = dim(distr_clin_subtype)[1] / 3)
rownames(distr_clin_subtype) <- NULL
colnames(distr_clin_subtype)[1] <- "value"
distr_clin_subtype$subtype <- revalue(distr_clin_subtype$subtype, c("Intervention" = "Treatment"))
distr_clin_subtype[distr_clin_subtype$statistic == "q1" & is.element(distr_clin_subtype$subtype, c("Treatment", "Outcome")), ] <- NA



## Working on *Methodological* subtypes ----
# Distribution of number of *Study design* characteristics
num_design_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Study design")))
distr_design_chars <- as.matrix(summary(num_design_chars[num_design_chars > 0]))

# Distribution of number of *Study setting* characteristics
num_setting_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Study setting")))
distr_setting_chars <- as.matrix(summary(num_setting_chars[num_setting_chars > 0]))

# Distribution of number of *Risk of bias* characteristics
num_rob_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Risk of bias")))
distr_rob_chars <- as.matrix(summary(num_rob_chars[num_rob_chars > 0]))

# Distribution of number of *Withdrawals* characteristics
num_withd_chars <- sapply(subtype_chars, function(x) length(which(x[, 3] == "Withdrawals")))
distr_withd_chars <- as.matrix(summary(num_withd_chars[num_withd_chars > 0]))

# Bring together
distr_metho_subtype <- as.data.frame(rbind(distr_design_chars, distr_setting_chars, distr_rob_chars, distr_withd_chars))
distr_metho_subtype$statistic <- rep(c("min", "q1", "median", "mean", "q3", "max"), 4)
distr_metho_subtype$subtype <- rep(c("Study design", "Study setting", "Risk of bias", "Withdrawals"),
                                   each = dim(distr_metho_subtype)[1] / 4)
rownames(distr_metho_subtype) <- NULL
colnames(distr_metho_subtype)[1] <- "value"
distr_metho_subtype[distr_metho_subtype$statistic == "q1" & is.element(distr_metho_subtype$subtype, c("Study design", "Study setting")), ] <- NA
distr_metho_subtype[is.element(distr_metho_subtype$statistic, c("q1", "q3")) & distr_metho_subtype$subtype == "Withdrawals", ] <- NA



## % of datasets per number of characteristics for each subtype ----
# Number of characteristic per subtype in each dataset
num_chars_subtype <- lapply(subtype_chars, function(x) as.data.frame(table(x[, 3])))

# Turn into a data-frame
dataset_chars_subtype <- do.call(rbind, num_chars_subtype)
colnames(dataset_chars_subtype) <- c("subtype", "frequency")
dataset_chars_subtype$subtype <- factor(dataset_chars_subtype$subtype,
                                        levels = c("Participant", "Intervention", "Outcome",
                                                   "Age", "Sex", "Ethnicity",
                                                    "Study design", "Study setting", "Risk of bias", "Withdrawals"))
dataset_chars_subtype$subtype <- revalue(dataset_chars_subtype$subtype, c("Intervention" = "Treatment"))

# Add number of datasets below each box plot
num_datasets <- data.frame(subtype = c("Participant", "Treatment", "Outcome",
                                       "Study design", "Study setting", "Risk of bias", "Withdrawals"),
                           num = c(dim(subset(dataset_chars_subtype, subtype == "Participant"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Treatment"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Outcome"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Study design"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Study setting"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Risk of bias"))[1],
                                   dim(subset(dataset_chars_subtype, subtype == "Withdrawals"))[1]))
num_datasets$perc <- round((num_datasets$num / 217) * 100, 0)
num_datasets$subtype <- factor(num_datasets$subtype, levels = c("Participant", "Treatment", "Outcome",
                                                                "Study design", "Study setting", "Risk of bias", "Withdrawals"))



## Box plots with integrated dots ----
# Clinical subtypes
boxplot_clin <-
  ggplot(subset(dataset_chars_subtype, is.element(subtype, c("Participant", "Treatment", "Outcome"))),
         aes(x = subtype,
             y = frequency,
             colour = subtype)) +
  geom_boxplot(size = 1,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_clin_subtype, statistic == "median"),
            aes(x = subtype,
                y = value,
                label = paste0("Q2 = ", value) ),
            hjust = -2.2,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_clin_subtype, statistic == "q1"),
            aes(x = subtype,
                y = value,
                label = paste0("Q1 = ", value) ),
            hjust = -2.2,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_clin_subtype, statistic == "q3"),
            aes(x = subtype,
                y = value,
                label = paste0("Q3 = ", value) ),
            hjust = -2.2,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_clin_subtype, statistic == "min"),
            aes(x = subtype,
                y = value,
                label = paste0("minimum = ", value) ),
            hjust = 0.5,
            vjust = 2.8,
            size = 3,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_clin_subtype, statistic == "max"),
            aes(x = subtype,
                y = value,
                label = paste0("maximum = ", value) ),
            hjust = 0.5,
            vjust = -2.2,
            size = 3,
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
  labs(y = "Number of characteristics",
       x = "Clinical characteristic subtypes",
       color =  "Characteristic subtypes") +
  ylim(0, 22) +
  theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))

# Methodological subtypes
boxplot_metho <-
  ggplot(subset(dataset_chars_subtype, is.element(subtype, c("Study design", "Study setting", "Risk of bias", "Withdrawals"))),
         aes(x = subtype,
             y = frequency,
             colour = subtype)) +
  geom_boxplot(size = 1,
               outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 1.3,
             alpha = .4) +
  geom_text(data = subset(distr_metho_subtype, statistic == "median"),
            aes(x = subtype,
                y = value,
                label = paste0("Q2 = ", value) ),
            hjust = -1.65,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_metho_subtype, statistic == "q1"),
            aes(x = subtype,
                y = value,
                label = paste0("Q1 = ", value) ),
            hjust = -1.65,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_metho_subtype, statistic == "q3"),
            aes(x = subtype,
                y = value,
                label = paste0("Q3 = ", value) ),
            hjust = -1.65,
            size = 3,
            inherit.aes = FALSE,
            check_overlap = TRUE) +
  geom_text(data = subset(distr_metho_subtype, statistic == "min"),
            aes(x = subtype,
                y = value,
                label = paste0("minimum = ", value) ),
            hjust = 0.5,
            vjust = 2.8,
            size = 3,
            inherit.aes = FALSE) +
  geom_text(data = subset(distr_metho_subtype, statistic == "max"),
            aes(x = subtype,
                y = value,
                label = paste0("maximum = ", value) ),
            hjust = 0.5,
            vjust = -2.2,
            size = 3,
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
       color =  "Characteristic subtypes") +
  ylim(0, 22) +
  theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))


# Bring all together
tiff("./30_Analysis & Results/Figure 2.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggarrange(boxplot_clin, boxplot_metho,
          nrow = 1,
          labels = c("a)", "b)"),
          legend = "none")
dev.off()
