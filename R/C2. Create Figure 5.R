#*******************************************************************************
#*
#*
#*                     Creating Figure 5 of the manuscript
#*   (A) Commonness of unique combinations among characteristic subtypes
#*       for datasets that include all three characteristic types;
#*   (B) Bar plots on frequency of all combinations of characteristic subtypes
#*       for datasets that include all three characteristic types
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the development version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries ----
list.of.packages <- c("tracenma", "plyr", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of data-frames on the characteristic types and subtypes
type_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)

# Number of characteristics per subtype in each dataset
chars_subtype_dataset <- lapply(type_chars, function(x) as.data.frame(table(x[, 3])))



## Distribution of subtypes in the datasets that include all characteristics types ----
# Characteristics types in each dataset
chars_type_vector <- lapply(type_chars, function(x) as.character(as.data.frame(table(x[, 2]))[, 1]))

# Vector of characteristic types
unique_types <- sort(unique(unlist(chars_type_vector)))

# Find id number of these datasets
id_datasets_alltypes <- which(unlist(lapply(chars_type_vector, function(x) length(match(x, unique_types)))) == 3)

# Unique characteristic subtypes in each dataset
chars_subtype_unique <- lapply(type_chars, function(x) as.character(as.data.frame(table(x[, 3]))[, 1]))

# Keeping only those datasets with the id number as retrieved above (id_datasets_alltypes)
chars_subtype_unique_restrict <- chars_subtype_unique[id_datasets_alltypes]
length(chars_subtype_unique_restrict) # 152 datasets

# Vector of characteristic subtypes
unique_subtypes <- sort(unique(unlist(chars_subtype_unique)))

# List of *possible* combinations of characteristic subtypes
possible_combinations <- do.call("c", lapply(seq_along(unique_subtypes), function(i) combn(unique_subtypes, i, FUN = list)))
length(possible_combinations) # 1023 possible combinations among the 10 subtypes

# Frequency of each unique *observed* combination
num_unique_comb <- as.data.frame(table(unlist(lapply(chars_subtype_unique_restrict, paste, collapse = " & "))))
colnames(num_unique_comb)[1] <- "combination"
num_unique_comb[order(num_unique_comb$Freq, decreasing = TRUE), ]
dim(num_unique_comb)[1] # 65 observed unique combinations

# Number of combinations appearing only once in the database
length(which(num_unique_comb$Freq == 1)) # 37 (37/65 = 57%)

# Summary of frequency of the remaining 28 (=65-37) combinations appear
summary(num_unique_comb$Freq[which(num_unique_comb$Freq > 1)])

# *Observed* combinations
observed_subtypes_comb <- chars_subtype_unique_restrict[!duplicated(chars_subtype_unique_restrict)]

# Number of observed combinations that contain each subtype
num_subtype_observed_comb <- lapply(unique_subtypes, function(y) length(unlist(lapply(observed_subtypes_comb, function(x) which(x == y)))))

# Bring together in a data-frame
num_subset_comb_dataset0 <- data.frame(subtype = unique_subtypes, num = unlist(num_subtype_observed_comb))
num_subset_comb_dataset0$subtype <- factor(num_subset_comb_dataset0$subtype,
                                           levels = c("Participant", "Intervention", "Outcome",
                                                      "Age", "Sex", "Ethnicity",
                                                      "Study design", "Study setting", "Risk of bias", "Withdrawals"))
num_subset_comb_dataset <- with(num_subset_comb_dataset0, num_subset_comb_dataset0[order(subtype, num), ])
num_subset_comb_dataset$perc <- round((num_subset_comb_dataset$num / length(observed_subtypes_comb)) * 100, 0)
num_subset_comb_dataset$type <- rep(c("Clinical", "Demographic", "Methodological"), c(3, 3, 4))
num_subset_comb_dataset$subtype <- revalue(num_subset_comb_dataset$subtype, c("Intervention" = "Treatment"))

# Grouped bar plot (152 datasets)
tiff("./Figures/Figure 5.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(num_subset_comb_dataset,
       aes(x = subtype,
           y = perc,
           fill = type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = subtype,
                y = perc,
                label = paste0(perc, "%", " ", "(", num, ")") ),
            vjust = -0.3,
            size = 4) +
  scale_fill_manual(breaks = c("Clinical", "Demographic", "Methodological"),
                    values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Characteristic subtypes",
       y = "Percentage of combinations with the subtype (%)",
       fill = "Characteristic types") +
  ggtitle("Restricting to 152 (70%) datasets containing all characteristic types") +
  ylim(0, 100) +
  theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))
dev.off()
