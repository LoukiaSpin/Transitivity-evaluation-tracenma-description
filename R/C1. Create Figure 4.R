#*******************************************************************************
#*
#*
#*                     Creating Figure 4 of the manuscript
#*     (Bar plots on frequency of all combinations of characteristic types)
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load the development version of tracenma
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)



## Load libraries ----
list.of.packages <- c("tracenma", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# List of data-frames on the characteristic types and subtypes
type_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)

# Number of characteristics per type in each dataset
chars_type_dataset <- lapply(type_chars, function(x) as.data.frame(table(x[, 2])))



## Commonness of combinations among characteristic types ----
# Keep the first column with the names, turning each submatrix into vector
chars_type_vector <- lapply(chars_type_dataset, function(x) as.character(x[, 1]))

# Frequency of each unique *observed* combination
num_unique_comb <- table(unlist(lapply(chars_type_vector, paste, collapse = " & ")))

# Vector of characteristic types
unique_types <- sort(unique(unlist(chars_type_vector)))

# List of *possible* combinations of characteristic types
possible_combinations <- do.call("c", lapply(seq_along(unique_types), function(i) combn(unique_types, i, FUN = list)))
length(possible_combinations) # 7 possible combinations

# *Unobserved* combinations in the database
unobserved_comb <- unlist(lapply(setdiff(possible_combinations, chars_type_vector[!duplicated(chars_type_vector)]),
                                 paste, collapse = " & "))



## Prepare dataset for grouped bar plots ----
# Bring together in a data-frame
num_comb_dataset0 <- data.frame(num = c(num_unique_comb, rep(0, length(unobserved_comb))),
                                comb = c(names(num_unique_comb), unobserved_comb))
num_comb_dataset0$comb <- factor(num_comb_dataset0$comb,
                                 levels = c("Clinical & Demographic & Methodological", "Clinical & Demographic", "Clinical & Methodological", "Demographic & Methodological",
                                            "Clinical", "Demographic", "Methodological"))
num_comb_dataset <- with(num_comb_dataset0, num_comb_dataset0[order(comb, num), ])
num_comb_dataset$comb_type = c("All three", "Pair of two", "Pair of two", "Pair of two", "Only one", "Only one", "Only one")
num_comb_dataset$comb_type <- factor(num_comb_dataset$comb_type, levels = c("All three", "Pair of two", "Only one"))
num_comb_dataset$perc <- round((num_comb_dataset$num / 217) * 100, 1)

# Bar plot
tiff("./Figures/Figure 4.tiff",
     height = 20,
     width = 37,
     units = "cm",
     compression = "lzw",
     res = 300)
ggplot(num_comb_dataset,
       aes(x = comb,
           y = perc,
           fill = comb_type)) +
  geom_bar(stat = "identity",
           alpha = 0.7) +
  geom_text(aes(x = comb,
                y = perc,
                label = paste0(perc, "%", " ", "(", num, ")") ),
            vjust = -0.3,
            size = 4) +
  scale_fill_manual(breaks = c("All three", "Pair of two", "Only one"),
                    values = c("#1B9E77", "#D95F02","#7570B3")) +
  scale_x_discrete(labels = scales::label_wrap(16)) +
  labs(x = " ",#"Combinations of characteristic types",
       y = "Percentage of datasets (%)",
       fill = "Characteristic types included") +
  ylim(0, 100) +
  theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.margin = margin(-10, 0, 0, 0))
dev.off()

