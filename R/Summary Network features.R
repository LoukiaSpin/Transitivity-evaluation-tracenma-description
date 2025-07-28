#*******************************************************************************
#*
#*
#*                      Summarise network-related features
#*              Descriptive statistics of database characteristics
#*
#* Author: Loukia M. Spineli
#* Date: April 2024
#*******************************************************************************



## Load libraries ----
#remotes::install_github("https://github.com/LoukiaSpin/tracenma.git", force = TRUE)
library("tracenma")



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# Load all 217 datasets as data-frames
datasets <- lapply(pmid_index, function(x) as.data.frame(get.dataset(pmid = x)$Dataset))

# Data-frames on the characteristic types and subtypes
type_subtype_chars <-
  lapply(pmid_index, function(x) get.dataset(pmid = x, show.type = TRUE)$Characteristics_index)



## Summarise network characteristics ----
# Number of studies
num_studies <- sapply(datasets, function(x) length(unique(x[, 1])))
summary(num_studies)

# Number of interventions
num_interv <- sapply(datasets, function(x) length(unique(unlist(x[, 2:3]))))
summary(num_interv)
which(num_interv == 3) # One network with three interventions

# Percentage of observed comparisons
num_obs_comp <- sapply(datasets, function(x) length(unique(paste(x[, 3], "vs", x[, 2]))))
perc_obs_comp <- round((num_obs_comp /
                          sapply(num_interv, function(x) dim(combn(x, 2))[2])) * 100, 0)
summary(perc_obs_comp)
which(perc_obs_comp == 100) # Three fully connected networks
