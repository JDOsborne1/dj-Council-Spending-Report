# Master Script
# Can migrate to function in future? 

library(drake)
library(dplyr)
library(ggplot2)
library(tools)
library(rmarkdown)
library(here)
library(forcats)
library(tibble)
library(tidyr)
library(stringr)
library(ggthemes)

source(here("R/extract_functions.R"))
source(here("R/aggregation_functions.R"))
source(here("R/plotting_functions.R"))
source(here("R/refinement_functions.R"))
source(here("R/reference_functions.R"))
source(here("R/fuzzy_matching_functions.R"))
source(here("R/utility_functions.R"))


link_list <- readd(refined_links)$link

source(here("R/plan.R"))


drake_config(full_plan)