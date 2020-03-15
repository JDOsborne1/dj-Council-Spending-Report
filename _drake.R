# Master Script
# Can migrate to function in future? 

library(drake)
library(dplyr)
library(ggplot2)
library(tools)


source(here::here("R/extract_functions.R"))


link_list <- readd(refined_links)$link

source(here::here("R/plan.R"))


drake_config(full_plan)