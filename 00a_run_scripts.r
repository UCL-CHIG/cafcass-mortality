
setwd("[omitted]")

local({
  r <- list("cran" = "[omitted]")
  options(repos = r)
})

library(data.table)
library(lubridate)
library(epitools)
library(tableone)
library(tidycmprsk)
library(RMySQL)

rm(list = ls())

cafcass_data_dir <- "[omitted]"
del_data_dir <- "[omitted]"

source("scripts/00b_functions.r")
source("scripts/01_load_deliveries.r")
source("scripts/02a_load_linkage_spine.r")
source("scripts/02b_deduplicate_person_id.r")
source("scripts/02c_deduplicate_tokenid.r")
source("scripts/02d_link.r")
source("scripts/03_get_case_data.r")
source("scripts/04_exclusions.r")
source("scripts/05_data_prep.r")
source("scripts/06_get_pre_court_phenotypes.r")
source("scripts/07_save.r")


load("processed/deliveries.rda")

source("scripts/08_descriptive_stats.r")
source("scripts/09_mortality_following_first_birth.r")
source("scripts/10a_code_cause_of_death.r")
source("scripts/10b_analyse_cause_of_death.r")
source("scripts/11a_prep_mortality_following_proceedings.r")
source("scripts/11b_mortality_following_proceedings.r")
source("scripts/12_additional_analyses.r")