# global.R

# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(heatmaply)
library(grDevices)
library(tibble)
library(tidyr)
library(kableExtra)
library(dplyr)
library(GiNA)
library(markdown)
library(leaflet)
library(purrr)
library(ggplot2)
library(openxlsx)
library(stringr)
library(leafpop)
library(htmlwidgets)
library(qs)

# Load data
bacterial_pathway_frame <- qs::qread("data/bacterial_pathways.qs")
taxonomy_16s_frame <- qs::qread("data/taxonomy_16S.qs")
taxonomy_meta_frame <- qread("data/taxonomy_meta.qs")
metabolites_frame <- qread(("data/metabolites.qs"))

# Set seed for reproducibility
seed_to_use <- 77