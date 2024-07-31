# taxonomy_16S_utils.R

library(dplyr)
library(tibble)
library(withr)

# Function to generate colors
generate_colors_16S <- function(n) {
  color_list <- c(
    "#36648B", "#CD4F39", "#CDBA96", "#CD96CD", "#00868B", 
    "#473C8B", "#698B22", "#CD8500", "#8B0A50", "#00008B", 
    "#8B2323", "#458B00", "#48D1CC", "#9400D3", "#9A0000", 
    "#7CFC00", "#0000CD", "#008B8B", "#BDB76B", "#E9967A"
  )
  
  if (n <= length(color_list)) {
    return(sample(color_list, size = n))
  } else {
    color_generator <- colorRampPalette(color_list)
    return(color_generator(n))
  }
}

# Set a seed for reproducibility
seed_to_use <- 77

# Function to transform and subset data
transform_and_subset_data_16S <- function(
    raw_data, 
    taxonomic_level_filter,
    taxonomy_filter = NULL,
    origin_filter = NULL,
    group_filter = NULL,
    sample_type_filter = NULL,
    gender_filter = NULL,
    age_filter = NULL,
    season_filter = NULL
) {
  # Filter by taxonomic level
  df_subset_16S <- raw_data %>%
    filter(taxonomic_level == taxonomic_level_filter)
  
  # Apply additional filters
  filters <- list(
    taxonomy = taxonomy_filter,
    Origin = origin_filter,
    Group = group_filter,
    SampleType = sample_type_filter,
    Gender = gender_filter,
    Adult_Pups = age_filter,
    Season = season_filter
  )
  
  for (col in names(filters)) {
    if (!is.null(filters[[col]]) && length(filters[[col]]) > 0) {
      df_subset_16S <- df_subset_16S %>% filter(!!sym(col) %in% filters[[col]])
    }
  }
  
  # Reorder the data frame based on the grouping variables
  df_subset_16S <- df_subset_16S %>%
    arrange(Origin, Group, SampleType, Gender, Adult_Pups, Season)
  
  # Reorder the SampleID factor levels according to the sorted data frame
  df_subset_16S$SampleID <- factor(df_subset_16S$SampleID, levels = unique(df_subset_16S$SampleID))
  
  # Generate colors for taxa
  unique_taxa <- unique(df_subset_16S$taxonomy)
  
  withr::with_seed(seed_to_use, {
    taxonomy_16S_colors <- generate_colors_16S(length(unique_taxa))
  })
  
  taxonomy_16S_mapping_df <- tibble(
    taxonomy = unique_taxa,
    taxonomy_16S_color = taxonomy_16S_colors
  )
  
  df_subset_16S %>%
    left_join(taxonomy_16S_mapping_df, by = "taxonomy")
}