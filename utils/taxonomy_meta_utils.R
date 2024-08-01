# taxonomy_meta_utils.R

library(dplyr)
library(tibble)
library(withr)

# Function to generate colors (reusing the same function from taxonomy_16S_utils.R)
generate_colors_meta <- function(n) {
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
transform_and_subset_data_meta <- function(
    raw_data, 
    taxonomic_level_filter,
    taxonomy_filter = NULL,
    location_filter = NULL,
    group_filter = NULL,
    sampling_location_filter = NULL,
    gender_filter = NULL,
    age_filter = NULL,
    young_adult_filter = NULL
) {
  # Create a mapping between UI choices and data frame values
  level_mapping <- c(
    "Phylum" = "Phylum",
    "Class" = "Class",
    "Order" = "Order",
    "Family" = "Family",
    "Genus" = "Genus",
    "Species" = "Species"
  )
  
  # Use the mapping to get the correct level for filtering
  data_level <- level_mapping[taxonomic_level_filter]
  
  # Filter by taxonomic level
  df_subset_meta <- raw_data %>%
    filter(taxonomic_level == data_level)
  
  # Apply additional filters
  filters <- list(
    taxonomy = taxonomy_filter,
    Location = location_filter,
    Group = group_filter,
    SamplingLocation = sampling_location_filter,
    Gender = gender_filter,
    Age = age_filter,
    Young_Adult = young_adult_filter
  )
  
  for (col in names(filters)) {
    if (!is.null(filters[[col]]) && length(filters[[col]]) > 0) {
      df_subset_meta <- df_subset_meta %>% filter(!!sym(col) %in% filters[[col]])
    }
  }
  
  # Reorder the data frame based on the grouping variables
  df_subset_meta <- df_subset_meta %>%
    arrange(Location, Group, SamplingLocation, Gender, Young_Adult, Age)
  
  # Reorder the Sample factor levels according to the sorted data frame
  df_subset_meta$Sample <- factor(df_subset_meta$Sample, levels = unique(df_subset_meta$Sample))
  
  # Generate colors for taxa
  unique_taxa <- unique(df_subset_meta$taxonomy)
  
  withr::with_seed(seed_to_use, {
    taxonomy_meta_colors <- generate_colors_meta(length(unique_taxa))
  })
  
  taxonomy_meta_mapping_df <- tibble(
    taxonomy = unique_taxa,
    taxonomy_meta_color = taxonomy_meta_colors
  )
  
  df_subset_meta %>%
    left_join(taxonomy_meta_mapping_df, by = "taxonomy")
}