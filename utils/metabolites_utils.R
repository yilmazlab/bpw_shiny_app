# metabolites_utils.R

library(dplyr)
library(tibble)
library(withr)

# Function to generate colors
generate_colors_metabolites <- function(type, size, n) {
  if (type == "metabolite") {
    color_list <- c(
      "#36648B", "#CD4F39", "#CDBA96", "#CD96CD", "#00868B", 
      "#473C8B", "#698B22", "#CD8500", "#8B0A50", "#00008B", 
      "#8B2323", "#458B00"
    )
  }
  
  if (type == "geographic") {
    color_list <- c(
      "#48D1CC", "#9400D3", "#9A0000", "#7CFC00", "#0000CD", "#008B8B"
    )
  }
  
  if (type == "metabolite_group") {
    color_list <- c(
      "#BDB76B", "#E9967A", "#90EE90", "#FFA07A", "#20B2AA", 
      "#BA55D3", "#FF6347", "#4682B4", "#00FA9A"
    )
  }
  
  if (size <= length(color_list)) {
    # If size is less than or equal to the number of predefined colors, use unique colors
    return(sample(color_list, size = size))
  } else {
    # If size is greater, use color ramp
    color_generator <- colorRampPalette(color_list)
    return(sample(color_generator(n), size = size, replace = TRUE))
  }
}

# Set a seed for reproducibility
seed_to_use <- 77

# Function to transform and subset data
transform_and_subset_data_metabolites <- function(
    raw_data, 
    metabolite_group_filter, 
    metabolite_name_filter, 
    rescaling
) {
  df_subset_metabolites <- raw_data %>%
    filter(metabolite_group %in% metabolite_group_filter,
           metabolite_name %in% metabolite_name_filter)
  
  if (rescaling == "on") {
    df_subset_metabolites <- df_subset_metabolites %>%
      group_by(metabolite_name) %>%
      mutate(metabolite_abundancy = scale(metabolite_abundancy)) %>%
      ungroup()
  }
  
  # Generate colors for metabolites
  unique_metabolites <- levels(df_subset_metabolites$metabolite_name)
  
  withr::with_seed(seed_to_use, {
    metabolite_colors <- generate_colors_metabolites(
      type = "metabolite", 
      size = length(unique_metabolites), 
      n = 200
    )
  })
  
  metabolite_mapping_df <- tibble(
    Original = unique_metabolites,
    Color = metabolite_colors
  )
  
  df_subset_metabolites <- df_subset_metabolites %>%
    left_join(metabolite_mapping_df, by = c("metabolite_name" = "Original")) %>%
    rename(metabolite_color = Color)
  
  # Generate colors for geographic locations
  unique_geographic_locations <- levels(df_subset_metabolites$geographic_location)
  
  geographic_location_colors_metabolites <- generate_colors_metabolites(
    type = "geographic", 
    size = length(unique_geographic_locations), 
    n = 50
  )
  
  geographic_location_df <- tibble(
    Original = unique_geographic_locations,
    Color = geographic_location_colors_metabolites
  )
  
  df_subset_metabolites <- df_subset_metabolites %>%
    left_join(geographic_location_df, by = c("geographic_location" = "Original")) %>%
    rename(geographic_location_color_metabolites = Color)
  
  # Generate colors for metabolite groups
  unique_metabolite_groups <- levels(df_subset_metabolites$metabolite_group)
  
  metabolite_group_colors <- generate_colors_metabolites(
    type = "metabolite_group", 
    size = length(unique_metabolite_groups), 
    n = 20
  )
  
  metabolite_group_df <- tibble(
    Original = unique_metabolite_groups,
    Color = metabolite_group_colors
  )
  
  df_subset_metabolites <- df_subset_metabolites %>%
    left_join(metabolite_group_df, by = c("metabolite_group" = "Original")) %>%
    rename(metabolite_group_color = Color)
  
  # Arrange the data
  df_subset_metabolites <- df_subset_metabolites %>%
    arrange(metabolite_group, metabolite_name, geographic_location, sample_id)
  
  df_subset_metabolites
}