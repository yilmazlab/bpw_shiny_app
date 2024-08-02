# metabolites_utils.R

transform_and_subset_data_metabolites <- function(raw_data, metabolite_group_filter, metabolite_name_filter, rescaling) {
  df_subset_metabolites <- raw_data %>%
    filter(metabolite_group %in% metabolite_group_filter, metabolite_name %in% metabolite_name_filter)
  
  if (rescaling == "on") {
    df_subset_metabolites <- df_subset_metabolites %>%
      group_by(metabolite_name) %>%
      mutate(metabolite_abundancy = log1p(scale(metabolite_abundancy))) %>%
      ungroup()
  }
  
  unique_metabolites <- levels(df_subset_metabolites$metabolite_name)
  unique_geographic_locations <- levels(df_subset_metabolites$geographic_location)
  unique_metabolite_groups <- levels(df_subset_metabolites$metabolite_group)
  
  withr::with_seed(seed_to_use, {
    metabolite_colours <- generate_colours("colour_group_1", length(unique_metabolites), 200)
    geographic_location_colours_metabolites <- generate_colours("colour_group_2", length(unique_geographic_locations), 50)
    metabolite_group_colours <- generate_colours("colour_group_3", length(unique_metabolite_groups), 20)
  })
  
  colour_mappings <- list(
    metabolite_name = tibble(Original = unique_metabolites, Colour = metabolite_colours),
    geographic_location = tibble(Original = unique_geographic_locations, Colour = geographic_location_colours_metabolites),
    metabolite_group = tibble(Original = unique_metabolite_groups, Colour = metabolite_group_colours)
  )
  
  for (col in names(colour_mappings)) {
    df_subset_metabolites <- df_subset_metabolites %>%
      left_join(colour_mappings[[col]], by = setNames("Original", col)) %>%
      rename_with(~paste0(col, "_colour_metabolites"), .cols = "Colour")
  }
  
  df_subset_metabolites %>%
    arrange(metabolite_group, metabolite_name, geographic_location, sample_id)
}