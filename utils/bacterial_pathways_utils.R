# utils/bacterial_pathways_utils.R

transform_and_subset_data <- function(raw_data, pathway_labels_filter, pathway_species_filter, rescaling) {
  df_subset <- raw_data %>%
    filter(pathway_label %in% pathway_labels_filter, pathway_species %in% pathway_species_filter) %>%
    mutate(abundance = as.numeric(abundance))
  
  if (rescaling == "on") {
    df_subset <- df_subset %>%
      mutate(abundance = log1p(scale(abundance)))
  }
  
  unique_pathways <- unique(df_subset$pathway)
  pathway_labels <- paste0("pw", seq_along(unique_pathways))
  
  withr::with_seed(seed_to_use, {
    pathway_colours <- generate_colours("colour_group_1", length(unique_pathways), 200)
  })
  
  pathway_mapping_df <- tibble(
    Original = unique_pathways,
    Label = pathway_labels,
    Colour = pathway_colours
  )
  
  df_subset <- df_subset %>%
    left_join(pathway_mapping_df, by = c("pathway" = "Original")) %>%
    mutate(
      selection_label = Label,
      pathway_colour = Colour,
      host_group_colour = case_when(
        host_group == "Wild" ~ "#4578BC",
        host_group == "SPF" ~ "#F37C79",
        host_group == "Human" ~ "#138140"
      )
    ) %>%
    select(-Label, -Colour)
  
  for (col in c("geographic_location", "sampling_location", "young_adult")) {
    unique_values <- unique(df_subset[[col]])
    colours <- generate_colours(ifelse(col == "geographic_location", "colour_group_2", 
                                       ifelse(col == "sampling_location", "colour_group_3", "colour_group_4")),
                                length(unique_values), 50)
    colour_df <- tibble(Original = unique_values, Colour = colours)
    df_subset <- df_subset %>%
      left_join(colour_df, by = setNames("Original", col)) %>%
      mutate(!!paste0(col, "_colour") := Colour) %>%
      select(-Colour)
  }
  
  df_subset %>%
    arrange(pathway_code, pathway_species, host_group, geographic_location, sampling_location, young_adult, sample_id)
}