# taxonomy_16S_utils.R

transform_and_subset_data_16s <- function(raw_data, taxonomic_level_filter, taxonomy_filter = NULL,
                                          origin_filter = NULL, group_filter = NULL, sample_type_filter = NULL,
                                          gender_filter = NULL, age_filter = NULL, season_filter = NULL) {
  df_subset_16s <- raw_data %>%
    filter(taxonomic_level == taxonomic_level_filter)
  
  filters <- list(
    taxonomy = taxonomy_filter, Origin = origin_filter, Group = group_filter,
    SampleType = sample_type_filter, Gender = gender_filter, Adult_Pups = age_filter, Season = season_filter
  )
  
  for (col in names(filters)) {
    if (!is.null(filters[[col]]) && length(filters[[col]]) > 0) {
      df_subset_16s <- df_subset_16s %>% filter(!!sym(col) %in% filters[[col]])
    }
  }
  
  df_subset_16s <- df_subset_16s %>%
    arrange(Origin, Group, SampleType, Gender, Adult_Pups, Season) %>%
    mutate(SampleID = factor(SampleID, levels = unique(SampleID)))
  
  unique_taxa <- unique(df_subset_16s$taxonomy)
  
  withr::with_seed(seed_to_use, {
    taxonomy_16s_colours <- generate_colours("colour_group_1", length(unique_taxa), 200)
  })
  
  taxonomy_16s_mapping_df <- tibble(
    taxonomy = unique_taxa,
    taxonomy_16s_colour = taxonomy_16s_colours
  )
  
  df_subset_16s %>%
    left_join(taxonomy_16s_mapping_df, by = "taxonomy")
}