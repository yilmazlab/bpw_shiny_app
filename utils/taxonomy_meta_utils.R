# taxonomy_meta_utils.R

transform_and_subset_data_meta <- function(raw_data, taxonomic_level_filter, taxonomy_filter = NULL,
                                           location_filter = NULL, group_filter = NULL, sampling_location_filter = NULL,
                                           gender_filter = NULL, age_filter = NULL, young_adult_filter = NULL) {
  level_mapping <- c(Phylum = "Phylum", Class = "Class", Order = "Order", Family = "Family", Genus = "Genus", Species = "Species")
  data_level <- level_mapping[taxonomic_level_filter]
  
  df_subset_meta <- raw_data %>%
    filter(taxonomic_level == data_level)
  
  filters <- list(
    taxonomy = taxonomy_filter, Location = location_filter, Group = group_filter,
    SamplingLocation = sampling_location_filter, Gender = gender_filter, Age = age_filter, Young_Adult = young_adult_filter
  )
  
  for (col in names(filters)) {
    if (!is.null(filters[[col]]) && length(filters[[col]]) > 0) {
      df_subset_meta <- df_subset_meta %>% filter(!!sym(col) %in% filters[[col]])
    }
  }
  
  df_subset_meta <- df_subset_meta %>%
    arrange(Location, Group, SamplingLocation, Gender, Young_Adult, Age) %>%
    mutate(Sample = factor(Sample, levels = unique(Sample)))
  
  unique_taxa <- unique(df_subset_meta$taxonomy)
  
  withr::with_seed(seed_to_use, {
    taxonomy_meta_colours <- generate_colours("colour_group_1", length(unique_taxa), 200)
  })
  
  taxonomy_meta_mapping_df <- tibble(
    taxonomy = unique_taxa,
    taxonomy_meta_colour = taxonomy_meta_colours
  )
  
  df_subset_meta %>%
    left_join(taxonomy_meta_mapping_df, by = "taxonomy")
}