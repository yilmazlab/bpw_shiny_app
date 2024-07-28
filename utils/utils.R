
generate_colors <- function(type, size, n) {
  if (type == "pathway") {
    color_list <- c(
      "#36648B",
      "#CD4F39",
      "#CDBA96",
      "#CD96CD",
      "#00868B",
      "#473C8B",
      "#698B22",
      "#CD8500",
      "#8B0A50",
      "#00008B",
      "#8B2323",
      "#458B00"
    )
  }

  if (type == "geographic") {
    color_list <- c(
      "#48D1CC",
      "#9400D3",
      "#9A0000",
      "#7CFC00",
      "#0000CD",
      "#008B8B"
    )
  }

  if (type == "location") {
    color_list <- c("#BDB76B", "#E9967A", "#90EE90")
  }

  if (type == "young_adult") {
    color_list <- c("#8B4513", "#B0E0E6", "#EE82EE")
  }

  color_generator <- colorRampPalette(
    color_list
  )

  sample(color_generator(n), size = size, replace = TRUE)
}


transform_and_subset_data <- function(
    raw_data,
    pathway_labels_filter,
    pathway_species_filter,
    rescaling
  ) {

  df_subset <- raw_data |>
    dplyr::filter(pathway_label %in% pathway_labels_filter) |>
    dplyr::filter(pathway_species %in% pathway_species_filter)

  df_subset <- df_subset |>
    dplyr::mutate(abundance = as.numeric(abundance))

  if (rescaling == "on") {
    df_subset <- df_subset |>
      dplyr::mutate(
        abundance = log1p(scale(log1p(abundance)))
      )
  }

  unique_pathways <- unique(df_subset$pathway)

  pathway_labels <- paste0("pw", seq_along(unique_pathways))

  seed_to_use <- 77

  withr::with_seed(seed_to_use, {
    pathway_colors <- generate_colors(
      type = "pathway",
      size = length(unique_pathways),
      n = 200
    )
  })

  pathway_mapping_df <- tibble::tibble(
    Original = unique_pathways,
    Label = pathway_labels,
    Color = pathway_colors
  )

  df_subset <- df_subset |>
    dplyr::left_join(pathway_mapping_df, by = c("pathway" = "Original")) |>
    dplyr::mutate(selection_label = Label, pathway_color = Color) |>
    dplyr::select(-Label, -Color)

  df_subset <- df_subset |> dplyr::mutate(
    host_group_color = dplyr::case_when(
      host_group == "Wild" ~ "#4578BC",
      host_group == "SPF" ~ "#F37C79",
      host_group == "Human" ~ "#138140"
    ))

  unique_geographic_locations <- unique(df_subset$geographic_location)

  geographic_location_colors <- generate_colors(
    type = "geographic",
    size = length(unique_geographic_locations),
    n = 50
  )

  geographic_location_df <- tibble::tibble(
    Original = unique_geographic_locations,
    Color = geographic_location_colors
  )

  df_subset <- df_subset |>
    dplyr::left_join(geographic_location_df, by = c("geographic_location" = "Original")) |>
    dplyr::mutate(geographic_location_color = Color) |>
    dplyr::select(-Color)

  unique_sampling_location <- unique(df_subset$sampling_location)

  withr::with_seed(seed_to_use, {
    sampling_location_colors <- generate_colors(
      type = "location",
      size = length(unique_sampling_location),
      n = 5
    )
  })

  sampling_location_df <- tibble::tibble(
    Original = unique_sampling_location,
    Color = sampling_location_colors
  )

  df_subset <- df_subset |>
    dplyr::left_join(sampling_location_df, by = c("sampling_location" = "Original")) |>
    dplyr::mutate(sampling_location_color = Color) |>
    dplyr::select(-Color)

  unique_young_adult <- unique(df_subset$young_adult)

  withr::with_seed(seed_to_use, {
    young_adult_colors <- generate_colors(
      type = "young_adult",
      size = length(unique_young_adult),
      n = 5
    )
  })

  young_adult_df <- tibble::tibble(
    Original = unique_young_adult,
    Color = young_adult_colors
  )

  df_subset <- df_subset |>
    dplyr::left_join(young_adult_df, by = c("young_adult" = "Original")) |>
    dplyr::mutate(young_adult_color = Color) |>
    dplyr::select(-Color)


  df_subset <- df_subset |>
    dplyr::arrange(
      pathway_code,
      pathway_species,
      host_group,
      geographic_location,
      sampling_location,
      young_adult,
      sample_id
    )

  df_subset

}



convert_coordinates <- function(coord_str, which) {

  latitude <- as.numeric(stringr::str_match(coord_str, "([-+]?[0-9]*\\.?[0-9]+)° [NS]")[,2])
  longitude <- as.numeric(stringr::str_match(coord_str, "([-+]?[0-9]*\\.?[0-9]+)° [EW]")[,2])

  if (stringr::str_detect(coord_str, "S")) {
    latitude <- latitude * -1
  }
  if (stringr::str_detect(coord_str, "W")) {
    longitude <- longitude * -1
  }

  if (which == "lat") {
    return(latitude)
  }

  if (which == "lon") {
    return(longitude)
  }

}


read_and_transform_taxonomy_xlsx <- function(file_path, sheet = 1) {

  taxonomy_df <- openxlsx::read.xlsx(file_path, sheet = sheet)

  taxonomy_df_final <- taxonomy_df %>%
    tidyr::pivot_longer(-Origin, names_to = "Location", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = Origin, values_from = Value) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      latitude = unlist(lapply(coordinates, convert_coordinates, which = "lat")),
      longitude = unlist(lapply(coordinates, convert_coordinates, which = "lon"))
    ) %>%
    tidyr::pivot_longer(
      -c(location, group, country, coordinates, latitude, longitude),
      names_to = "taxo_name",
      values_to = "taxo_value"
    ) %>%
    dplyr::mutate(
      taxo_value = as.numeric(taxo_value)
    ) %>%
    dplyr::mutate(
      taxo_value = round(x = taxo_value, digits = 4)
    ) %>%
    dplyr::arrange(location)

  taxonomy_df_final
}


generate_leaflet_plot_list <- function(data_to_consider) {

  p <- ggplot(data_to_consider, aes(location, y = taxo_value, fill = taxo_name)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(title = NULL)) +
    facet_wrap(~category) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  plotly::ggplotly(p)

}





