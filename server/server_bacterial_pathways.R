# server/server_bacterial_pathways.R

df_subset_reactive <- shiny::reactiveVal(tibble::tibble())

output$pathway_species_ui <- renderUI({
  req(input$pathway_label)
  df_subset <- bacterial_pathway_frame[bacterial_pathway_frame$pathway_label %in% input$pathway_label,]
  species_choices <- unique(df_subset$pathway_species)
  species_choices <- c("Sample community", species_choices[species_choices != "Sample community"])
  
  pickerInput(
    inputId = "pathway_species",
    label = "Select species",
    choices = species_choices,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      multiple = TRUE
    ),
    selected = "Sample community",
    multiple = TRUE
  )
})

observeEvent(input$apply, {
  # Generate the title based on the rescaling input
  title <- if (input$rescaling == "off") {
    "Relative pathway abundance"
  } else {
    "Sequential log(1 + x)-standardisation rescaled relative pathway abundance"
  }
  output$visualisation_output <- renderUI({
    output_plots <- tagList()
    input_visualisation_bpw <- shiny::isolate(input$visualisation)
    
    output_plots <- tagAppendChild(output_plots, h3(title, style = "text-align: center;"))
  
    
    if (input_visualisation_bpw %in% c("heatmap", "both")) {
      output_plots <- tagAppendChild(output_plots, 
                                     div(
                                       plotlyOutput("heatmap", height = "800px")
                                     ))
    }
    if (input_visualisation_bpw %in% c("boxplots", "both")) {
      output_plots <- tagAppendChild(output_plots, plotlyOutput("boxplots", height = "600px"))
    }
    
    return(output_plots)
  })
  
  subset_data <- transform_and_subset_data(
    raw_data = bacterial_pathway_frame,
    pathway_labels_filter = input$pathway_label,
    pathway_species_filter = input$pathway_species,
    rescaling = input$rescaling
  )
  
  df_subset_reactive(subset_data)
})

output$heatmap <- renderPlotly({
  df_subset <- df_subset_reactive()
  
  if (nrow(df_subset) == 0) {
    return(NULL)
  }
  
  heatmap_data <- df_subset %>%
    dplyr::select(selection_label, sample_id, abundance) %>%
    tidyr::spread(key = sample_id, value = abundance) %>%
    tibble::column_to_rownames("selection_label")
  
  unique_sample_ids <- unique(df_subset$sample_id)
  heatmap_data <- tibble::as_tibble(heatmap_data) %>% 
    dplyr::select(all_of(unique_sample_ids))
  heatmap_data <- as.matrix(heatmap_data)
  
  row_side_colour <- df_subset %>%
    dplyr::select(selection_label, pathway_colour) %>%
    dplyr::distinct() %>%
    tibble::column_to_rownames("selection_label")
  
  names(row_side_colour)[names(row_side_colour) == "pathway_colour"] <- "Pathway label"
  
  col_side_colour <- df_subset %>%
    dplyr::select(
      sample_id,
      host_group_colour,
      geographic_location_colour,
      sampling_location_colour,
      young_adult_colour
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(match(sample_id, colnames(heatmap_data))) %>%
    tibble::column_to_rownames("sample_id")
  
  names(col_side_colour) <- c("Host type", "Geographic origin", "Sampling location", "Developmental stage")
  
  clustering <- shiny::isolate(input$clustering)
  
  if (clustering == "both" && nrow(heatmap_data) == 1) {
    shinyWidgets::show_alert(
      title = "Cannot operate clustering on only 1 pathway",
      text = "Please choose more than 1 pathway to operate the clustering",
      type = "error"
    )
    return(NULL)
  }
  
  heatmaply(
    heatmap_data,
    RowSideColors = row_side_colour,
    ColSideColors = col_side_colour,
    dendrogram = clustering,
    color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(1000),
    custom_hovertext = matrix(
      paste(
        "Pathway label:", rownames(heatmap_data),
        "<br>Sample:", colnames(heatmap_data)
      ),
      nrow = nrow(heatmap_data),
      ncol = ncol(heatmap_data)
    )
  ) %>%
    layout(
      xaxis = list(showticklabels = FALSE),
      showlegend = c(row_side_colors = FALSE)
    )
})

output$boxplots <- renderPlotly({
  df_subset <- df_subset_reactive()
  
  if (nrow(df_subset) == 0) {
    return(NULL)
  }
  
  colour_vector <- df_subset %>%
    dplyr::select(selection_label, pathway_colour) %>%
    distinct() %>%
    pull(pathway_colour, name = selection_label)
  
  grouping_variable <- switch(
    shiny::isolate(input$grouping_variable),
    "Host type" = df_subset$host_group,
    "Geographic origin" = df_subset$geographic_location,
    "Sampling location" = df_subset$sampling_location,
    "Developmental stage" = df_subset$young_adult
  )
  
  boxplots <- df_subset %>%
    ggplot(aes(x = grouping_variable, y = abundance, fill = selection_label)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
    facet_wrap(~ selection_label) +
    scale_fill_manual(values = colour_vector, guide = FALSE) +
    theme_classic() +
    labs(x = shiny::isolate(input$grouping_variable),
         y = "Relative abundance")
  
  if (length(unique(grouping_variable)) > 4) {
    boxplots <- boxplots + scale_x_discrete(labels = NULL)
  }
  
  ggplotly(boxplots)
})

output$table <- renderUI({
  df_subset <- df_subset_reactive()
  
  if (nrow(df_subset) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset %>%
    dplyr::select(
      selection_label,
      pathway_colour,
      pathway_code,
      pathway_description,
      pathway_species,
      significantly_different
    ) %>%
    distinct() %>%
    mutate(
      selection_label = cell_spec(
        selection_label,
        "html",
        color = "white",
        background = adjustcolor(pathway_colour, alpha.f = 0.7)
      )
    ) %>%
    dplyr::rename(
      "Pathway label" = selection_label,
      "Pathway code" = pathway_code,
      "Pathway description" = pathway_description,
      "Species / community" = pathway_species,
      "Significant difference" = significantly_different
    )
  
  table_html <- table_data %>%
    select(-pathway_colour) %>%
    kable("html", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(table_html)
})

output$legend_table <- renderUI({
  df_subset <- df_subset_reactive()
  
  if (nrow(df_subset) == 0) {
    return(NULL)
  }
  
  pathway_labels <- tibble(
    variable_index = "Pathway labels",
    Label = "pw# = consecutively assigned to selected pathways in alphabetical order",
    colour_index = "white"  # White background for black text
  )
  
  legend_data <- bind_rows(
    pathway_labels,
    create_legend_df(df_subset, "host_group", "Host type", "Host type of sample"),
    create_legend_df(df_subset, "geographic_location", "Geographic origin", "Geographic origin of sample"),
    create_legend_df(df_subset, "sampling_location", "Sampling location", "Type / location of sample"),
    create_legend_df(df_subset, "young_adult", "Developmental stage", "Developmental status of host")
  ) %>%
    mutate(variable_index = factor(
      variable_index,
      levels = c("Pathway labels", "Host type", "Geographic origin", "Sampling location", "Developmental stage")
    ))
  
  legend_html <- legend_data %>%
    dplyr::arrange(variable_index) %>%
    dplyr::mutate(Colours = case_when(
      variable_index == "Pathway labels" ~ cell_spec(
        Label,
        "html",
        color = "black",
        background = "white"
      ),
      TRUE ~ cell_spec(
        Label,
        "html",
        color = "white",
        background = adjustcolor(colour_index, alpha.f = 0.7)
      )
    )) %>%
    select(Colours) %>%
    kable("html", escape = FALSE, col.names = c("Colours and legends")) %>%
    pack_rows(index = table(arrange(
      legend_data %>%
        dplyr::select(variable_index) %>%
        dplyr::arrange(variable_index)
    )),
    background = "grey80") %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(legend_html)
})

create_legend_df <- function(df, column, variable_index, description) {
  df %>%
    distinct(!!sym(column), !!sym(paste0(column, "_colour"))) %>%
    rename(
      Label = !!sym(column),
      colour_index = !!sym(paste0(column, "_colour"))
    ) %>%
    mutate(
      Description = description,
      variable_index = variable_index,
      Colours = "   "
    )
}