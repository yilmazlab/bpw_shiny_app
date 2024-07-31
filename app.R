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
library(ggplot2)
library(openxlsx)
library(stringr)
library(leafpop)
library(htmlwidgets)
library(qs)
source("utils/utils.R")
source("utils/taxonomy_16S_utils.R")
source("utils/metabolites_utils.R")

bacterial_pathway_frame <- qs::qread("data/bacterial_pathways.qs")
taxonomy_16S_frame <- qs::qread("data/taxonomy_16S.qs")
metabolites_frame <- qread(("data/metabolites.qs"))

ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  shiny::titlePanel("The vivaria study"),
  
  shiny::tabsetPanel(
    
    shiny::tabPanel(
      title = "The investigators",
      shiny::includeMarkdown("www/cover_page.md")
    ),

    shiny::tabPanel(
      title = "Laboratory microbiota profiles (map)",
      shiny::br(),
      shiny::actionButton(
        inputId = "display_taxonomy_map",
        label = "Display Map"
      ),
      shiny::br(),
      shiny::br(),
      leaflet::leafletOutput(
        outputId = "leaflet_taxonomy_map",
        height = "1200px"
      )
    ),
    
    shiny::tabPanel(
      title = "16S taxonomy profiles",
      shiny::sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(
            inputId = "visualization_16S",
            label = "Select visualisation",
            choices = c("Barplot" = "barplot", "Boxplot" = "boxplot"),
            selected = "barplot"
          ),
          pickerInput(
            inputId = "taxonomic_level",
            label = "Select taxonomic level",
            choices = c("Phylum", "Family", "Genus"),
            selected = "Phylum",
            options = list(`actions-box` = TRUE, `live-search` = TRUE),
            multiple = FALSE
          ),
          pickerInput(
            inputId = "taxonomy",
            label = "Select taxa",
            choices = NULL,
            selected = NULL,
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          conditionalPanel(
            condition = "input.visualization_16S == 'boxplot'",
            selectInput(
              inputId = "grouping_variable",
              label = "Select grouping variable",
              choices = c("Geographic origin" = "Origin", 
                          "Host type" = "Group", 
                          "Sampling location" = "SampleType", 
                          "Sex" = "Gender", 
                          "Sampling season" = "Season"),
              selected = "Group"
            )
          ),
          hr(),
          h4("Select subsetting options:"),
          pickerInput(
            inputId = "origin",
            label = "Geographic origin",
            choices = unique(taxonomy_16S_frame$Origin),
            selected = unique(taxonomy_16S_frame$Origin),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "group",
            label = "Host type",
            choices = unique(taxonomy_16S_frame$Group),
            selected = unique(taxonomy_16S_frame$Group),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "sample_type",
            label = "Sampling location",
            choices = unique(taxonomy_16S_frame$SampleType),
            selected = unique(taxonomy_16S_frame$SampleType),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "gender",
            label = "Sex",
            choices = unique(taxonomy_16S_frame$Gender),
            selected = unique(taxonomy_16S_frame$Gender),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "age",
            label = "Developmental stage",
            choices = unique(taxonomy_16S_frame$Adult_Pups),
            selected = unique(taxonomy_16S_frame$Adult_Pups),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          pickerInput(
            inputId = "season",
            label = "Sampling season",
            choices = unique(taxonomy_16S_frame$Season),
            selected = unique(taxonomy_16S_frame$Season),
            options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
            multiple = TRUE
          ),
          actionButton("apply_16S", "Apply")
        ),
        mainPanel(
          plotlyOutput("plot_output_16S", height = "800px"),
          uiOutput("taxonomy_16S_info")
        )
      )
    ),

    shiny::tabPanel(
      title = "Metabolite profiles",
      shiny::sidebarLayout(
      sidebarPanel(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = "metabolite_name",
          label = "Select metabolites",
          choices = unique(metabolites_frame$metabolite_name),
          options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
          multiple = TRUE
        ),
        
        shiny::radioButtons(
          inputId = "visualization_metabolites",
          label = "Select visualisation",
          choices = c("Heatmap only" = "heatmap", "Boxplots only" = "boxplots", "Both" = "both"),
          selected = "heatmap"
        ),
        
        shiny::conditionalPanel(
          condition = "input.visualization_metabolites == 'heatmap' || input.visualization_metabolites == 'both'",
          radioButtons(
            inputId = "rescaling_metabolites",
            label = "Rescaling",
            choices = c("On" = "on", "Off" = "off"),
            selected = "off"
          ),
          
          shiny::radioButtons(
            inputId = "clustering_metabolites",
            label = "Heatmap clustering",
            choices = c("On" = "both", "Off" = "none"),
            selected = "none"
          )
        ),  
        
        shiny::conditionalPanel(
          condition = "input.visualization_metabolites == 'boxplots' || input.visualization_metabolites == 'both'",
          shiny::radioButtons(
            inputId = "grouping_variable",
            label = "Grouping variable",
            choices = c(
              "Host type" = "Group",
              "Geographic origin" = "Origin",
              "Sampling location" = "SampleType",
              "Developmental stage" = "Adult_Pups"
            ),
            selected = "Group"
          )
        ),
        
        shiny::actionButton("apply_metabolites", "Apply"),
        shiny::uiOutput("legend_table_metabolites")
      ),
      
      shiny::mainPanel(
        shiny::uiOutput("visualization_output_metabolites"),
        shiny::uiOutput("metabolite_table")
        )
      )
    ),
    
    shiny::tabPanel(
      title = "Usage instructions",
      shiny::includeMarkdown("www/notes_page.md")
    )
    
  )
)

server <- function(input, output, session) {
  df_subset_reactive <- shiny::reactiveVal(tibble::tibble())

  output$pathway_species_ui <- renderUI({
    req(input$pathway_label)
    df_subset <-
      bacterial_pathway_frame[bacterial_pathway_frame$pathway_label %in% input$pathway_label,]
    species_choices <- unique(df_subset$pathway_species)
    species_choices <-
      c("Sample community", species_choices[species_choices != "Sample community"])
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

  shiny::observeEvent(input$apply, {
    output$visualization_output <- renderUI({
      output_plots <- tagList()

      input_viz <- shiny::isolate(input$visualization)

      if (input_viz == "heatmap" || input_viz == "both") {
        output_plots <- tagAppendChild(output_plots,
                                       plotlyOutput("heatmap", height = "800px"))
      }
      if (input_viz == "boxplots" || input_viz == "both") {
        output_plots <- tagAppendChild(output_plots,
                                       plotlyOutput("boxplots", height = "600px"))
      }

      return(output_plots)
    })

  })

  shiny::observeEvent(input$apply, {
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

    heatmap_data <- df_subset |>
      dplyr::select(selection_label, sample_id, abundance) |>
      tidyr::spread(key = sample_id, value = abundance) |>
      tibble::column_to_rownames("selection_label")

    unique_sample_ids <- unique(df_subset$sample_id)

    heatmap_data <- tibble::as_tibble(heatmap_data)

    heatmap_data <-
      heatmap_data |> dplyr::select(all_of(unique_sample_ids))

    heatmap_data <- as.matrix(heatmap_data)

    row_side_color <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      dplyr::distinct() |>
      tibble::column_to_rownames("selection_label")

    names(row_side_color)[names(row_side_color) == "pathway_color"] <-
      "Pathway label"

    col_side_color <- df_subset |>
      dplyr::select(
        sample_id,
        host_group_color,
        geographic_location_color,
        sampling_location_color,
        young_adult_color
      ) |>
      dplyr::distinct() |>
      dplyr::arrange(match(sample_id, colnames(heatmap_data))) |>
      tibble::column_to_rownames("sample_id")

    names(col_side_color)[names(col_side_color) == "host_group_color"] <-
      "Host type"
    names(col_side_color)[names(col_side_color) == "geographic_location_color"] <-
      "Geographic origin"
    names(col_side_color)[names(col_side_color) == "sampling_location_color"] <-
      "Sampling location"
    names(col_side_color)[names(col_side_color) == "young_adult_color"] <-
      "Developmental stage"

    clustering <- shiny::isolate(input$clustering)

    if (clustering == "both" && nrow(heatmap_data) == 1) {
      shinyWidgets::show_alert(title = "Can't operate clustering on only 1 pathway",
                               text = "Please choose more than 1 pathway in order to operate
        the clustering",
        type = "error")

      return(NULL)

    }

    heatmaply(
      heatmap_data,
      RowSideColors = row_side_color,
      ColSideColors = col_side_color,
      dendrogram = clustering,
      color = colorRampPalette(rev(brewer.pal(
        n = 11, name =
          "RdBu"
      )))(1000),
      custom_hovertext = matrix(
        paste(
          "Pathway label:",
          rownames(heatmap_data),
          "<br>Sample:",
          colnames(heatmap_data)
        ),
        nrow = nrow(heatmap_data),
        ncol = ncol(heatmap_data)
      )
    ) |>
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

    # Create a named vector of colors
    color_vector <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      distinct() |>
      pull(pathway_color, name = selection_label)  # Use pull to create a named vector

    # Determine the grouping variable based on the selected radio button
    grouping_variable <-
      switch(
        shiny::isolate(input$grouping_variable),
        "Host type" = df_subset$host_group,
        "Geographic origin" = df_subset$geographic_location,
        "Sampling location" = df_subset$sampling_location,
        "Developmental stage" = df_subset$young_adult
      )

    # Create the boxplots
    boxplots <- df_subset |>
      ggplot(aes(x = grouping_variable, y = abundance, fill = selection_label)) +
      geom_boxplot(outliers = F) +
      geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +  # Add dotplot with transparency and color mapping
      facet_wrap( ~ selection_label) +
      scale_fill_manual(values = color_vector, guide = FALSE) +  # Use the color vector here and remove the legend
      theme_classic() +
      labs(x = shiny::isolate(input$grouping_variable),
           y = "Relative abundance")  # Set the x and y axis labels

    # Remove the x-axis labels if there are more than 4 unique values in grouping_variable
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

    table_data <- df_subset |>
      dplyr::select(
        selection_label,
        pathway_color,
        pathway_code,
        pathway_description,
        pathway_species,
        significantly_different
      ) |>
      distinct() |>
      mutate(
        selection_label = cell_spec(
          selection_label,
          "html",
          color = "white",
          background = adjustcolor(pathway_color, alpha.f = 0.7)
        )
      ) |>
      dplyr::rename(
        "Pathway label" = selection_label,
        "Pathway code" = pathway_code,
        "Pathway description" = pathway_description,
        "Species / community" = pathway_species,
        "Significant difference" = significantly_different
      )

    # Create the table
    table_html <- table_data |>
      select(-pathway_color)  |>
      kable("html", escape = FALSE) |>
      kable_styling("striped", full_width = FALSE)

    # Render the table as HTML
    HTML(table_html)
  })

  output$legend_table <- renderUI({
    df_subset <- df_subset_reactive()

    if (nrow(df_subset) == 0) {
      return(NULL)
    }

    # Use levels() function for factors
    # Modify host_group_df
    host_group_df <- data.frame(
      Label = levels(df_subset$host_group),
      Description = "Host type of sample",
      variable_index = "Host type",
      Colors = "   ",
      color_index = sapply(levels(df_subset$host_group), function(l) {
        # Find the first color where df_subset$host_group matches the level
        color <-
          df_subset$host_group_color[df_subset$host_group == l][1]
        # If no color is found, return NA
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )

    # Modify geographic_location_df
    geographic_location_df <- data.frame(
      Label = levels(df_subset$geographic_location),
      Description = "Geographic origin of sample",
      variable_index = "Geographic origin",
      Colors = "   ",
      color_index = sapply(levels(df_subset$geographic_location), function(l) {
        color <-
          df_subset$geographic_location_color[df_subset$geographic_location == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )

    # Modify sampling_location_df
    sampling_location_df <- data.frame(
      Label = levels(df_subset$sampling_location),
      Description = "Type / location of sample",
      variable_index = "Sampling location",
      Colors = "   ",
      color_index = sapply(levels(df_subset$sampling_location), function(l) {
        color <-
          df_subset$sampling_location_color[df_subset$sampling_location == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )

    # Modify young_adult_df
    young_adult_df <- data.frame(
      Label = levels(df_subset$young_adult),
      Description = "Developmental status of host",
      variable_index = "Developmental stage",
      Colors = "   ",
      color_index = sapply(levels(df_subset$young_adult), function(l) {
        color <- df_subset$young_adult_color[df_subset$young_adult == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )

    # Combine data frames
    legend_data <-
      rbind(host_group_df,
            geographic_location_df,
            sampling_location_df,
            young_adult_df) |>
      dplyr::mutate(variable_index = ifelse(
        variable_index == "host_group",
        "Host type",
        ifelse(
          variable_index == "geographic_location",
          "Geographic origin",
          ifelse(
            variable_index == "sampling_location",
            "Sampling location",
            ifelse(
              variable_index == "young_adult",
              "Developmental stage",
              variable_index
            )
          )
        )
      )) |>
      dplyr::mutate(variable_index = factor(
        variable_index,
        levels = c(
          "Host type",
          "Geographic origin",
          "Sampling location",
          "Developmental stage"
        )
      ))


    # Create the legend table
    legend_html <- legend_data |>
      dplyr::arrange(variable_index) |>
      dplyr::mutate(Colors = cell_spec(
        Colors,
        "html",
        color = "white",
        background = adjustcolor(color_index, alpha.f = 0.7)
      )) |>
      select(Colors) |>
      kable("html", escape = FALSE) |>
      pack_rows(index = table(arrange(
        legend_data |>
          dplyr::select(variable_index) |>
          dplyr::arrange(variable_index)
      )),
      background = "grey80") |>
      kable_styling("striped", full_width = FALSE)

    # Render the legend table as HTML
    HTML(legend_html)
  })


  shiny::observeEvent(input$display_taxonomy_map, {


    output$leaflet_taxonomy_map <- leaflet::renderLeaflet({

      taxonomy_phylum_data <- read_and_transform_taxonomy_xlsx(
        "data/taxanomy_world_map.xlsx",
        sheet = 1
      )

      taxonomy_phylum_data <- taxonomy_phylum_data %>%
        dplyr::mutate(
          category = "Phylum"
        )

      taxonomy_genus_data <- read_and_transform_taxonomy_xlsx(
        "data/taxanomy_world_map.xlsx",
        sheet = 2
      )

      taxonomy_genus_data <- taxonomy_genus_data %>%
        dplyr::mutate(
          category = "Genus"
        )

      taxonomy_data <- rbind(
        taxonomy_phylum_data,
        taxonomy_genus_data
      )

      taxonomy_data <- taxonomy_data %>%
        dplyr::arrange(location)

      taxonomy_data <- taxonomy_data %>%
        dplyr::mutate(
          location = as.factor(location)
        )

      taxonomy_data_splitted <- split(taxonomy_data, taxonomy_data$location)

      plot_list <- lapply(taxonomy_data_splitted, generate_leaflet_plot_list)

      fl = lapply(
        plot_list, function(j) {
          fl = tempfile(fileext = ".html")
          htmlwidgets::saveWidget(j, file = fl)
          return(fl)
        }
      )

      leaflet_taxonomy_data <- taxonomy_data %>%
        dplyr::distinct(location, longitude, latitude)

      leaflet_taxonomy_data <- leaflet_taxonomy_data %>%
        dplyr::arrange(location)

      leaflet(leaflet_taxonomy_data) %>%
        addTiles() %>%
        addMarkers(lng = ~longitude, lat = ~latitude, group = "location") %>%
        leafpop:::addPopupIframes(fl, group = "location", width = 600, height = 400)

    })


  })
  
  df_subset_reactive_metabolites <- shiny::reactiveVal(tibble::tibble())
  
  observeEvent(input$apply_metabolites, {
    output$visualization_output_metabolites <- renderUI({
      output_plots_metabolites <- tagList()
      
      input_viz_metabolites <- isolate(input$visualization_metabolites)
      
      if (input_viz_metabolites == "heatmap" || input_viz_metabolites == "both") {
        output_plots_metabolites <- tagAppendChild(
          output_plots_metabolites, 
          plotlyOutput("heatmap_metabolites", height = "800px")
        )
      }
      if (input_viz_metabolites == "boxplots" || input_viz_metabolites == "both") {
        output_plots_metabolites <- tagAppendChild(
          output_plots_metabolites, 
          plotlyOutput("boxplots_metabolites", height = "600px")
        )
      }
      
      return(output_plots_metabolites)
    })
  })
  
  observeEvent(input$apply_metabolites, {
    subset_data <- transform_and_subset_data_metabolites(
      raw_data = metabolites_frame,
      metabolite_group_filter = unique(metabolites_frame$metabolite_group),  # Use all groups
      metabolite_name_filter = input$metabolite_name,
      rescaling = input$rescaling_metabolites
    )
    
    df_subset_reactive_metabolites(subset_data)
  })
  
  output$heatmap_metabolites <- renderPlotly({
    df_subset_metabolites <- df_subset_reactive_metabolites()
    
    if (nrow(df_subset_metabolites) == 0) {
      return(NULL)
    }
    
    heatmap_data <- df_subset_metabolites %>%
      select(metabolite_name, sample_id, metabolite_abundancy) %>%
      spread(key = sample_id, value = metabolite_abundancy) %>%
      column_to_rownames("metabolite_name")
    
    row_side_color <- df_subset_metabolites %>%
      select(metabolite_name, metabolite_group_color) %>%
      distinct() %>%
      column_to_rownames("metabolite_name")
    
    col_side_color <- df_subset_metabolites %>%
      select(sample_id, geographic_location_color_metabolites) %>%
      distinct() %>%
      column_to_rownames("sample_id")
    
    clustering <- isolate(input$clustering_metabolites)
    if (clustering == "both" && nrow(heatmap_data) == 1) {
      shinyWidgets::show_alert(title = "Can't operate clustering on only 1 metabolite",
                               text = "Please choose more than 1 metabolite in order to operate
        the clustering",
        type = "error")
      
      return(NULL)
      
    }
    
    heatmaply(heatmap_data, 
              RowSideColors = row_side_color, 
              ColSideColors = col_side_color, 
              dendrogram = clustering,
              color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(1000),
              custom_hovertext = matrix(
                paste("Metabolite:", rownames(heatmap_data), "<br>Sample:", colnames(heatmap_data)),
                nrow = nrow(heatmap_data),
                ncol = ncol(heatmap_data)
              )) %>%
      layout(
        xaxis = list(showticklabels = FALSE),
        showlegend = c(row_side_colors = FALSE) 
      )
  })
  
  output$boxplots_metabolites <- renderPlotly({
    df_subset_metabolites <- df_subset_reactive_metabolites()
    
    if (nrow(df_subset_metabolites) == 0) {
      return(NULL)
    }
    
    boxplots <- df_subset_metabolites %>%
      ggplot(aes(x = geographic_location, y = metabolite_abundancy, fill = metabolite_name)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
      facet_wrap(~ metabolite_name, scales = "free_y") +
      scale_fill_manual(values = setNames(unique(df_subset_metabolites$metabolite_color), unique(df_subset_metabolites$metabolite_name))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Geographic Location", y = "Metabolite Abundance")
    
    ggplotly(boxplots)
  })
  
  output$legend_table_metabolites <- renderUI({
    df_subset_metabolites <- df_subset_reactive_metabolites()
    
    if (nrow(df_subset_metabolites) == 0) {
      return(NULL)
    }
    
    table_data <- df_subset_metabolites %>%
      select(geographic_location, sample_id, geographic_location_color_metabolites) %>%
      distinct() %>%
      mutate(geographic_location = cell_spec(geographic_location, "html", 
                                             color = "white", 
                                             background = adjustcolor(geographic_location_color_metabolites, alpha.f = 0.7))) %>%
      select(geographic_location, sample_id)
    
    table_html <- table_data %>%
      kable("html", escape = FALSE, col.names = c("Geographic location", "Sample id")) %>%
      kable_styling("striped", full_width = FALSE)
    
    HTML(table_html)
  })
  
  output$metabolite_table <- renderUI({
    df_subset_metabolites <- df_subset_reactive_metabolites()
    
    if (nrow(df_subset_metabolites) == 0) {
      return(NULL)
    }
    
    table_data <- df_subset_metabolites %>%
      select(metabolite_name, metabolite_group, metabolite_color) %>%
      distinct() %>%
      mutate(metabolite_name = cell_spec(metabolite_name, "html", 
                                         color = "white", 
                                         background = adjustcolor(metabolite_color, alpha.f = 0.7))) %>%
      select(metabolite_name, metabolite_group)
    
    table_html <- table_data %>%
      kable("html", escape = FALSE, col.names = c("Metabolite", "Group")) %>%
      kable_styling("striped", full_width = FALSE)
    
    HTML(table_html)
  }) 

  observe({
    req(input$taxonomic_level)
    selected_level <- tolower(input$taxonomic_level)
    taxonomy_choices <- unique(taxonomy_16S_frame$taxonomy[taxonomy_16S_frame$taxonomic_level == selected_level])
    
    updatePickerInput(
      session = session,
      inputId = "taxonomy",
      choices = taxonomy_choices,
      selected = NULL
    )
  })
  
  df_subset_reactive_16S <- shiny::reactiveVal(NULL)
  
  observeEvent(input$apply_16S, {
    req(input$taxonomic_level)
    
    if (is.null(input$taxonomy) || length(input$taxonomy) == 0) {
      shinyWidgets::show_alert(
        title = "No taxa selected",
        text = "Please select at least one taxon for the visualisation.",
        type = "error"
      )
      return(NULL)
    }
    
    subset_data <- transform_and_subset_data_16S(
      raw_data = taxonomy_16S_frame,
      taxonomic_level_filter = tolower(input$taxonomic_level),
      taxonomy_filter = input$taxonomy,
      origin_filter = input$origin,
      group_filter = input$group,
      sample_type_filter = input$sample_type,
      gender_filter = input$gender,
      age_filter = input$age,
      season_filter = input$season
    )
    
    df_subset_reactive_16S(subset_data)
  })
  
  output$plot_output_16S <- renderPlotly({
    req(df_subset_reactive_16S())
    df_subset_16S <- df_subset_reactive_16S()
    
    if (nrow(df_subset_16S) == 0) {
      return(NULL)
    }
    
    if (input$visualization_16S == "barplot") {
      hovertext <- paste(
        "SampleID:", df_subset_16S$SampleID, "<br>",
        "Taxonomy:", df_subset_16S$taxonomy, "<br>",
        "Relative Abundance:", round(df_subset_16S$relative_abundance, 4), "<br>",
        "Origin:", df_subset_16S$Origin, "<br>",
        "Group:", df_subset_16S$Group, "<br>",
        "SampleType:", df_subset_16S$SampleType, "<br>",
        "Gender:", df_subset_16S$Gender, "<br>",
        "Age:", df_subset_16S$Adult_Pups, "<br>",
        "Season:", df_subset_16S$Season
      )
      
      plot_ly(df_subset_16S, x = ~SampleID, y = ~relative_abundance, type = 'bar', color = ~taxonomy,
              colors = setNames(df_subset_16S$taxonomy_16S_color, df_subset_16S$taxonomy),
              hoverinfo = 'text', text = hovertext) %>%
        layout(barmode = 'stack', 
               title = paste('Relative Abundance of Taxonomies -', input$taxonomic_level),
               xaxis = list(title = 'Sample ID'),
               yaxis = list(title = 'Relative Abundance'),
               showlegend = FALSE)
      
    } else {
      grouping_var <- input$grouping_variable
      
      boxplots <- df_subset_16S %>%
        ggplot(aes_string(x = grouping_var, y = "relative_abundance", fill = "taxonomy")) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        facet_wrap(~ taxonomy, scales = "free_y") +
        scale_fill_manual(values = setNames(unique(df_subset_16S$taxonomy_16S_color), unique(df_subset_16S$taxonomy))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        labs(x = names(which(c("Origin" = "Geographic origin", 
                               "Group" = "Host type", 
                               "SampleType" = "Sampling location", 
                               "Gender" = "Sex", 
                               "Season" = "Sampling season") == grouping_var)), 
             y = "Relative Abundance",
             title = paste('Relative Abundance of Taxonomies -', input$taxonomic_level))
      
      ggplotly(boxplots)
    }
  })
  
  output$taxonomy_16S_info <- renderUI({
    req(df_subset_reactive_16S())
    df_subset_16S <- df_subset_reactive_16S()
    
    if (nrow(df_subset_16S) == 0) {
      return(NULL)
    }
    
    table_data <- df_subset_16S %>%
      group_by(taxonomy, taxonomy_16S_color) %>%
      summarise(mean_abundance = mean(relative_abundance), .groups = "drop") %>%
      arrange(desc(mean_abundance)) %>%
      mutate(
        mean_abundance = sprintf("%.4f", mean_abundance),
        taxonomy = cell_spec(taxonomy, "html", 
                             color = "white", 
                             background = adjustcolor(taxonomy_16S_color, alpha.f = 0.7))
      )
    
    table_html <- table_data %>%
      select(taxonomy, mean_abundance) %>%
      kable("html", escape = FALSE, col.names = c("Taxonomy", "Mean Relative Abundance")) %>%
      kable_styling("striped", full_width = FALSE)
    
    HTML(paste("<h4>Mean Relative Abundance</h4>", table_html))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
