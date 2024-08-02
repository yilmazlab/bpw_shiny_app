# ui/ui_bacterial_pathways.R

ui_bacterial_pathways <- shiny::sidebarLayout(
  sidebarPanel(
    width = 3,
    shinyWidgets::pickerInput(
      inputId = "pathway_label",
      label = "Select pathways",
      choices = unique(bacterial_pathway_frame$pathway_label),
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        multiple = TRUE
      ),
      multiple = TRUE
    ),
    
    shiny::uiOutput("pathway_species_ui"),
    
    shiny::radioButtons(
      inputId = "visualisation",
      label = "Select visualisation",
      choices = c(
        "Heatmap only" = "heatmap",
        "Boxplots only" = "boxplots",
        "Both" = "both"
      ),
      selected = "heatmap"
    ),
    
    # Rescaling option always available
    shiny::radioButtons(
      inputId = "rescaling",
      label = "Rescaling",
      choices = c("On" = "on", "Off" = "off"),
      selected = "off"
    ),
    
    shiny::conditionalPanel(
      condition = "input.visualisation == 'heatmap' || input.visualisation == 'both'",
      shiny::radioButtons(
        inputId = "clustering",
        label = "Heatmap clustering",
        choices = c("On" = "both", "Off" = "none"),
        selected = "none"
      )
    ),
    
    shiny::conditionalPanel(
      condition = "input.visualisation == 'boxplots' || input.visualisation == 'both'",
      shiny::radioButtons(
        inputId = "grouping_variable",
        label = "Grouping variable",
        choices = c(
          "Host type",
          "Geographic origin",
          "Sampling location",
          "Developmental stage"
        ),
        selected = "Host type"
      )
    ),
    
    shiny::actionButton("apply", "Apply"),
    shiny::uiOutput("legend_table")
  ),
  
  shiny::mainPanel(
    shiny::uiOutput("visualisation_output"),
    shiny::tableOutput("table")
  )
)
