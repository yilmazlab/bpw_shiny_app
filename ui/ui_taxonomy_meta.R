# ui/ui_taxonomy_meta.R

ui_taxonomy_meta <- shiny::sidebarLayout(
  sidebarPanel(
    width = 3,
    radioButtons(
      inputId = "visualisation_meta",
      label = "Select visualisation",
      choices = c("Barplot" = "barplot", "Boxplot" = "boxplot"),
      selected = "barplot"
    ),
    pickerInput(
      inputId = "taxonomic_level_meta",
      label = "Select taxonomic level",
      choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
      selected = "Phylum",
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = FALSE
    ),
    pickerInput(
      inputId = "taxonomy_meta",
      label = "Select taxa",
      choices = NULL,
      selected = NULL,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    conditionalPanel(
      condition = "input.visualisation_meta == 'boxplot'",
      selectInput(
        inputId = "grouping_variable_meta",
        label = "Select grouping variable",
        choices = c("Geographic location" = "Location", 
                    "Host type" = "Group", 
                    "Sampling location" = "SamplingLocation", 
                    "Sex" = "Gender", 
                    "Developmental stage" = "Young_Adult"),
        selected = "Group"
      )
    ),
    hr(),
    h4("Select subsetting options:"),
    pickerInput(
      inputId = "location_meta",
      label = "Geographic location",
      choices = unique(taxonomy_meta_frame$Location),
      selected = unique(taxonomy_meta_frame$Location),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "group_meta",
      label = "Host type",
      choices = unique(taxonomy_meta_frame$Group),
      selected = unique(taxonomy_meta_frame$Group),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "sampling_location_meta",
      label = "Sampling location",
      choices = unique(taxonomy_meta_frame$SamplingLocation),
      selected = unique(taxonomy_meta_frame$SamplingLocation),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "gender_meta",
      label = "Sex",
      choices = unique(taxonomy_meta_frame$Gender),
      selected = unique(taxonomy_meta_frame$Gender),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "young_adult_meta",
      label = "Developmental stage",
      choices = unique(taxonomy_meta_frame$Young_Adult),
      selected = unique(taxonomy_meta_frame$Young_Adult),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    actionButton("apply_meta", "Apply")
  ),
  mainPanel(
    h3("Relative taxa abundance", style = "text-align: center;"),
    plotlyOutput("plot_output_meta", height = "800px"),
    uiOutput("taxonomy_meta_info")
  )
)