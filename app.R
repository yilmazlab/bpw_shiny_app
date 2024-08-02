# app.R

source("global.R")
source("utils/general_utils.R")
source("utils/taxonomy_map_utils.R")
source("utils/taxonomy_16S_utils.R")
source("utils/taxonomy_meta_utils.R")
source("utils/bacterial_pathways_utils.R")
source("utils/metabolites_utils.R")

# UI components
source("ui/ui_investigators.R")
source("ui/ui_taxonomy_map.R")
source("ui/ui_taxonomy_16s.R")
source("ui/ui_taxonomy_meta.R")
source("ui/ui_bacterial_pathways.R")
source("ui/ui_metabolites.R")
source("ui/ui_user_instructions.R")

ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  titlePanel("The vivaria study"),
  
  tabsetPanel(
    tabPanel("The investigators", ui_investigators),
    tabPanel("Laboratory microbiota profiles (map)", ui_taxonomy_map),
    tabPanel("Taxonomy profiles (16S sequencing)", ui_taxonomy_16s),
    tabPanel("Taxonomy profiles (shotgun sequencing)", ui_taxonomy_meta),
    tabPanel("Bacterial pathways", ui_bacterial_pathways),
    tabPanel("Metabolite profiles", ui_metabolites),
    tabPanel("User instructions", ui_usage_instructions)
  )
)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    df_subset = NULL,
    df_subset_16s = NULL,
    df_subset_meta = NULL,
    df_subset_metabolites = NULL
  )
  
  # Server logic for each module
  source("server/server_taxonomy_map.R", local = TRUE)
  source("server/server_taxonomy_16s.R", local = TRUE)
  source("server/server_taxonomy_meta.R", local = TRUE)
  source("server/server_bacterial_pathways.R", local = TRUE)
  source("server/server_metabolites.R", local = TRUE)
}

shinyApp(ui = ui, server = server)