library(dplyr)
library(tidyr)
library(qs)
library(openxlsx)

taxonomy_meta <- read.xlsx("data/MetagenomicData.xlsx", sheet = 1)

metadata <- taxonomy_meta[,1:7]
taxonomydata <- taxonomy_meta[,c(1, 8:ncol(taxonomy_meta))]

# Define the function to count the number of pipe characters in a string -> number of pipes is a marker for taxonomic level
count_pipes <- function(column_name) {
  return(sum(unlist(strsplit(column_name, NULL)) == "|"))
}

# Create subsets of the dataframe based on the number of pipes in the column names
create_subframe <- function(df, num_pipes) {
  # Always include the 'Sample' column
  columns_to_keep <- c("Sample", names(df)[sapply(names(df), count_pipes) == num_pipes])
  return(df[, columns_to_keep, drop = FALSE])
}

# Create the subframes
taxonomydata_p <- create_subframe(taxonomydata, 1)
taxonomydata_c <- create_subframe(taxonomydata, 2)
taxonomydata_o <- create_subframe(taxonomydata, 3)
taxonomydata_f <- create_subframe(taxonomydata, 4)
taxonomydata_g <- create_subframe(taxonomydata, 5)
taxonomydata_s <- create_subframe(taxonomydata, 6)

# Transform the dataframe to long format
taxonomydata_p <- pivot_longer(
  taxonomydata_p,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)
taxonomydata_c <- pivot_longer(
  taxonomydata_c,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)
taxonomydata_o <- pivot_longer(
  taxonomydata_o,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)
taxonomydata_f <- pivot_longer(
  taxonomydata_f,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)
taxonomydata_g <- pivot_longer(
  taxonomydata_g,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)
taxonomydata_s <- pivot_longer(
  taxonomydata_s,
  cols = -Sample,
  names_to = "taxonomy",
  values_to = "relative_abundance"
)

# add id columns for non-expansive merging
taxonomydata_p$taxonomic_level <- "Phylum" 
taxonomydata_c$taxonomic_level <- "Class" 
taxonomydata_o$taxonomic_level <- "Order" 
taxonomydata_f$taxonomic_level <- "Family" 
taxonomydata_g$taxonomic_level <- "Genus" 
taxonomydata_s$taxonomic_level <- "Species" 

final_meta_frame <- rbind(taxonomydata_p, taxonomydata_c,  taxonomydata_o,  taxonomydata_f,  taxonomydata_g, taxonomydata_s)
final_meta_frame <- merge(final_meta_frame, metadata, by = "Sample", all.x = TRUE)

# Define the desired order for taxonomic levels
desired_order <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")

# Convert taxonomic_level to a factor with the desired order
final_meta_frame$taxonomic_level <- factor(final_meta_frame$taxonomic_level, levels = desired_order)


qsave(final_meta_frame, "data/taxonomy_meta.qs",
      preset = "archive")

rm(list = ls())

taxonomy_meta_frame <- qread("data/taxonomy_meta.qs")
