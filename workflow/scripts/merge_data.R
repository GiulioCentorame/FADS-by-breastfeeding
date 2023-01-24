library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(readr)

save.image("debug.RData")

merge_data <- function(phenotypes_path,
                       SBP_path,
                       DBP_path,
                       variants_path,
                       output_path) {
  # Read data in
  phenotypes <- readRDS(phenotypes_path)

  variants <- read_table(variants_path)

  SBP_data <- read_tsv(SBP_path) %>%
    rename(
      Age_SBP = Age,
      Centre_SBP = Centre
    )

  DBP_data <- read_tsv(DBP_path) %>%
    rename(
      Age_DBP = Age,
      Centre_DBP = Centre
    )

  # Outer join
  merged_data <-
    phenotypes %>%
    # We already operated the exclusions on the main data
    # so inner join allows to retain only the ones we
    # have not excluded yet
    inner_join(variants, by = c("eid" = "IID")) %>%
    left_join(SBP_data, by = "eid") %>%
    left_join(DBP_data, by = "eid")

  # Write to disk
  saveRDS(merged_data, output_path)
}

merge_data(
  phenotypes_path = snakemake@input$phenotypes,
  variants_path = snakemake@input$variants,
  SBP_path = snakemake@input$SBP,
  DBP_path = snakemake@input$DBP,
  output_path = snakemake@output$merged
)
