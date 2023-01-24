# Load dtplyr
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(readr)

clean_phenotypes <- function(data_path,
                             field_ids_path,
                             std_exclusions_path,
                             withdrawals_path,
                             related_individuals_path,
                             output_path) {
  # Open function to assign levels
  data <- readRDS(data_path) %>%
    lazy_dt()

  field_ids <- read_lines(field_ids_path)

  # Load file lists
  std_exclusions <- read_lines(std_exclusions_path)
  withdrawals <- read_lines(withdrawals_path)
  related_individuals <- read_lines(related_individuals_path)

  # Extract fields of interest
  data_filtered <-
    data %>%
    select(eid, contains(paste0("_f", field_ids, "_"))) %>%
    filter(
      # Standard genetic QC exclusions
      !(eid %in% std_exclusions$IID),
      # Withdrawals
      !(eid %in% withdrawals),
      !(eid %in% related_individuals)
    ) %>%
    # Run transformation
    as_tibble()

  # Write data
  data_filtered %>%
    saveRDS(., file = output_path)
}

clean_phenotypes(
  data_path = snakemake@input$data,
  field_ids_path = snakemake@input$field_ids,
  std_exclusions_path = snakemake@input$std_exclusions,
  withdrawals_path = snakemake@input$withdrawals,
  related_individuals_path = snakemake@input$related_individuals,
  output_path = snakemake@output$filtered_data
)
