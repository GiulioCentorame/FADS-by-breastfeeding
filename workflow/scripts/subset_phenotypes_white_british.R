# Load dtplyr
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(readr)

save.image("debug.RData")

clean_phenotypes <- function(data_path,
                             field_ids_path,
                             std_exclusions_path,
                             withdrawals_path,
                             related_individuals_path,
                             nonwhitebritish_path,
                             output_path) {
  # Open function to assign levels
  data <- readRDS(data_path) %>%
    lazy_dt()

  field_ids <- read_lines(field_ids_path)

  # Load file lists
  # Space-delimited file
  std_exclusions <- read_delim(std_exclusions_path,
    delim = " "
  )
  nonwhitebritish <- read_delim(nonwhitebritish_path,
    delim = " "
  )
  # .csv but it's one line per exclusion
  withdrawals <- read_lines(withdrawals_path)
  # same
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
      !(eid %in% related_individuals),
      !(eid %in% nonwhitebritish$IID)
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
  nonwhitebritish_path = snakemake@input$nonwhitebritish,
  output_path = snakemake@output$filtered_data
)
