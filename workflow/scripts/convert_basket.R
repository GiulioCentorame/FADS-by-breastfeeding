# HACK use conda env when this gets fixed: https://github.com/conda-forge/r-arrow-feedstock/issues/56
renv::activate()
library(ukbtools)
library(dplyr)

# Save image for debugging
save.image("debug.RData")

extract_ukb <- function(basket_path,
                        phenotypes_file,
                        basket_names,
                        output_file_path) {
  # Declare list
  ukb_data <- list()

  # Check that output folder exists, if not, create it
  # NOTE will print a warning if it exists

  output_file_path %>%
    stringi::stri_replace_last_regex(
      .,
      "\\/[^/]+\\.rds$", # matches just the file name
      "" # swap with empty string
    ) %>%
    dir.create(.,
      recursive = TRUE
    )

  # Load UKB data and populate list with it

  for (basket in basket_names) {
    ukb_data[[basket]] <-
      ukb_df(basket,
        path = basket_path,
        n_threads = "max"
      )
  }

  ukb <-
    purrr::reduce(ukb_data,
      full_join,
      by = "eid"
    )

  saveRDS(ukb, file = output_file_path)
}

# Run
extract_ukb(
  basket_path = snakemake@config$basket_path,
  basket_names = snakemake@config$basket_filename,
  output_file_path = snakemake@output$rds_file
)
