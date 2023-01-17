# HACK use conda env when this gets fixed: https://github.com/conda-forge/r-arrow-feedstock/issues/56
renv::activate()
library(ukbtools)
library(arrow)
library(dplyr)

extract_ukb <- function(basket_path,
                        phenotypes_file,
                        basket_names,
                        output_file_path) {
  # Load UKB data

  phenotypes <- scan(phenotypes_file)

  for (basket in basket_names) {
    ukb_data[[basket]] <-
      ukb_df(basket,
        path = basket_path,
        n_threads = "max"
      )
  }

  purrr::reduce(ukb_data,
    full_join,
    by = "eid"
  ) %>%
    arrow::write_dataset(
      dataset = .,
      format = "parquet",
      path = output_path
    )
}

# Run
extract_ukb(
  basket_path = snakemake@config$basket_path,
  basket_names = snakemake@config$basket_filenames,
  phenotypes_file = snakemake@input$phenotypes,
  output_file_path = snakemake@output$output
)
