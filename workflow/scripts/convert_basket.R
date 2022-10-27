library(ukbtools)

extract_ukb <- function(data_path, filename, output) {
  # Load UKB data

  full_ukb <- ukb_df(
    filename,
    path = data_path,
    # Without this option it will take ages
    n_threads = max
  )
}

# Run
extract_ukb(
  data_path = snakemake@config$basket_path,
  filename = snakemake@config$basket_filename,
  output = snakemake@output$output
)
