library(data.table)

# Extract raw phenotypes from the UKB .rda file
# Returns a .rda file with a data.table object containing
# the raw phenotypes
extract_raw_phenotypes <- function(data, output) {
  base::load(data)
}

extract_raw_phenotypes(
  data = snakemake@input[[1]],
  output = snakemake@output[[1]]
)
