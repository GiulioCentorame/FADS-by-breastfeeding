library(data.table)

merge_data <- function(phenotypes_path, variants_path, output_path) {
  # Read data in
  phenotypes <- fread(phenotypes_path)

  variants <- fread(variants_path)

  # Outer join

  merged_data <-
    merge(phenotypes,
      variants[, .SD, .SDcols = !c("FID", "PAT", "MAT", "SEX", "PHENOTYPE")],
      by.x = "eid",
      by.y = "IID"
    )

  # Write to disk
  fwrite(merged_data, output_path, sep = "\t")
}

merge_data(
  phenotypes_path = snakemake@input$phenotypes,
  variants_path = snakemake@input$variants,
  output_path = snakemake@output$merged
)
