library(readr)
library(dplyr)
library(tidyr)

save.image("debug.RData")

clean_table_ld <-
  function(path_table_ld,
           path_output_ld) {
    table_ld <- read_tsv(path_table_ld)

    # Get upper triangle of the matrix
    mat_r2 <-
      table_ld %>%
      mutate(rsq = r^2) %>%
      select(g1_rsid, g2_rsid, rsq) %>%
      pivot_wider(
        names_from = g2_rsid,
        values_from = rsq
      ) %>%
      tibble::column_to_rownames("g1_rsid")

    # Fill lower triangle with upper triangle
    mat_r2[lower.tri(mat_r2)] <- mat_r2[upper.tri(mat_r2)]

    # Move column names to a column and wrap in a tibble
    tibble_r2 <- as_tibble(mat_r2, rownames = "SNP")

    write_tsv(tibble_r2, file = path_output_ld)
  }

clean_table_ld(
  path_table_ld = snakemake@input[[1]],
  path_output_ld = snakemake@output[[1]]
)
