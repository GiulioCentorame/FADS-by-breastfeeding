library(data.table)
library(dplyr)

clean_phenotypes <- function(data,
                             script,
                             std_exclusions,
                             withdrawals,
                             output) {
  # Open function to assign levels
  source(script)

  # Open data
  phenotypes <- fread(data)

  # Load file lists
  std_exclusions <- fread(std_exclusions)
  withdrawals <- scan(withdrawals)

  # Assign levels
  phenotypes_levels <- assign_levels(phenotypes)

  # Clean up data
  # HACK move to its own function
  phenotypes_clean <-
    phenotypes_levels %>%
    rename(
      eid                          =       f.eid,
      age                          =    f.34.0.0,
      reported_sex                 =    f.31.0.0,
      genetic_sex                  = f.22001.0.0,
      FI_online                    = f.20191.0.0,
      Townsend_deprivation_index   =   f.189.0.0,
      IMD_England                  = f.26410.0.0,
      IMD_Scotland                 = f.26427.0.0,
      IMD_Wales                    = f.26426.0.0,
      Batch                        = f.22001.0.0,
      Plate                        = f.22004.0.0,
      Well                         = f.22008.0.0
    ) %>%
    # Change names for principal components files
    rename_with(
      ~ sub("22009", "principal_component", .x, fixed = TRUE) %>%
        sub(".0.", "_", ., fixed = TRUE),
      .cols = starts_with("f.22009.")
    ) %>%
    # Change names for FI assessment centre
    rename_with(
      ~ sub("20016", "FI_centre", .x, fixed = TRUE) %>%
        sub("\\.0$", "_", .) %>%
        gsub(".", "_", ., fixed = TRUE),
      .cols = starts_with("f.20016.")
    ) %>%
    # Change names for age completed FT education
    rename_with(
      ~ sub("845", "age_completed_ft_education", .x, fixed = TRUE) %>%
        sub("\\.0$", "_", .) %>%
        gsub(".", "_", ., fixed = TRUE),
      .cols = starts_with("f.845.")
    ) %>%
    # Change names for qualification variables
    rename_with(
      ~ sub("6138", "qualifications", .x, fixed = TRUE) %>%
        sub(".0.", "_", ., fixed = TRUE),
      .cols = starts_with("f.6138.")
    ) %>%
    # Change names for DHA variables
    rename_with(
      ~ sub("23450", "DHA_total", .x, fixed = TRUE) %>%
        sub(".0.", "_", ., fixed = TRUE),
      .cols = starts_with("f.23450.")
    ) %>%
    rename_with(
      ~ sub("23457", "DHA_to_FA_ratio", .x, fixed = TRUE) %>%
        sub(".0.", "_", ., fixed = TRUE),
      .cols = starts_with("f.23457.")
    ) %>%
    rename_with(
      ~ sub("26231", "DHA_PRS", .x, fixed = TRUE) %>%
        sub(".0.0", "", ., fixed = TRUE),
      .cols = starts_with("f.26231.")
    ) %>%
    # Remove fs at the beginning of every variable
    rename_with(~ gsub("f.", "", .x, fixed = TRUE))

  if (any(
    # Have we transformed all the variable names?
    phenotypes_clean %>%
      names() %>%
      grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", ., perl = TRUE)
  )) {
    warning("Some variables still have field IDs")
  }

  # Remove std exclusions and withdrawals
  phenotypes_clean <-
    phenotypes_clean %>%
    filter(
      # Standard genetic QC exclusions
      !(std_exclusions$IID %in% eid),
      # Withdrawals
      !(withdrawals %in% eid)
    )

  # Write data
  fwrite(phenotypes_clean, output, sep = "\t")
}

clean_phenotypes(
  data = snakemake@input$data,
  script = snakemake@input$derived_script,
  std_exclusions = snakemake@input$std_exclusions,
  withdrawals = snakemake@input$withdrawals,
  output = snakemake@output$clean_data
)
