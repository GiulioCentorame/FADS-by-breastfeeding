library(dplyr)
library(purrr)
library(ggplot2)
library(forcats)
library(tidyr)
library(broom)

### Functions
# Formulas to fit
formula_unadj <- function(outcome,
                          SNP,
                          dual_delivery = FALSE) {
  if (dual_delivery) {
    formula(paste(outcome, "~", SNP, "+ delivery"))
  } else {
    formula(paste(outcome, "~", SNP))
  }
}

formula_age_sex <- function(outcome,
                            SNP,
                            dual_delivery = FALSE) {
  if (dual_delivery) {
    formula(paste(outcome, "~", SNP, "+ age + sex + delivery"))
  } else {
    formula(paste(outcome, "~", SNP, "+ age + sex"))
  }
}

formula_age_sex_PC <- function(outcome,
                               SNP,
                               dual_delivery = FALSE) {
  if (dual_delivery) {
    formula(paste(
      outcome,
      "~", SNP,
      "+ age + sex + delivery +",
      paste(paste0("PC", 1:40), collapse = " + ")
    ))
  } else {
    formula(paste(
      outcome,
      "~", SNP,
      "+ age + sex +",
      paste(paste0("PC", 1:40), collapse = " + ")
    ))
  }
}


nested_tibble_to_coeff_tibble <- function(data) {
  data %>%
    mutate(model = map(model, broom::tidy)) %>%
    unnest(cols = c(model))
}

# MAIN

fit_models_additive <- function(data_path,
                                output_path) {
  base::load(data_path)

  # Add common data
  data_binary <-
    list_binary_first_instance_breastfeeding %>%
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na())

  # Unadjusted
  models_binary_unadj <-
    variants %>%
    map(
      \(SNP)
      data_binary %>%
        names() %>%
        map(\(x) data_binary[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) glm(formula_unadj(x, SNP), data = df, family = binomial(link = "logit"))),
            outcome = x,
            covariates = "Unadjusted"
          ))
    )

  models_binary_age_sex <-
    variants %>%
    map(
      \(SNP)
      data_binary %>%
        names() %>%
        map(\(x) data_binary[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) glm(formula_age_sex(x, SNP), data = df, family = binomial(link = "logit"))),
            outcome = x,
            covariates = "Adjusted for age and sex"
          ))
    )


  models_binary_age_sex_PC <-
    variants %>%
    map(
      \(SNP)
      data_binary %>%
        names() %>%
        map(\(x) data_binary[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) glm(formula_age_sex_PC(x, SNP), data = df, family = binomial(link = "logit"))),
            outcome = x,
            covariates = "Adjusted for age, sex, and PCs"
          ))
    )

  # TODO find a better alternative, this is ugly
  all_models_binary <-
    bind_rows(
      bind_rows(models_binary_unadj),
      bind_rows(models_binary_age_sex),
      bind_rows(models_binary_age_sex_PC)
    )

  all_tables_binary <-
    all_models_binary %>%
    nested_tibble_to_coeff_tibble()

  # Free some memory
  rm(data_binary)

  ## Continuous traits, single delivery mode
  # Unadjusted

  data_continuous_sd <-
    list_continuous_single_delivery_first_instance_breastfeeding %>%
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na())

  models_continuous_sd_unadj <-
    variants %>%
    map(
      \(SNP)
      data_continuous_sd %>%
        names() %>%
        map(\(x) data_continuous_sd[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_unadj(x, SNP), data = df)),
            outcome = x,
            covariates = "Unadjusted"
          ))
    )

  models_continuous_sd_age_sex <-
    variants %>%
    map(
      \(SNP)
      data_continuous_sd %>%
        names() %>%
        map(\(x) data_continuous_sd[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_age_sex(x, SNP), data = df)),
            outcome = x,
            covariates = "Adjusted for age and sex"
          ))
    )

  models_continuous_sd_age_sex_PC <-
    variants %>%
    map(
      \(SNP)
      data_continuous_sd %>%
        names() %>%
        map(\(x) data_continuous_sd[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_age_sex_PC(x, SNP), data = df)),
            outcome = x,
            covariates = "Adjusted for age, sex, and PCs"
          ))
    )

  all_models_continuous_sd <-
    bind_rows(
      bind_rows(models_continuous_sd_unadj),
      bind_rows(models_continuous_sd_age_sex),
      bind_rows(models_continuous_sd_age_sex_PC)
    )

  all_tables_continuous_sd <-
    all_models_continuous_sd %>%
    nested_tibble_to_coeff_tibble()

  # Free some memory
  rm(data_continuous_sd)

  ## Continuous traits, multiple delivery mode
  # Add common data
  data_continuous_md <-
    list_continuous_multiple_delivery_first_instance_breastfeeding %>%
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na())

  # Unadjusted
  models_continuous_md_unadj <-
    variants %>%
    map(
      \(SNP)
      data_continuous_md %>%
        names() %>%
        map(\(x) data_continuous_md[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_unadj(x, SNP, dual_delivery = TRUE), data = df)),
            outcome = x,
            covariates = "Unadjusted"
          ))
    )

  models_continuous_md_age_sex <-
    variants %>%
    map(
      \(SNP)
      data_continuous_md %>%
        names() %>%
        map(\(x) data_continuous_md[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_age_sex(x, SNP, dual_delivery = TRUE), data = df)),
            outcome = x,
            covariates = "Adjusted for age and sex"
          ))
    )

  models_continuous_md_age_sex_PC <-
    variants %>%
    map(
      \(SNP)
      data_continuous_md %>%
        names() %>%
        map(\(x) data_continuous_md[[x]] %>%
          group_by(breastfed_as_a_baby) %>%
          nest() %>%
          transmute(
            model = map(data, \(df) lm(formula_age_sex_PC(x, SNP, dual_delivery = TRUE), data = df)),
            outcome = x,
            covariates = "Adjusted for age, sex, and PCs"
          ))
    )

  all_models_continuous_md <-
    bind_rows(
      bind_rows(models_continuous_md_unadj),
      bind_rows(models_continuous_md_age_sex),
      bind_rows(models_continuous_md_age_sex_PC)
    )

  all_tables_continuous_md <-
    all_models_continuous_md %>%
    nested_tibble_to_coeff_tibble()

  # Free some memory
  rm(data_continuous_md)

  ## Prepare data for plot

  factor_mapping <-
    c(
      fluid_intelligence = "Verbal-numerical reasoning",
      proportion_correct_matrix_completion = "Matrix pattern completion",
      reaction_time_ms = "Reaction time",
      tower_rearranging_correct_answers = "Tower rearranging",
      numeric_memory_max_digits = "Numeric memory",
      pairs_matching_incorrect_matches = "Visual memory",
      paired_associate_learning_correct_word_pairs = "Paired associate learning",
      prospective_memory_binary = "Prospective memory*",
      symbol_digit_substitution_correct_answers = "Symbol digit substitution",
      trailmaking_path_1 = "Trailmaking A",
      trailmaking_path_2 = "Trailmaking B",
      trailmaking_2_minus_1 = "Trailmaking B-A",
      years_of_schooling = "Years in full-time education",
      number_of_offspring = "Number of offspring",
      bmi = "Body mass index",
      ldl_direct = "LDL direct measurement",
      hdl_direct = "HDL direct measurement",
      sbp = "Systolic blood pressure",
      dbp = "Diastolic blood pressure",
      angina = "Angina pectoris",
      heart_attack = "Myocardial infarction",
      stroke = "Stroke",
      hayfever_rhinitis_eczema = "Hayfever, rhinitis, or eczema",
      diabetes = "Diabetes"
    )

  # Potential alternative to visualise it: https://github.com/rdboyes/forester


  summary_stats <-
    bind_rows(
      all_tables_binary,
      all_tables_continuous_md,
      all_tables_continuous_sd
    )

  summary_stats_plots <-
    summary_stats %>%
    filter(
      term %in% variants
    ) %>%
    rename(
      variant = term,
      stratum = breastfed_as_a_baby
    ) %>%
    mutate(
      covariates = as_factor(covariates) %>%
        fct_relevel(
          "Unadjusted",
          "Adjusted for age and sex",
          "Adjusted for age, sex, and PCs"
        ),
      outcome = as_factor(outcome) %>%
        fct_relabel(~ factor_mapping[.x])
    )

  model_summary_binary <-
    all_models_binary %>%
    bind_rows() %>%
    mutate(model = map(model, glance)) %>%
    unnest(cols = c(model))

  model_summary_continuous <-
    bind_rows(all_models_continuous_md, all_models_continuous_sd) %>%
    mutate(model = map(model, glance)) %>%
    unnest(cols = c(model))

  base::save(summary_stats, summary_stats_plots, model_summary_binary, model_summary_continuous,
    file = output_path
  )
}

fit_models_additive(
  data_path = snakemake@input$data,
  output_path = snakemake@output$output
)
