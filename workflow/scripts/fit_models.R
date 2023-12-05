library(dplyr)
library(purrr)
library(ggplot2)
library(forcats)
library(tidyr)
library(broom)

### Functions
snakemake@source("__models__.R")

nested_tibble_to_coeff_tibble <- function(data) {
  data %>%
    mutate(model = map(model, broom::tidy)) %>%
    unnest(cols = c(model))
}

# MAIN

fit_models <- function(data_path,
                       recessive,
                       output_path) {
  base::load(data_path)

  # Overwrite variant names with the ones using
  # recessive coding when fitting recessive models
  if (recessive) {
    variants <- variants_recessive
  }

  #######################
  #### Binary traits ####
  #######################

  # Create datasets for analysis
  data_binary <-
    list_binary_first_instance_breastfeeding %>%
    # Add common data and restrict to complete cases
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na()) %>%
    map(
      \(x) x %>%
        mutate(breastfed_as_a_baby = "Total") %>%
        # Merge data twice: one with all cases as "Total" (dot)
        # the other one with the actual breastfeeding staus (x)
        bind_rows(
          total = .,
          stratified = x,
          .id = "type"
        )
    )

  ## Stratified models ##
  #######################

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
            model = map(data, \(df) glm(formula_unadj(x, SNP),
              data = df,
              family = binomial(link = "logit")
            )),
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
            model = map(data, \(df) glm(formula_age_sex(x, SNP),
              data = df,
              family = binomial(link = "logit")
            )),
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
            model = map(data, \(df) glm(formula_age_sex_PC(x, SNP),
              data = df,
              family = binomial(link = "logit")
            )),
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

  ## Interaction models ##
  ########################

  data_interactions_binary <-
    data_binary %>%
    map(\(x) filter(x, breastfed_as_a_baby %in% c("Yes", "No"))) %>%
    map(\(x) mutate(x,
      breastfed_as_a_baby = case_match(
        breastfed_as_a_baby,
        "Yes" ~ 1,
        "No" ~ 0
      )
    ))

  # NOTE I should redo most scripts
  table_permutations_binary <-
    expand.grid(
      traits = names(data_interactions_binary),
      variants = variants
    )

  models_interactions_unadjusted_binary <-
    map2(
      table_permutations_binary$traits,
      table_permutations_binary$variants,
      \(trait, SNP)
      glm(
        formula(formula_interactions_unadj(trait, SNP)),
        data = data_interactions_binary[[trait]],
        family = binomial(link = "logit")
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Unadjusted"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_binary <-
    map2(
      table_permutations_binary$traits,
      table_permutations_binary$variants,
      \(trait, SNP)
      glm(
        formula(formula_interactions_age_sex(trait, SNP)),
        data = data_interactions_binary[[trait]],
        family = binomial(link = "logit")
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age and sex"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_PC_binary <-
    map2(
      table_permutations_binary$traits,
      table_permutations_binary$variants,
      \(trait, SNP)
      glm(
        formula(formula_interactions_age_sex_PC(trait, SNP)),
        data = data_interactions_binary[[trait]],
        family = binomial(link = "logit")
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, and PCs"
        )
    ) %>%
    bind_rows()

  models_keller_interactions_binary <-
    map2(
      table_permutations_binary$traits,
      table_permutations_binary$variants,
      \(trait, SNP)
      glm(
        formula(formula_interactions_age_sex_PC_pairwise_interactions(trait, SNP)),
        data = data_interactions_binary[[trait]],
        family = binomial(link = "logit")
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, PCs, and pairwise interactions"
        )
    ) %>%
    bind_rows()

  all_tables_interactions_binary <-
    bind_rows(
      bind_rows(models_interactions_unadjusted_binary),
      bind_rows(models_interactions_age_sex_binary),
      bind_rows(models_interactions_age_sex_PC_binary),
      bind_rows(models_keller_interactions_binary)
    )

  # Free some memory
  rm(data_binary)

  #################################################
  #### Continuous traits, single delivery mode ####
  #################################################

  ## Stratified models ##
  #######################

  # Unadjusted

  data_continuous_sd <-
    list_continuous_single_delivery_first_instance_breastfeeding %>%
    # Add common data and restrict to complete cases
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na()) %>%
    map(
      \(x) x %>%
        mutate(breastfed_as_a_baby = "Total") %>%
        # Merge data twice: one with all cases as "Total" (dot)
        # the other one with the actual breastfeeding staus (x)
        bind_rows(
          total = .,
          stratified = x,
          .id = "type"
        )
    )


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

  ## Interaction models ##
  ########################

  data_interactions_continuous_sd <-
    data_continuous_sd %>%
    map(\(x) filter(x, breastfed_as_a_baby %in% c("Yes", "No"))) %>%
    map(\(x) mutate(x,
      breastfed_as_a_baby = case_match(
        breastfed_as_a_baby,
        "Yes" ~ 1,
        "No" ~ 0
      )
    ))

  # NOTE I should redo most scripts
  table_permutations_continuous_sd <-
    expand.grid(
      traits = names(data_interactions_continuous_sd),
      variants = variants
    )

  models_interactions_unadjusted_continuous_sd <-
    map2(
      table_permutations_continuous_sd$traits,
      table_permutations_continuous_sd$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_unadj(trait, SNP)),
        data = data_interactions_continuous_sd[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Unadjusted"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_continuous_sd <-
    map2(
      table_permutations_continuous_sd$traits,
      table_permutations_continuous_sd$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex(trait, SNP)),
        data = data_interactions_continuous_sd[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age and sex"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_PC_continuous_sd <-
    map2(
      table_permutations_continuous_sd$traits,
      table_permutations_continuous_sd$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex_PC(trait, SNP)),
        data = data_interactions_continuous_sd[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, and PCs"
        )
    ) %>%
    bind_rows()

  models_keller_interactions_continuous_sd <-
    map2(
      table_permutations_continuous_sd$traits,
      table_permutations_continuous_sd$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex_PC_pairwise_interactions(trait, SNP)),
        data = data_interactions_continuous_sd[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, PCs, and pairwise interactions"
        )
    ) %>%
    bind_rows()

  all_tables_interactions_continuous_sd <-
    bind_rows(
      bind_rows(models_interactions_unadjusted_continuous_sd),
      bind_rows(models_interactions_age_sex_continuous_sd),
      bind_rows(models_interactions_age_sex_PC_continuous_sd),
      bind_rows(models_keller_interactions_continuous_sd)
    )

  # Free some memory
  rm(data_continuous_sd)

  ###################################################
  #### Continuous traits, multiple delivery mode ####
  ###################################################

  data_continuous_md <-
    list_continuous_multiple_delivery_first_instance_breastfeeding %>%
    map(\(x) full_join(x, common_data, by = "eid") %>%
      drop_na()) %>%
    map(
      \(x) x %>%
        mutate(breastfed_as_a_baby = "Total") %>%
        bind_rows(
          total = .,
          stratified = x,
          .id = "type"
        )
    )

  ## Stratified models ##
  #######################

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

  ## Interaction models ##
  ########################

  data_interactions_continuous_md <-
    data_continuous_md %>%
    map(\(x) filter(x, breastfed_as_a_baby %in% c("Yes", "No"))) %>%
    map(\(x) mutate(x,
      breastfed_as_a_baby = case_match(
        breastfed_as_a_baby,
        "Yes" ~ 1,
        "No" ~ 0
      )
    ))

  table_permutations_continuous_md <-
    expand.grid(
      traits = names(data_interactions_continuous_md),
      variants = variants
    )

  models_interactions_unadjusted_continuous_md <-
    map2(
      table_permutations_continuous_md$traits,
      table_permutations_continuous_md$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_unadj(trait, SNP)),
        data = data_interactions_continuous_md[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Unadjusted"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_continuous_md <-
    map2(
      table_permutations_continuous_md$traits,
      table_permutations_continuous_md$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex(trait, SNP)),
        data = data_interactions_continuous_md[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age and sex"
        )
    ) %>%
    bind_rows()

  models_interactions_age_sex_PC_continuous_md <-
    map2(
      table_permutations_continuous_md$traits,
      table_permutations_continuous_md$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex_PC(trait, SNP)),
        data = data_interactions_continuous_md[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, and PCs"
        )
    ) %>%
    bind_rows()

  models_keller_interactions_continuous_md <-
    map2(
      table_permutations_continuous_md$traits,
      table_permutations_continuous_md$variants,
      \(trait, SNP)
      lm(
        formula(formula_interactions_age_sex_PC_pairwise_interactions(trait, SNP)),
        data = data_interactions_continuous_md[[trait]]
      ) %>%
        broom::tidy() %>%
        mutate(
          outcome = trait,
          covariates = "Adjusted for age, sex, PCs, and pairwise interactions"
        )
    ) %>%
    bind_rows()

  all_tables_interactions_continuous_md <-
    bind_rows(
      bind_rows(models_interactions_unadjusted_continuous_md),
      bind_rows(models_interactions_age_sex_continuous_md),
      bind_rows(models_interactions_age_sex_PC_continuous_md),
      bind_rows(models_keller_interactions_continuous_md)
    )

  # Free some memory
  rm(data_continuous_md)

  ## Prepare data for plot

  factor_mapping <-
    c(
      fluid_intelligence = "Verbal-numerical reasoning",
      correct_answers_matrix_completion = "Matrix pattern completion",
      reaction_time_ms = "Reaction time",
      tower_rearranging_correct_answers = "Tower rearranging",
      numeric_memory_max_digits = "Numeric memory",
      pairs_matching_incorrect_matches = "Visual memory",
      paired_associate_learning_correct_word_pairs = "Paired associate learning",
      prospective_memory_binary = "Prospective memory",
      symbol_digit_substitution_correct_answers = "Symbol digit substitution",
      trailmaking_path_1 = "Trailmaking A",
      trailmaking_path_2 = "Trailmaking B",
      trailmaking_2_minus_1 = "Trailmaking B-A",
      years_of_schooling = "Years in full-time education",
      number_of_offspring = "Number of offspring",
      bmi = "Body mass index",
      total_cholesterol = "Total cholesterol",
      triglycerides = "Triglycerides",
      ldl_direct = "LDL direct measurement",
      hdl_direct = "HDL direct measurement",
      sbp = "Systolic blood pressure",
      dbp = "Diastolic blood pressure",
      angina = "Angina pectoris",
      heart_attack = "Myocardial infarction",
      stroke = "Stroke",
      hayfever_rhinitis_eczema = "Hayfever, rhinitis, or eczema",
      type2_diabetes = "Type-II diabetes"
    )

  # Potential alternative to visualise it: https://github.com/rdboyes/forester


  summary_stats <-
    bind_rows(
      all_tables_binary,
      all_tables_continuous_md,
      all_tables_continuous_sd
    )

  summary_stats_interactions <-
    bind_rows(
      all_tables_interactions_binary,
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

  summary_stats_plots_interactions <-
    summary_stats_interactions %>%
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
          "Adjusted for age, sex, and PCs",
          "Adjusted for age, sex, PCs, and pairwise interactions"
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

  model_summary_interactions <-
    bind_rows(
      all_tables_interactions_binary,
      all_tables_interactions_continuous_md,
      all_tables_interactions_continuous_sd
    ) %>%
    mutate(
      outcome = as_factor(outcome) %>%
        fct_relabel(~ factor_mapping[.x])
    )

  base::save(
    summary_stats,
    summary_stats_plots,
    summary_stats_interactions,
    summary_stats_plots_interactions,
    model_summary_binary,
    model_summary_continuous,
    model_summary_interactions,
    file = output_path
  )
}

fit_models(
  data_path = snakemake@input$data,
  # recessive needs logical values
  recessive = snakemake@params$recessive,
  output_path = snakemake@output$output
)
