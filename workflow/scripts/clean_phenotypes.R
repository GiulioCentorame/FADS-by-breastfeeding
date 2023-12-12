# Load packages

library(tidyr)
library(readr)
library(lubridate)
library(stringi)
library(forcats)
library(dplyr)


# Functions

longer_by_instance <- function(data) {
  pivot_longer(
    data = data,
    cols = matches(".+_f.+_0"),
    names_to = c(".value", "instance"),
    names_pattern = "(.+)_(.)_0"
  )
}

test_duplicates <- function(data) {
  if (data %>% group_by(eid) %>% filter(n() > 1) %>% nrow() != 0) {
    stop(deparse(substitute(data)), " has duplicated IDs")
  } else {
    message(deparse(substitute(data)), " has no duplicated IDs")
  }
}

test_dual_delivery <- function(data) {
  if (data %>% full_join(common_data) %>% drop_na() %>% .$delivery %>% table() %>% length() < 2) {
    stop(deparse(substitute(data)), " has data from less than 2 delivery modes")
  }
}

count_participants_long_data <- function(data) {
  data %>%
    group_by(eid) %>%
    n_groups()
}


# MAIN

clean_phenotypes <- function(data_path,
                             output_path) {
  # 335,650 observations!
  data <- readRDS(data_path)

  message("Data on ", nrow(data), " participants")

  # Define list of SNPs
  variants <- c("rs1535_A", "rs174575_C", "rs174583_C")
  variants_recessive <- c("rs1535_GG", "rs174575_GG", "rs174583_TT")

  # Simple variables to compute
  data <-
    data %>%
    mutate(
      # Dates
      date_of_birth = ym(paste(year_of_birth_f34_0_0, month_of_birth_f52_0_0)),
      age_when_fluid_intelligence_test_completed_online = interval(
        start = date_of_birth,
        end = when_fluid_intelligence_test_completed_f20135_0_0
      ) / years(1),
      age_when_numeric_memory_test_completed_online = interval(
        start = date_of_birth,
        end = when_numeric_memory_test_completed_f20138_0_0
      ) / years(1),
      age_when_symbol_digit_substitution_completed_online = interval(
        start = date_of_birth,
        end = when_symbol_digit_substitution_test_completed_f20137_0_0
      ) / years(1),
      age_when_trail_making_test_completed_online = interval(
        start = date_of_birth,
        end = when_trail_making_test_completed_f20136_0_0
      ) / years(1),
      age_when_pairs_test_completed_online = interval(
        start = date_of_birth,
        end = when_pairs_test_completed_f20134_0_0
      ) / years(1),
      # Sex (as in genetic sex)
      sex = genetic_sex_f22001_0_0 %>%
        as_factor() %>%
        fct_recode(
          Male = "1",
          Female = "0"
        )
    )

  # Checks
  if (data %>% filter(as.character(sex) != as.character(sex_f31_0_0)) %>% nrow() > 0) {
    stop("Participant sex mismatch")
  }

  # Breastfeeding
  # should lead to 335,650 observations
  breastfeeding_first_instance <-
    data %>%
    select(eid, starts_with("breastfed_as_a_baby")) %>%
    mutate(breastfed_as_a_baby = coalesce(
      breastfed_as_a_baby_f1677_0_0,
      breastfed_as_a_baby_f1677_1_0,
      breastfed_as_a_baby_f1677_2_0
    )) %>%
    select(eid, breastfed_as_a_baby) %>%
    drop_na() %>%
    mutate(breastfed_as_a_baby = case_when(
      breastfed_as_a_baby == 1 ~ "Yes",
      breastfed_as_a_baby == 0 ~ "No",
      breastfed_as_a_baby == -1 ~ "Do not know",
      breastfed_as_a_baby == -3 ~ "Prefer not to answer"
    ) %>%
      as_factor() %>%
      fct_relevel("Yes", "No", "Do not know", "Prefer not to answer") %>%
      fct_collapse(Missing = c("Do not know", "Prefer not to answer")))

  message(nrow(breastfeeding_first_instance), " participants with data on breastfeeding")
  message(
    breastfeeding_first_instance %>% filter(breastfed_as_a_baby == "Yes") %>% nrow(),
    " participants who were breastfed"
  )
  message(
    breastfeeding_first_instance %>% filter(breastfed_as_a_baby == "No") %>% nrow(),
    " participants who were bottlefed"
  )
  message(
    breastfeeding_first_instance %>% filter(breastfed_as_a_baby == "Missing") %>% nrow(),
    " participants with missing data"
  )

  # Variants
  data_variants <-
    data %>%
    select(
      eid,
      rs1535_A,
      rs174575_C,
      rs174583_C
    ) %>%
    mutate(
      rs1535_GG = as.numeric(round(2 - rs1535_A) == 2),
      rs174575_GG = as.numeric(round(2 - rs174575_C) == 2),
      rs174583_TT = as.numeric(round(2 - rs174583_C) == 2)
    )

  # Covariates
  covariates <-
    data %>%
    select(
      eid,
      # Sex (genetically-derived)
      sex,
      # principal components
      starts_with("genetic_principal_components")
    ) %>%
    rename_with(
      .fn = ~ stri_replace_first_fixed(
        .,
        "genetic_principal_components_f22009_0_",
        "PC"
      )
    )

  ### Common data to all models

  common_data <-
    full_join(
      data_variants,
      breastfeeding_first_instance,
      by = "eid"
    ) %>%
    full_join(
      covariates,
      by = "eid"
    )

  age_assessment_centre <-
    data %>%
    select(
      eid,
      contains("f21003")
    ) %>%
    longer_by_instance() %>%
    drop_na()

  descriptive_data <-
    data %>%
    select(
      eid,
      year_of_birth_f34_0_0,
      townsend_deprivation_index_at_recruitment_f189_0_0,
      genotype_measurement_batch_f22000_0_0,
      heterozygosity_pca_corrected_f22004_0_0,
      genotype_measurement_well_f22008_0_0
    ) %>%
    full_join(common_data, by = "eid")

  # Outcomes
  # HACK: most of this can be rewritten with dplyr::coalesce()
  # Intelligence

  fluid_intelligence_assessment_centre <-
    data %>%
    select(
      eid,
      starts_with("fluid_intelligence_score_f20016"),
      starts_with("age_when_attended_assessment_centre")
    ) %>%
    # Convert data into long format (3 cols: eid, FI, age, instance)
    longer_by_instance() %>%
    # Remove missing data points
    drop_na() %>%
    group_by(eid) %>%
    # Get only the earliest instance
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    # Keep cases within 3 SDs
    filter(abs(scale(fluid_intelligence_score_f20016)) < 3) %>%
    transmute(
      eid,
      fluid_intelligence_assessment_centre_raw = fluid_intelligence_score_f20016,
      fluid_intelligence_assessment_centre = scale(fluid_intelligence_score_f20016),
      age_fluid_intelligence_assessment_centre = age_when_attended_assessment_centre_f21003
    )

  fluid_intelligence_online <-
    data %>%
    select(
      eid,
      fluid_intelligence_score_f20191_0_0,
      age_when_fluid_intelligence_test_completed_online,
      fluid_intelligence_completion_status_f20242_0_0
    ) %>%
    # Keep cases within 3 SDs
    filter(abs(scale(fluid_intelligence_score_f20191_0_0)) < 3) %>%
    mutate(
      fluid_intelligence_online_raw = fluid_intelligence_score_f20191_0_0,
      fluid_intelligence_online = scale(fluid_intelligence_score_f20191_0_0),
      # If incomplete, set as NA
      fluid_intelligence_online = case_when(
        fluid_intelligence_completion_status_f20242_0_0 == 0 ~ as.numeric(fluid_intelligence_online),
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      eid,
      fluid_intelligence_online_raw,
      fluid_intelligence_online,
      age_when_fluid_intelligence_test_completed_online
    )

  fluid_intelligence <-
    full_join(fluid_intelligence_assessment_centre,
      fluid_intelligence_online,
      by = "eid"
    ) %>%
    transmute(
      eid,
      fluid_intelligence = case_when(
        !is.na(fluid_intelligence_assessment_centre) ~ fluid_intelligence_assessment_centre,
        TRUE ~ fluid_intelligence_online
      ),
      fluid_intelligence_raw = case_when(
        !is.na(fluid_intelligence_assessment_centre_raw) ~ fluid_intelligence_assessment_centre_raw,
        TRUE ~ fluid_intelligence_online_raw
      ),
      age = case_when(
        !is.na(fluid_intelligence_assessment_centre) ~ as.numeric(age_fluid_intelligence_assessment_centre),
        TRUE ~ as.numeric(age_when_fluid_intelligence_test_completed_online)
      ),
      delivery = case_when(
        !is.na(fluid_intelligence_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      )
    ) %>%
    drop_na()

  message(
    count_participants_long_data(fluid_intelligence),
    " participants with data on fluid intelligence"
  ) # 175,127

  ## Educational attainment

  # Variable levels to factor values
  # FIXME this shouldn't be necessary with ukbtools?
  coding_to_qualifications <-
    c(
      "1" = "College or University degree",
      "2" = "NVQ or HND or HNC or equivalent",
      "3" = "Other professional qualifications eg: nursing, teaching",
      "4" = "A levels/AS levels or equivalent",
      "5" = "CSEs or equivalent",
      "6" = "O levels/GCSEs or equivalent",
      "-7" = "None of the above",
      "-3" = "Prefer not to answer"
    )

  # Map degrees and years of education

  qualification_to_year_equivalence <-
    c(
      "College or University degree" = 20,
      "NVQ or HND or HNC or equivalent" = 19,
      "Other professional qualifications eg: nursing, teaching" = 15,
      "A levels/AS levels or equivalent" = 13,
      "CSEs or equivalent" = 10,
      "O levels/GCSEs or equivalent" = 10,
      "None of the above" = 7,
      "Prefer not to answer" = NA
    )

  # NOTE Years of schooling has a rectangular distribution, so outliers removal
  # based on SDs would not make any sense!
  years_of_schooling <-
    data %>%
    select(
      eid,
      starts_with("qualifications")
    ) %>%
    # Variable coding -> qualification string
    mutate(across(
      starts_with("qualifications"),
      ~ coding_to_qualifications[.x]
    )) %>%
    # Qualification string -> years
    # NOTE from here onwards, "qualification" is in number of years of education
    mutate(across(
      starts_with("qualifications"),
      ~ qualification_to_year_equivalence[.x]
    )) %>%
    pivot_longer(
      cols = matches(".+_f.+_."),
      names_to = c(".value", "instance", "entry"),
      names_pattern = "(.+)_(.)_(.)"
    ) %>%
    # Remove NAs to slim down cases to match in the next join
    drop_na() %>%
    full_join(age_assessment_centre, by = c(
      "eid",
      "instance"
    )) %>%
    # NOTE this seems redundant but it prevents A TON of warnings
    # to be printed, as max(c(NA, NA), na.rm = TRUE) returns -Inf
    drop_na() %>%
    group_by(eid) %>%
    # take highest qualification
    filter(qualifications_f6138 == max(qualifications_f6138, na.rm = TRUE)) %>%
    # when there are ties, take highest age
    filter(age_when_attended_assessment_centre_f21003 == max(age_when_attended_assessment_centre_f21003, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-c(instance, entry)) %>%
    distinct() %>%
    transmute(
      eid,
      years_of_schooling_raw = qualifications_f6138,
      years_of_schooling = scale(qualifications_f6138),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(
    count_participants_long_data(years_of_schooling),
    " participants with data on years of schooling"
  ) # 332,939

  # Number of offspring

  number_of_offspring_males <-
    data %>%
    select(
      eid,
      contains("f2405"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>% # 186948
    # Max number of children that any person has reported
    filter(number_of_children_fathered_f2405 == max(number_of_children_fathered_f2405)) %>%
    # Max age with that number of children, if there are ties
    filter(instance == max(instance)) %>%
    ungroup() %>%
    transmute(
      eid,
      number_of_offspring = case_when(
        # Negative number = no data = NA
        number_of_children_fathered_f2405 < 0 ~ NA_real_,
        # See Nicole's paper
        number_of_children_fathered_f2405 > 10 ~ 10,
        TRUE ~ as.numeric(number_of_children_fathered_f2405)
      ),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  number_of_offspring_females <-
    data %>%
    select(
      eid,
      contains("f2734"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>% # 186948
    # Max number of children that any person has reported
    filter(number_of_live_births_f2734 == max(number_of_live_births_f2734)) %>%
    # Max age with that number of children, if there are ties
    filter(instance == max(instance)) %>%
    ungroup() %>%
    transmute(eid,
      number_of_offspring = case_when(
        # Negative number = no data = NA
        number_of_live_births_f2734 < 0 ~ NA_real_,
        # See https://www.nature.com/articles/s41467-021-25723-z
        number_of_live_births_f2734 > 10 ~ 10,
        TRUE ~ as.numeric(number_of_live_births_f2734)
      ),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  number_of_offspring <-
    full_join(number_of_offspring_males, number_of_offspring_females) %>%
    # JUST TO BE 100% SURE
    filter(number_of_offspring >= 0) %>%
    filter(abs(scale(number_of_offspring)) < 3) %>%
    transmute(
      eid,
      age,
      number_of_offspring_raw = number_of_offspring,
      number_of_offspring = scale(number_of_offspring)
    ) %>%
    # Retain cases within 4 SDs
    # These are pre-scaled so I just need to filter by the abs value!
    drop_na()

  message(
    count_participants_long_data(number_of_offspring),
    " participants with data on number of offspring"
  ) # 335,648

  # Cholesterol

  total_cholesterol <-
    data %>%
    select(
      eid,
      contains("f30690"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-instance) %>%
    filter(abs(scale(cholesterol_f30690)) < 3) %>%
    transmute(
      eid,
      total_cholesterol_raw = cholesterol_f30690,
      total_cholesterol = scale(cholesterol_f30690),
      age = age_when_attended_assessment_centre_f21003,
    ) %>%
    drop_na()

  message(
    count_participants_long_data(total_cholesterol),
    " participants with data on total cholesterol"
  ) # 294,443

  ldl_direct <-
    data %>%
    select(
      eid,
      contains("f30780"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-instance) %>%
    filter(abs(scale(ldl_direct_f30780)) < 3) %>%
    transmute(
      eid,
      ldl_direct_raw = ldl_direct_f30780,
      ldl_direct = scale(ldl_direct_f30780),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(
    count_participants_long_data(ldl_direct),
    " participants with data on ldl cholesterol"
  ) # 320,302

  hdl_direct <-
    data %>%
    select(
      eid,
      contains("f30760"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-instance) %>%
    filter(abs(scale(hdl_cholesterol_f30760)) < 3) %>%
    transmute(
      eid,
      hdl_direct_raw = hdl_cholesterol_f30760,
      hdl_direct = scale(hdl_cholesterol_f30760),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(
    count_participants_long_data(hdl_direct),
    " participants with data on hdl cholesterol"
  ) # 294,443

  triglycerides <-
    data %>%
    select(
      eid,
      contains("f30870"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-instance) %>%
    filter(abs(scale(triglycerides_f30870)) < 3) %>%
    transmute(
      eid,
      triglycerides_raw = triglycerides_f30870,
      triglycerides = scale(triglycerides_f30870),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(
    count_participants_long_data(triglycerides),
    " participants with data on triglycerides"
  ) # 294,443

  # BMI
  bmi <-
    data %>%
    select(
      eid,
      contains("f21001"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-instance) %>%
    filter(abs(scale(body_mass_index_bmi_f21001)) < 3) %>%
    transmute(
      eid,
      bmi_raw = body_mass_index_bmi_f21001,
      bmi = scale(body_mass_index_bmi_f21001),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(
    count_participants_long_data(bmi),
    " participants with data on bmi"
  ) # 334,618

  # SBP
  sbp <-
    data %>%
    select(
      eid,
      SBP,
      Age_SBP
    ) %>%
    filter(abs(scale(SBP)) < 3) %>%
    transmute(
      eid,
      sbp_raw = SBP,
      sbp = scale(SBP),
      age = Age_SBP
    ) %>%
    drop_na()

  message(
    count_participants_long_data(sbp),
    " participants with data on sbp"
  ) # 335,650


  # DBP
  dbp <-
    data %>%
    select(
      eid,
      DBP,
      Age_DBP
    ) %>%
    filter(abs(scale(DBP)) < 3) %>%
    transmute(
      eid,
      dbp_raw = DBP,
      dbp = scale(DBP),
      age = Age_DBP
    )

  message(
    count_participants_long_data(dbp),
    " participants with data on dbp"
  ) # 335,650


  ## Cardiovascular outcomes

  vascular_problems_coding <-
    c(
      "1" = "Heart attack",
      "2" = "Angina",
      "3" = "Stroke",
      "4" = "High blood pressure",
      "-7" = "None of the above",
      "-3" = "Prefer not to answer"
    )

  vascular_problems <-
    data %>%
    select(
      eid,
      starts_with("vascularheart_problems_diagnosed_by_doctor_f6150")
    ) %>%
    pivot_longer(
      cols = matches(".+_f.+_."),
      names_to = c(".value", "instance", "entry"),
      names_pattern = "(.+)_(.)_(.)"
    ) %>%
    drop_na() %>%
    mutate(
      eid,
      vascularheart_problems_diagnosed_by_doctor_f6150 = vascular_problems_coding[vascularheart_problems_diagnosed_by_doctor_f6150],
      reported = 1
    ) %>%
    full_join(age_assessment_centre, by = c("eid", "instance")) %>%
    mutate(vascularheart_problems_diagnosed_by_doctor_f6150 = na_if(vascularheart_problems_diagnosed_by_doctor_f6150, "Prefer not to answer"))


  # Angina pectoris

  angina <-
    vascular_problems %>%
    group_by(eid) %>%
    summarise(
      angina = case_when(
        # If at least one of the reported values was the condition, it's affected
        any(vascularheart_problems_diagnosed_by_doctor_f6150 == "Angina", na.rm = TRUE) ~ TRUE,
        # If they're all NA, it's missing
        all(is.na(vascularheart_problems_diagnosed_by_doctor_f6150)) ~ NA,
        # Unaffected for remaining values (no reported condition and not all missing)
        TRUE ~ FALSE
      ),
      # For age, min age when reporting angina
      age = case_when(
        angina == TRUE ~ min(age_when_attended_assessment_centre_f21003, na.rm = TRUE),
        # or max age without reporting it
        angina == FALSE ~ max(age_when_attended_assessment_centre_f21003, na.rm = TRUE)
      )
    ) %>%
    drop_na()

  message(count_participants_long_data(angina), " participants with data on angina") # 335,151

  message(
    angina %>% filter(angina == TRUE) %>% nrow(), " cases, ",
    angina %>% filter(angina == FALSE) %>% nrow(), " controls."
  ) # 10,745 cases, 324,406 controls

  # Myocardial infarction

  heart_attack <-
    vascular_problems %>%
    group_by(eid) %>%
    summarise(
      heart_attack = case_when(
        # If at least one of the reported values was the condition, it's affected
        any(vascularheart_problems_diagnosed_by_doctor_f6150 == "Heart attack", na.rm = TRUE) ~ TRUE,
        # If they're all NA, it's missing
        all(is.na(vascularheart_problems_diagnosed_by_doctor_f6150)) ~ NA,
        # Unaffected for remaining values (no reported condition and not all missing)
        TRUE ~ FALSE
      ),
      # For age, min age when reporting angina
      age = case_when(
        heart_attack == TRUE ~ min(age_when_attended_assessment_centre_f21003, na.rm = TRUE),
        # or max age without reporting it
        heart_attack == FALSE ~ max(age_when_attended_assessment_centre_f21003, na.rm = TRUE)
      )
    ) %>%
    drop_na()

  message(count_participants_long_data(heart_attack), " participants with data on myocardial infarction") # 335,151

  message(
    heart_attack %>% filter(heart_attack == TRUE) %>% nrow(), " cases, ",
    heart_attack %>% filter(heart_attack == FALSE) %>% nrow(), " controls."
  ) # 7,907 cases, 327,244 controls

  # Stroke

  stroke <-
    vascular_problems %>%
    group_by(eid) %>%
    summarise(
      stroke = case_when(
        # If at least one of the reported values was the condition, it's affected
        any(vascularheart_problems_diagnosed_by_doctor_f6150 == "Stroke", na.rm = TRUE) ~ TRUE,
        # If they're all NA, it's missing
        all(is.na(vascularheart_problems_diagnosed_by_doctor_f6150)) ~ NA,
        # Unaffected for remaining values (no reported condition and not all missing)
        TRUE ~ FALSE
      ),
      # For age, min age when reporting angina
      age = case_when(
        stroke == TRUE ~ min(age_when_attended_assessment_centre_f21003, na.rm = TRUE),
        # or max age without reporting it
        stroke == FALSE ~ max(age_when_attended_assessment_centre_f21003, na.rm = TRUE)
      )
    ) %>%
    drop_na()

  message(count_participants_long_data(stroke), " participants with data on stroke") # 335,151

  message(
    stroke %>% filter(stroke == TRUE) %>% nrow(), " cases, ",
    stroke %>% filter(stroke == FALSE) %>% nrow(), " controls."
  ) # 5,628 cases, 329,883 controls

  # Hayfever, rhinitis, and eczema

  f6152_coding <-
    c(
      "5" = "Blood clot in the leg (DVT)",
      "7" = "Blood clot in the lung",
      "6" = "Emphysema/chronic bronchitis",
      "8" = "Asthma",
      "9" = "Hayfever, allergic rhinitis or eczema",
      "-7" = "None of the above",
      "-3" = "Prefer not to answer"
    )

  hayfever_rhinitis_eczema <-
    data %>%
    select(
      eid,
      starts_with("blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152")
    ) %>%
    pivot_longer(
      cols = matches(".+_f.+_."),
      names_to = c(".value", "instance", "entry"),
      names_pattern = "(.+)_(.)_(.)"
    ) %>%
    drop_na() %>%
    mutate(eid,
      blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152 = f6152_coding[blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152]
    ) %>%
    full_join(data %>% select(eid, year_of_birth_f34_0_0), by = c("eid")) %>%
    mutate(blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152 = na_if(blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152, "Prefer not to answer")) %>%
    group_by(eid) %>%
    summarise(
      hayfever_rhinitis_eczema = case_when(
        # If at least one of the reported values was the condition, it's affected
        any(blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152 == "Hayfever, allergic rhinitis or eczema", na.rm = TRUE) ~ TRUE,
        # If they're all NA, it's missing
        all(is.na(blood_clot_dvt_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diagnosed_by_doctor_f6152)) ~ NA,
        # Unaffected for remaining values (no reported condition and not all missing)
        TRUE ~ FALSE
      ),
      age = year_of_birth_f34_0_0
    ) %>%
    ungroup() %>%
    distinct() %>%
    drop_na()

  message(count_participants_long_data(hayfever_rhinitis_eczema), " participants with data on hayfever, rhinitis, eczema") # 335,151

  message(
    hayfever_rhinitis_eczema %>% filter(hayfever_rhinitis_eczema == TRUE) %>% nrow(), " cases, ",
    hayfever_rhinitis_eczema %>% filter(hayfever_rhinitis_eczema == FALSE) %>% nrow(), " controls."
  ) # 5,628 cases, 329,883 controls

  # Diabetes
  # Relying on the definition here: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0162388

  diabetes_coding <-
    c(
      "1" = "Yes",
      "0" = "No",
      # Do not know/prefer not to answer as NA
      "-1" = NA,
      "-3" = NA
    )

  insulin_coding <-
    c(
      "1" = "Yes",
      "0" = "No",
      "-1" = "Do not know",
      "-3" = "Prefer not to answer",
      # Have to do this because the coding isn't consistent
      "Yes" = "Yes",
      "No" = "No",
      "Do not know" = "Do not know",
      "Prefer not to answer" = "Prefer not to answer"
    )


  diabetes <-
    data %>%
    select(
      eid,
      contains("diabetes_diagnosed_by_doctor_f2443"),
      contains("age_diabetes_diagnosed"),
      contains("started_insulin_within_one_year_diagnosis_of_diabetes_f2986"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na(diabetes_diagnosed_by_doctor_f2443)

  message(count_participants_long_data(diabetes), " participants who responded to a question on diabetes") # 335,647

  diabetes_no_unsure <-
    diabetes %>%
    mutate(
      diabetes_diagnosed_by_doctor_f2443 = diabetes_coding[diabetes_diagnosed_by_doctor_f2443],
      started_insulin_within_one_year_diagnosis_of_diabetes_f2986 = insulin_coding[started_insulin_within_one_year_diagnosis_of_diabetes_f2986]
    ) %>%
    # Remove cases that are unsure (recoded as NA)
    drop_na(diabetes_diagnosed_by_doctor_f2443)

  message(count_participants_long_data(diabetes_no_unsure), " participants with valid data on diabetes") # 334,955

  diabetes_established_diagnosis <-
    diabetes_no_unsure %>%
    # Calculate and filter out participants diagnosed 1 year before reporting or
    # less as they might be T1D
    # NOTE: the following mutate() will generate NAs for controls!
    mutate(
      years_since_diabetes_diagnosis = age_when_attended_assessment_centre_f21003 - age_diabetes_diagnosed_f2976,
      recent_diagnosis = years_since_diabetes_diagnosis <= 1,
      # Also exclude young participants that might not have developed it quite yet
      under_35 = age_diabetes_diagnosed_f2976 < 35
    ) %>%
    # filter() excludes NAs automatically, but NAs are controls
    # NOTE again that filter() joins condition with &
    filter(
      !recent_diagnosis | is.na(recent_diagnosis),
      !under_35 | is.na(under_35),
      diabetes_diagnosed_by_doctor_f2443 == "No" | !is.na(age_diabetes_diagnosed_f2976)
    )

  message(count_participants_long_data(diabetes_established_diagnosis), " participants with a diagnosis of diabetes older than 1 year") # 330,260

  type2_diabetes <-
    diabetes_established_diagnosis %>%
    group_by(eid) %>%
    summarise(
      # Type 2: if they have reported at least once to have  diabetes and
      # have never reported to have started insulin within one year of diagnosis
      type2_diabetes = case_when(
        any(diabetes_diagnosed_by_doctor_f2443 == "Yes", na.rm = TRUE) & !any(started_insulin_within_one_year_diagnosis_of_diabetes_f2986 == "Yes", na.rm = TRUE) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Age:
      # - min age when reported diagnosis
      # - max age without diagnosis
      age = case_when(
        type2_diabetes == TRUE ~ min(age_when_attended_assessment_centre_f21003, na.rm = TRUE),
        # or max age without reporting it
        type2_diabetes == FALSE ~ max(age_when_attended_assessment_centre_f21003, na.rm = TRUE)
      )
    ) %>%
    distinct() %>%
    ungroup()

  message(count_participants_long_data(type2_diabetes), " participants with data on type II diabetes") # 330,260

  message(
    type2_diabetes %>% filter(type2_diabetes == TRUE) %>% nrow(), " cases, ",
    type2_diabetes %>% filter(type2_diabetes == FALSE) %>% nrow(), " controls."
  ) # 11,096 cases, 319,164 controls

  # Matrix pattern completion
  correct_answers_matrix_completion <-
    data %>%
    select(
      eid,
      contains("f6373"),
      contains("f6374"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    mutate(correct_answers_matrix_completion = number_of_puzzles_correctly_solved_f6373) %>%
    # Must be positive!
    filter(correct_answers_matrix_completion >= 0) %>%
    group_by(eid) %>%
    # Take first available instance
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(abs(scale(correct_answers_matrix_completion)) < 3) %>%
    transmute(
      eid,
      correct_answers_matrix_completion_raw = correct_answers_matrix_completion,
      correct_answers_matrix_completion = scale(correct_answers_matrix_completion),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(count_participants_long_data(correct_answers_matrix_completion), " participants with data on matrix pattern completion") # 19,365

  # Reaction time

  reaction_time_ms <-
    data %>%
    select(
      eid,
      contains("f20023"),
      contains("age_when_attended_assessment_centre")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    # ms can't be negative, right?
    filter(mean_time_to_correctly_identify_matches_f20023 >= 0) %>%
    group_by(eid) %>%
    # Take first available instance
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    # Trim outliers
    filter(abs(scale(mean_time_to_correctly_identify_matches_f20023)) < 3) %>%
    transmute(eid,
      reaction_time_ms_raw = mean_time_to_correctly_identify_matches_f20023,
      reaction_time_ms = scale(log(mean_time_to_correctly_identify_matches_f20023)),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(count_participants_long_data(reaction_time_ms), " participants with data on reaction time") # 333,697

  # Tower rearranging

  tower_rearranging_correct_answers <-
    data %>%
    select(
      eid,
      contains("f21004"),
      contains("f6383"),
      contains("f21003")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    mutate() %>%
    # No negative numbers, right?
    filter(number_of_puzzles_correct_f21004 >= 0) %>%
    group_by(eid) %>%
    # Take first available instance
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(abs(scale(number_of_puzzles_correct_f21004)) < 3) %>%
    transmute(eid,
      tower_rearranging_correct_answers_raw = number_of_puzzles_correct_f21004,
      tower_rearranging_correct_answers = scale(number_of_puzzles_correct_f21004),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    distinct() %>%
    drop_na()

  message(count_participants_long_data(tower_rearranging_correct_answers), " participants with data on tower rearranging") # 19,219

  # Numeric memory

  numeric_memory_assessment_centre <-
    data %>%
    select(
      eid,
      contains("f4282"),
      contains("f21003"),
      contains("f4281")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    # field 4281 has 3 options:
    # - stopped (2): the test was interrupted by the system due to
    #   too many errors (99+% participants)
    # - complete (3): the participant maxed out on the questions
    # - abandoned (9): the participant quit the test before completion
    #
    # In this case, we want to retain stopped and complete only
    filter(completion_status_of_numeric_memory_test_f4281 %in% c(2, 3)) %>%
    select(-completion_status_of_numeric_memory_test_f4281) %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(eid,
      max_digits_numeric_memory_assessment_centre = maximum_digits_remembered_correctly_f4282,
      age_when_attended_assessment_centre_f21003
    )


  numeric_memory_online <-
    data %>%
    select(
      eid,
      maximum_digits_remembered_correctly_f20240_0_0,
      age_when_numeric_memory_test_completed_online
    )

  numeric_memory_max_digits <-
    full_join(numeric_memory_assessment_centre, numeric_memory_online,
      by = "eid"
    ) %>%
    transmute(eid,
      numeric_memory_max_digits = case_when(
        !is.na(max_digits_numeric_memory_assessment_centre) ~ as.numeric(max_digits_numeric_memory_assessment_centre),
        TRUE ~ as.numeric(maximum_digits_remembered_correctly_f20240_0_0)
      ),
      age = case_when(
        !is.na(max_digits_numeric_memory_assessment_centre) ~ as.numeric(age_when_attended_assessment_centre_f21003),
        TRUE ~ as.numeric(age_when_numeric_memory_test_completed_online)
      ),
      delivery = case_when(
        !is.na(max_digits_numeric_memory_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      )
    ) %>%
    filter(abs(scale(numeric_memory_max_digits)) < 3) %>%
    mutate(
      numeric_memory_max_digits_raw = numeric_memory_max_digits,
      numeric_memory_max_digits = scale(numeric_memory_max_digits)
    ) %>%
    drop_na()

  message(count_participants_long_data(numeric_memory_max_digits), " participants with data on numeric memory") # 113,713

  # Pairs matching
  # NOTE: trickiest coding of the bunch. There are two main strategies in literature
  # --- Strategy 1 ---
  # Variables coded as in https://link.springer.com/article/10.1007/s10519-022-10127-6
  # Compute main score as follows:
  # Each correct pair earns 2 points in round 1 & 2, 1 point in round 3
  # Each incorrect pair looses 1 point
  # Within each round, negative scores are brought back to zero
  #  For that, replace NA with zero

  # age_assessment_centre <-
  #   data %>%
  #   select(eid,
  #          contains("f21003")) %>%
  #   longer_by_instance() %>%
  #   drop_na()
  #
  # #pairs_matching_scoring1_assessment_centre <-
  # data %>%
  #   select(eid,
  #          matches("f399"),
  #          contains("f398")) %>%
  #   pivot_longer(cols = matches(".+_f.+_."),
  #                names_to = c(".value", "instance", "round"),
  #                names_pattern = "(.+)_(.)_(.)"
  #                ) %>%
  #   full_join(age_assessment_centre) %>%
  #   mutate(correct_points = case_when(round == 3...))

  # --- Strategy 2 ---
  # Also done in the alternative way as in
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4844168/
  # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0231627
  # N of errors in round 2 (6 pairs), then ln+1
  pairs_matching_scoring2_assessment_centre <-
    data %>%
    select(
      eid,
      # field with incorrect answers
      matches("f399_._2"),
      # age
      contains("f21003")
    ) %>%
    pivot_longer(
      cols = matches(".+_f.+_."),
      names_to = c(".value", "instance"),
      names_pattern = "(.+)_(.)_."
    ) %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(eid,
      incorrect_matches_round_2_assessment_centre = number_of_incorrect_matches_in_round_f399,
      age_assessment_centre = age_when_attended_assessment_centre_f21003
    )

  pairs_matching_scoring2_online <-
    data %>%
    select(
      eid,
      matches("f20132"),
      age_when_pairs_test_completed_online
    ) %>%
    pivot_longer(
      cols = matches(".+_f.+_."),
      names_to = c(".value", "round"),
      names_pattern = "(.+)_._(.)"
    ) %>%
    drop_na() %>%
    filter(round == 2) %>%
    select(-round) %>%
    rename(
      incorrect_matches_round_2_online = number_of_incorrect_matches_in_round_f20132,
      age_online = age_when_pairs_test_completed_online
    )

  pairs_matching_incorrect_matches <-
    full_join(
      pairs_matching_scoring2_assessment_centre,
      pairs_matching_scoring2_online
    ) %>%
    transmute(eid,
      pairs_matching_incorrect_matches = case_when(
        !is.na(incorrect_matches_round_2_assessment_centre) ~ incorrect_matches_round_2_assessment_centre,
        TRUE ~ incorrect_matches_round_2_online
      ),
      age = case_when(
        !is.na(incorrect_matches_round_2_assessment_centre) ~ as.numeric(age_assessment_centre),
        TRUE ~ as.numeric(age_online)
      ),
      delivery = case_when(
        !is.na(incorrect_matches_round_2_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      )
    ) %>%
    drop_na() %>% # prevents -Inf
    # log(x+1) transform
    mutate(pairs_matching_incorrect_matches = log(pairs_matching_incorrect_matches + 1)) %>%
    # Remove outliers
    filter(abs(scale(pairs_matching_incorrect_matches)) < 3) %>%
    # transform in SD units
    mutate(
      pairs_matching_incorrect_matches_raw = pairs_matching_incorrect_matches,
      pairs_matching_incorrect_matches = scale(pairs_matching_incorrect_matches)
    ) %>%
    drop_na()

  message(count_participants_long_data(pairs_matching_incorrect_matches), " participants with data on visual memory") # 335,650

  # Paired associate learning
  # NOTE: this has a massive ceiling effect

  paired_associate_learning_correct_word_pairs <-
    data %>%
    select(
      eid,
      contains("f20197"),
      contains("f21003")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = FALSE)) %>%
    ungroup() %>%
    filter(number_of_word_pairs_correctly_associated_f20197 >= 0) %>%
    filter(abs(scale(number_of_word_pairs_correctly_associated_f20197)) < 3) %>%
    transmute(
      eid,
      paired_associate_learning_correct_word_pairs_raw = number_of_word_pairs_correctly_associated_f20197,
      paired_associate_learning_correct_word_pairs = scale(number_of_word_pairs_correctly_associated_f20197),
      age = age_when_attended_assessment_centre_f21003
    ) %>%
    drop_na()

  message(count_participants_long_data(paired_associate_learning_correct_word_pairs), " participants with data on paired associate learning") # 19,573

  # Prospective memory
  # 0 = skipped, incorrect, or missing
  # 1 = correct at 1st attempt
  # 2 = correct at 2nd attempt
  # NOTE: here scored in two ways
  # 1) 3 levels, with  0 if skipped, 1 if correct
  #    at 2st attempt, 2 if correct at 1st
  # 2) Binary, 1 = 1st attempt, 0 = 2nd or skipped, incorrect,
  #    or missing answer

  prospective_memory_binary <-
    data %>%
    select(
      eid,
      contains("f20018"),
      contains("f21003"),
      contains("f4287")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    # 3 == completed
    # 9 == abandoned
    filter(test_completion_status_f4287 == 3) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(
      eid,
      age = age_when_attended_assessment_centre_f21003,
      prospective_memory_result_3point = case_when(
        # If 1 attempt, 2 points
        prospective_memory_result_f20018 == 1 ~ 2,
        # If 2 attempts, 1 point
        prospective_memory_result_f20018 == 2 ~ 1,
        prospective_memory_result_f20018 == 0 ~ 0,
        TRUE ~ as.numeric(TRUE)
      ),
      prospective_memory_result_binary = case_when(
        prospective_memory_result_f20018 == 2 ~ 0,
        TRUE ~ as.numeric(TRUE)
      )
    ) %>%
    transmute(eid,
      prospective_memory_binary = prospective_memory_result_binary,
      age
    ) %>%
    drop_na()

  message(count_participants_long_data(prospective_memory_binary), " participants with data on prospective memory") # 136,838
  message(
    prospective_memory_binary %>% filter(prospective_memory_binary == 1) %>% nrow(), " participants recalled the task correctly, ",
    prospective_memory_binary %>% filter(prospective_memory_binary == 0) %>% nrow(), " did not recall the task correctly"
  ) # 115,431 correct recall, 21,407 incorrect recal

  ### Symbol digit substitution
  # Unit: correct answers
  # Source: https://www.sciencedirect.com/science/article/pii/S0160289619300789
  symbol_digit_substitution_correct_answers_assessment_centre <-
    data %>%
    select(
      eid,
      contains("f23324"),
      contains("f23323"),
      contains("f21003")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(eid,
      age_assessment_centre = age_when_attended_assessment_centre_f21003,
      symbol_digit_substitution_correct_answers_assessment_centre = number_of_symbol_digit_matches_made_correctly_f23324
    )

  symbol_digit_substitution_correct_answers_online <-
    data %>%
    select(
      eid,
      contains("f20159"),
      contains("f20245"),
      age_when_symbol_digit_substitution_completed_online
    ) %>%
    filter(symbol_digit_completion_status_f20245_0_0 == 0) %>%
    select(-symbol_digit_completion_status_f20245_0_0) %>%
    rename(symbol_digit_substitution_correct_answers_online = number_of_symbol_digit_matches_made_correctly_f20159_0_0)

  symbol_digit_substitution_correct_answers <-
    full_join(
      symbol_digit_substitution_correct_answers_online,
      symbol_digit_substitution_correct_answers_assessment_centre
    ) %>%
    transmute(eid,
      symbol_digit_substitution_correct_answers = case_when(
        !is.na(symbol_digit_substitution_correct_answers_assessment_centre) ~ symbol_digit_substitution_correct_answers_assessment_centre,
        TRUE ~ symbol_digit_substitution_correct_answers_online
      ),
      age = case_when(
        !is.na(symbol_digit_substitution_correct_answers_assessment_centre) ~ as.numeric(age_assessment_centre),
        TRUE ~ as.numeric(age_when_symbol_digit_substitution_completed_online)
      ),
      delivery = case_when(
        !is.na(symbol_digit_substitution_correct_answers_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      )
    ) %>%
    # Must be positive
    filter(symbol_digit_substitution_correct_answers >= 0) %>%
    # Trim outliers
    filter(abs(scale(symbol_digit_substitution_correct_answers)) < 3) %>%
    # Rescale in SD units
    mutate(
      symbol_digit_substitution_correct_answers_raw = symbol_digit_substitution_correct_answers,
      symbol_digit_substitution_correct_answers = scale(symbol_digit_substitution_correct_answers)
    ) %>%
    drop_na()

  message(count_participants_long_data(symbol_digit_substitution_correct_answers), " participants with data on symbol digit substitution") # 106,881

  # Trailmaking
  # AC: deciseconds, online: seconds. 0 = test was not completed, so NA
  # alternative scoring can be -log(trail b/trail a)

  trailmaking_assessment_centre <-
    data %>%
    select(
      eid,
      contains("f6348"),
      contains("f6350"),
      contains("f21003")
    ) %>%
    longer_by_instance() %>%
    drop_na() %>%
    group_by(eid) %>%
    filter(instance == min(instance, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      ## Round 1
      # 0 means incomplete, so NA
      duration_to_complete_numeric_path_trail_1_f6348 = na_if(duration_to_complete_numeric_path_trail_1_f6348, 0),
      # Convert deciseconds into seconds, then round it
      duration_to_complete_numeric_path_trail_1_f6348 = duration_to_complete_numeric_path_trail_1_f6348 / 10,
      ## Round 2
      # 0 means incomplete, so NA
      duration_to_complete_alphanumeric_path_trail_2_f6350 = na_if(duration_to_complete_alphanumeric_path_trail_2_f6350, 0),
      # Convert deciseconds into seconds, then round it
      duration_to_complete_alphanumeric_path_trail_2_f6350 = duration_to_complete_alphanumeric_path_trail_2_f6350 / 10
    ) %>%
    transmute(eid,
      duration_numeric_path_trail_1_assessment_centre = duration_to_complete_numeric_path_trail_1_f6348,
      duration_numeric_path_trail_2_assessment_centre = duration_to_complete_alphanumeric_path_trail_2_f6350,
      age_when_attended_assessment_centre_f21003
    )

  # trailmaking_online
  trailmaking_online <-
    data %>%
    select(
      eid,
      contains("f20156"),
      contains("f20157"),
      contains("f20246"),
      age_when_trail_making_test_completed_online
    ) %>%
    filter(trail_making_completion_status_f20246_0_0 == 0) %>%
    select(-trail_making_completion_status_f20246_0_0) %>%
    drop_na()

  trailmaking_path_1_unfiltered <-
    full_join(trailmaking_assessment_centre, trailmaking_online, by = "eid") %>%
    select(
      eid,
      duration_numeric_path_trail_1_assessment_centre,
      age_when_attended_assessment_centre_f21003,
      duration_to_complete_numeric_path_trail_1_f20156_0_0,
      age_when_trail_making_test_completed_online
    ) %>%
    transmute(eid,
      trailmaking_path_1 = case_when(
        !is.na(duration_numeric_path_trail_1_assessment_centre) ~ duration_numeric_path_trail_1_assessment_centre,
        TRUE ~ duration_to_complete_numeric_path_trail_1_f20156_0_0
      ),
      age = case_when(
        !is.na(duration_numeric_path_trail_1_assessment_centre) ~ as.numeric(age_when_attended_assessment_centre_f21003),
        TRUE ~ as.numeric(age_when_trail_making_test_completed_online)
      ),
      delivery = case_when(
        !is.na(duration_numeric_path_trail_1_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      )
    ) %>%
    drop_na()

  message(count_participants_long_data(trailmaking_path_1_unfiltered), " participants with valid data on trailmaking path 1") # 83,591

  trailmaking_path_1 <-
    trailmaking_path_1_unfiltered %>%
    filter(trailmaking_path_1 < 250) %>%
    filter(abs(scale(log(trailmaking_path_1))) < 3) %>%
    drop_na()

  message(count_participants_long_data(trailmaking_path_1), " participants with data on trailmaking path 1 after removing cases over 250 s") # 83,589

  trailmaking_path_2_unfiltered <-
    full_join(trailmaking_assessment_centre, trailmaking_online, by = "eid") %>%
    select(
      eid,
      duration_numeric_path_trail_2_assessment_centre,
      age_when_attended_assessment_centre_f21003,
      duration_to_complete_alphanumeric_path_trail_2_f20157_0_0,
      age_when_trail_making_test_completed_online
    ) %>%
    transmute(eid,
      trailmaking_path_2 = case_when(
        !is.na(duration_numeric_path_trail_2_assessment_centre) ~ duration_numeric_path_trail_2_assessment_centre,
        TRUE ~ duration_to_complete_alphanumeric_path_trail_2_f20157_0_0
      ),
      age = case_when(
        !is.na(duration_numeric_path_trail_2_assessment_centre) ~ as.numeric(age_when_attended_assessment_centre_f21003),
        TRUE ~ as.numeric(age_when_trail_making_test_completed_online)
      ),
      delivery = case_when(
        !is.na(duration_numeric_path_trail_2_assessment_centre) ~ "Assessment centre",
        TRUE ~ "Online"
      ),
    ) %>%
    drop_na()

  message(count_participants_long_data(trailmaking_path_2_unfiltered), " participants with valid data on trailmaking path 2") # 83,244

  trailmaking_path_2 <-
    trailmaking_path_2_unfiltered %>%
    filter(trailmaking_path_2 < 250) %>%
    filter(abs(scale(log(trailmaking_path_2))) < 3)

  message(count_participants_long_data(trailmaking_path_2), " participants with data on trailmaking path 2 after removing cases over 250 s") # 83,185

  trailmaking_2_minus_1 <-
    inner_join(trailmaking_path_1, trailmaking_path_2) %>%
    # Make sure we have complete cases on both trails
    drop_na() %>%
    transmute(eid,
      trailmaking_2_minus_1 = trailmaking_path_2 - trailmaking_path_1,
      delivery,
      age = age
    ) %>%
    # Trim outliers
    filter(abs(scale(trailmaking_2_minus_1)) < 3) %>%
    # Rescale in SD units
    mutate(
      trailmaking_2_minus_1_raw = trailmaking_2_minus_1,
      trailmaking_2_minus_1 = scale(trailmaking_2_minus_1)
    ) %>%
    drop_na()


  message(count_participants_long_data(trailmaking_2_minus_1), " participants with data on trailmaking 2-1") # 83,049


  trailmaking_path_1 <-
    trailmaking_path_1 %>%
    drop_na() %>% # prevents -Inf
    mutate(
      trailmaking_path_1_raw = trailmaking_path_1,
      trailmaking_path_1 = scale(log(trailmaking_path_1))
    )

  trailmaking_path_2 <-
    trailmaking_path_2 %>% # prevents -Inf
    drop_na() %>%
    mutate(
      trailmaking_path_2_raw = trailmaking_path_2,
      trailmaking_path_2 = scale(log(trailmaking_path_2))
    )

  ## TESTS
  # No duplicates
  test_duplicates(fluid_intelligence)
  test_duplicates(years_of_schooling)
  test_duplicates(number_of_offspring)
  test_duplicates(total_cholesterol)
  test_duplicates(triglycerides)
  test_duplicates(ldl_direct)
  test_duplicates(ldl_direct)
  test_duplicates(bmi)
  test_duplicates(sbp)
  test_duplicates(dbp)
  test_duplicates(angina)
  test_duplicates(heart_attack)
  test_duplicates(stroke)
  test_duplicates(hayfever_rhinitis_eczema)
  test_duplicates(type2_diabetes)
  test_duplicates(correct_answers_matrix_completion)
  test_duplicates(reaction_time_ms)
  test_duplicates(tower_rearranging_correct_answers)
  test_duplicates(numeric_memory_max_digits)
  test_duplicates(pairs_matching_incorrect_matches)
  test_duplicates(paired_associate_learning_correct_word_pairs)
  test_duplicates(prospective_memory_binary)
  test_duplicates(symbol_digit_substitution_correct_answers)
  test_duplicates(trailmaking_path_1)
  test_duplicates(trailmaking_path_2)
  test_duplicates(trailmaking_2_minus_1)

  ### Create lists of datasets
  ## List 1
  # Continuous outcome, multiple delivery mode covariate, first available instance breastfeeding
  # Extra variable named "delivery"

  list_continuous_multiple_delivery_first_instance_breastfeeding <- list(
    fluid_intelligence = fluid_intelligence,
    numeric_memory_max_digits = numeric_memory_max_digits,
    symbol_digit_substitution_correct_answers = symbol_digit_substitution_correct_answers,
    trailmaking_path_1 = trailmaking_path_1,
    trailmaking_path_2 = trailmaking_path_2,
    trailmaking_2_minus_1 = trailmaking_2_minus_1 # ,
    # pairs_matching_incorrect_matches = pairs_matching_incorrect_matches
  )

  ## List 2
  # Continuous outcome, only one delivery mode, first available instance breastfeeding

  list_continuous_single_delivery_first_instance_breastfeeding <-
    list(
      years_of_schooling = years_of_schooling,
      number_of_offspring = number_of_offspring,
      total_cholesterol = total_cholesterol,
      ldl_direct = ldl_direct,
      hdl_direct = hdl_direct,
      triglycerides = triglycerides,
      bmi = bmi,
      sbp = sbp,
      dbp = dbp,
      correct_answers_matrix_completion = correct_answers_matrix_completion,
      reaction_time_ms = reaction_time_ms,
      tower_rearranging_correct_answers = tower_rearranging_correct_answers,
      paired_associate_learning_correct_word_pairs = paired_associate_learning_correct_word_pairs
    )

  ## List 3
  # Binary outcome (= logistic regression), first available instance breastfeeding

  list_binary_first_instance_breastfeeding <-
    list(
      angina = angina,
      heart_attack = heart_attack,
      stroke = stroke,
      hayfever_rhinitis_eczema = hayfever_rhinitis_eczema,
      type2_diabetes = type2_diabetes # ,
      # prospective_memory_binary = prospective_memory_binary
    )


  ## Write data
  base::save(
    variants,
    variants_recessive,
    common_data,
    descriptive_data,
    list_continuous_multiple_delivery_first_instance_breastfeeding,
    list_continuous_single_delivery_first_instance_breastfeeding,
    list_binary_first_instance_breastfeeding,
    file = output_path
  )
}

clean_phenotypes(
  data_path = snakemake@input$data,
  output_path = snakemake@output$output
)
