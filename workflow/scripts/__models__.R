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

# Interaction models

formula_interactions_unadj <-
  function(outcome,
           SNP,
           dual_delivery = FALSE) {
    main_formula <-
      paste(
        outcome,
        "~",
        paste0(SNP, "*breastfed_as_a_baby")
      )

    # Add delivery term if needed
    if (dual_delivery) {
      paste(main_formula, "+ delivery")
    } else {
      main_formula
    }
  }

formula_interactions_age_sex <-
  function(outcome,
           SNP,
           dual_delivery = FALSE) {
    main_formula <-
      paste(
        formula_interactions_unadj(
          outcome = outcome,
          SNP = SNP,
          dual_delivery = FALSE
        ),
        "+ age + sex"
      )

    if (dual_delivery) {
      paste(main_formula, "+ delivery")
    } else {
      main_formula
    }
  }

formula_interactions_age_sex_PC <-
  function(outcome,
           SNP,
           dual_delivery = FALSE) {
    main_formula <-
      paste(
        formula_interactions_age_sex(
          outcome = outcome,
          SNP = SNP,
          dual_delivery = FALSE
        ),
        "+",
        paste(paste0("PC", 1:40), collapse = " + ")
      )

    if (dual_delivery) {
      paste(main_formula, "+ delivery")
    } else {
      main_formula
    }
  }

formula_interactions_age_sex_PC_pairwise_interactions <-
  function(outcome,
           SNP,
           dual_delivery = FALSE) {
    main_formula <-
      paste(
        outcome,
        "~ (", SNP, "+ breastfed_as_a_baby + age + sex)^2 +",
        paste(
          paste0("PC", 1:40),
          collapse = " + "
        ), "+",
        # Interaction with pcs
        # - breastfeeding
        paste0(
          paste0("PC", 1:40),
          "*breastfed_as_a_baby",
          collapse = " + "
        ), "+",
        # - SNP
        paste0(
          paste0("PC", 1:40),
          "*", SNP,
          collapse = " + "
        ), "+",
        # - sex
        paste0(
          paste0("PC", 1:40),
          "*sex",
          collapse = " + "
        ), "+",
        # - age
        paste0(
          paste0("PC", 1:40),
          "*age",
          collapse = " + "
        )
      )

    if (dual_delivery) {
      paste(main_formula, "+ delivery")
    } else {
      main_formula
    }
  }
