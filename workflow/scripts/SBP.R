##################################################################################################################################
######
###### Program:             PhenotypePreparationSBP.R
###### Written by:		      Geng Wang
###### R packages loaded:	  dyplr
###### Datafiles :		      ukb6221.csv
###### Objective :		      select field and clean files
######
##################################################################################################################################

library(dplyr)

extract_SBP <- function(raw_data_path,
                        output_path) {
  ######################
  ### Open datafiles ###
  ######################
  ukb6221 <- read.csv(raw_data_path, header = T, sep = ",", check.names = FALSE)
  dim(ukb6221)

  ######################
  ### Select fields  ###
  ######################
  ukb6221_SBP <- select(ukb6221, c(eid, "31-0.0", "34-0.0", "54-0.0":"54-2.0", "93-0.0":"93-2.1", "4080-0.0":"4080-2.1", "6153-0.0":"6153-2.3", "6177-0.0":"6177-2.2", "21003-0.0", "21003-1.0", "53-0.0":"53-2.0"))

  ######################
  ### Summeray SBP_at 0.0 #
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP %>% mutate("4080-0" = ifelse
  (is.na(ukb6221_SBP$`4080-0.0`) & is.na(ukb6221_SBP$`4080-0.1`), NA,
    ifelse(is.na(ukb6221_SBP$`4080-0.1`), ukb6221_SBP$"4080-0.0",
      ifelse(is.na(ukb6221_SBP$`4080-0.0`), ukb6221_SBP$"4080-0.1",
        rowMeans(ukb6221_SBP[, c("4080-0.0", "4080-0.1")])
      )
    )
  ))

  ## summary(!is.na(ukb6221_SBP$`4080-0.0`)&is.na(ukb6221_SBP$`4080-0.1`))
  ## summary(!is.na(ukb6221_SBP$`4080-1.0`)&is.na(ukb6221_SBP$`4080-1.1`))
  ## summary(!is.na(ukb6221_SBP$`4080-2.0`)&is.na(ukb6221_SBP$`4080-2.1`))
  ##
  ######################
  ### Outlier SBP_at 0.0 ##
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("4080-0" = ifelse(
    !is.na(ukb6221_SBP_0$`4080-0.0`) & !is.na(ukb6221_SBP_0$`4080-0.1`) &
      abs(ukb6221_SBP_0$`4080-0.0` - ukb6221_SBP_0$`4080-0.1`) > 4.56 * sd(ukb6221_SBP_0$`4080-0`, na.rm = T),
    NA, ukb6221_SBP_0$`4080-0`
  ))


  ######################
  ### Medication 0.0 ###
  ######################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "4080-0" =
      ifelse(is.na(ukb6221_SBP_0$`4080-0`) |
        (is.na(ukb6221_SBP_0$`6153-0.0`) &
          is.na(ukb6221_SBP_0$`6153-0.1`) &
          is.na(ukb6221_SBP_0$`6153-0.2`) &
          is.na(ukb6221_SBP_0$`6153-0.3`) &
          is.na(ukb6221_SBP_0$`6177-0.0`) &
          is.na(ukb6221_SBP_0$`6177-0.1`) &
          is.na(ukb6221_SBP_0$`6177-0.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-0.0` == 2 |
        ukb6221_SBP_0$`6153-0.1` == 2 |
        ukb6221_SBP_0$`6153-0.2` == 2 |
        ukb6221_SBP_0$`6153-0.3` == 2 |
        ukb6221_SBP_0$`6177-0.0` == 2 |
        ukb6221_SBP_0$`6177-0.1` == 2 |
        ukb6221_SBP_0$`6177-0.2` == 2),
      ukb6221_SBP_0$`4080-0` + 15, ukb6221_SBP_0$`4080-0`
      )
      )
  )

  ##########################
  ### Summeray SBP_at 1.0 #
  ##########################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("4080-1" = ifelse
  (is.na(ukb6221_SBP$`4080-1.0`) & is.na(ukb6221_SBP$`4080-1.1`), NA,
    ifelse(is.na(ukb6221_SBP$`4080-1.1`), ukb6221_SBP$"4080-1.0",
      ifelse(is.na(ukb6221_SBP$`4080-1.0`), ukb6221_SBP$"4080-1.1",
        rowMeans(ukb6221_SBP[, c("4080-1.0", "4080-1.1")])
      )
    )
  ))
  ######################
  ### Outlier SBP_at 1.0 ##
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("4080-1" = ifelse(
    !is.na(ukb6221_SBP_0$`4080-1.0`) & !is.na(ukb6221_SBP_0$`4080-1.1`) &
      abs(ukb6221_SBP_0$`4080-1.0` - ukb6221_SBP_0$`4080-1.1`) > 4.56 * sd(ukb6221_SBP_0$`4080-1`, na.rm = T),
    NA, ukb6221_SBP_0$`4080-1`
  ))


  ######################
  ### Medication 1.0 ###
  ######################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "4080-1" =
      ifelse(is.na(ukb6221_SBP_0$`4080-1`) |
        (is.na(ukb6221_SBP_0$`6153-1.0`) &
          is.na(ukb6221_SBP_0$`6153-1.1`) &
          is.na(ukb6221_SBP_0$`6153-1.2`) &
          is.na(ukb6221_SBP_0$`6153-1.3`) &
          is.na(ukb6221_SBP_0$`6177-1.0`) &
          is.na(ukb6221_SBP_0$`6177-1.1`) &
          is.na(ukb6221_SBP_0$`6177-1.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-1.0` == 2 |
        ukb6221_SBP_0$`6153-1.1` == 2 |
        ukb6221_SBP_0$`6153-1.2` == 2 |
        ukb6221_SBP_0$`6153-1.3` == 2 |
        ukb6221_SBP_0$`6177-1.0` == 2 |
        ukb6221_SBP_0$`6177-1.1` == 2 |
        ukb6221_SBP_0$`6177-1.2` == 2),
      ukb6221_SBP_0$`4080-1` + 15, ukb6221_SBP_0$`4080-1`
      )
      )
  )

  ######################
  ### Summeray SBP_at 2.0 #
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("4080-2" = ifelse
  (is.na(ukb6221_SBP$`4080-2.0`) & is.na(ukb6221_SBP$`4080-2.1`), NA,
    ifelse(is.na(ukb6221_SBP$`4080-2.1`), ukb6221_SBP$"4080-2.0",
      ifelse(is.na(ukb6221_SBP$`4080-2.0`), ukb6221_SBP$"4080-2.1",
        rowMeans(ukb6221_SBP[, c("4080-2.0", "4080-2.1")])
      )
    )
  ))
  ######################
  ### Outlier SBP_at 2.0 ##
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("4080-2" = ifelse(
    !is.na(ukb6221_SBP_0$`4080-2.0`) & !is.na(ukb6221_SBP_0$`4080-2.1`) &
      abs(ukb6221_SBP_0$`4080-2.0` - ukb6221_SBP_0$`4080-2.1`) > 4.56 * sd(ukb6221_SBP_0$`4080-2`, na.rm = T),
    NA, ukb6221_SBP_0$`4080-2`
  ))


  ######################
  ### Medication 2.0 ###
  ######################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "4080-2" =
      ifelse(is.na(ukb6221_SBP_0$`4080-2`) |
        (is.na(ukb6221_SBP_0$`6153-2.0`) &
          is.na(ukb6221_SBP_0$`6153-2.1`) &
          is.na(ukb6221_SBP_0$`6153-2.2`) &
          is.na(ukb6221_SBP_0$`6153-2.3`) &
          is.na(ukb6221_SBP_0$`6177-2.0`) &
          is.na(ukb6221_SBP_0$`6177-2.1`) &
          is.na(ukb6221_SBP_0$`6177-2.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-2.0` == 2 |
        ukb6221_SBP_0$`6153-2.1` == 2 |
        ukb6221_SBP_0$`6153-2.2` == 2 |
        ukb6221_SBP_0$`6153-2.3` == 2 |
        ukb6221_SBP_0$`6177-2.0` == 2 |
        ukb6221_SBP_0$`6177-2.1` == 2 |
        ukb6221_SBP_0$`6177-2.2` == 2),
      ukb6221_SBP_0$`4080-2` + 15, ukb6221_SBP_0$`4080-2`
      )
      )
  )

  ##########################
  ### Summeray SBP_man 0.0 #
  ##########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-0" = ifelse
  (is.na(ukb6221_SBP$`93-0.0`) & is.na(ukb6221_SBP$`93-0.1`), NA,
    ifelse(is.na(ukb6221_SBP$`93-0.1`), ukb6221_SBP$"93-0.0",
      ifelse(is.na(ukb6221_SBP$`93-0.0`), ukb6221_SBP$"93-0.1",
        rowMeans(ukb6221_SBP[, c("93-0.0", "93-0.1")])
      )
    )
  ))
  ##########################
  ### Outlier SBP_man 0.0 ##
  ##########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-0" = ifelse(
    !is.na(ukb6221_SBP_0$`93-0.0`) & !is.na(ukb6221_SBP_0$`93-0.1`) &
      abs(ukb6221_SBP_0$`93-0.0` - ukb6221_SBP_0$`93-0.1`) > 4.56 * sd(ukb6221_SBP_0$`93-0`, na.rm = T),
    NA, ukb6221_SBP_0$`93-0`
  ))


  ######################
  ### Medication 0.0 ###
  ######################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "93-0" =
      ifelse(is.na(ukb6221_SBP_0$`93-0`) |
        (is.na(ukb6221_SBP_0$`6153-0.0`) &
          is.na(ukb6221_SBP_0$`6153-0.1`) &
          is.na(ukb6221_SBP_0$`6153-0.2`) &
          is.na(ukb6221_SBP_0$`6153-0.3`) &
          is.na(ukb6221_SBP_0$`6177-0.0`) &
          is.na(ukb6221_SBP_0$`6177-0.1`) &
          is.na(ukb6221_SBP_0$`6177-0.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-0.0` == 2 |
        ukb6221_SBP_0$`6153-0.1` == 2 |
        ukb6221_SBP_0$`6153-0.2` == 2 |
        ukb6221_SBP_0$`6153-0.3` == 2 |
        ukb6221_SBP_0$`6177-0.0` == 2 |
        ukb6221_SBP_0$`6177-0.1` == 2 |
        ukb6221_SBP_0$`6177-0.2` == 2),
      ukb6221_SBP_0$`93-0` + 15, ukb6221_SBP_0$`93-0`
      )
      )
  )
  #############################
  ### Summeray SBP_man 1.0 #
  #############################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-1" = ifelse
  (is.na(ukb6221_SBP$`93-1.0`) & is.na(ukb6221_SBP$`93-1.1`), NA,
    ifelse(is.na(ukb6221_SBP$`93-1.1`), ukb6221_SBP$"93-1.0",
      ifelse(is.na(ukb6221_SBP$`93-1.0`), ukb6221_SBP$"93-1.1",
        rowMeans(ukb6221_SBP[, c("93-1.0", "93-1.1")])
      )
    )
  ))
  #########################
  ### Outlier SBP_man 1.0 ##
  #########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-1" = ifelse(
    !is.na(ukb6221_SBP_0$`93-1.0`) & !is.na(ukb6221_SBP_0$`93-1.1`) &
      abs(ukb6221_SBP_0$`93-1.0` - ukb6221_SBP_0$`93-1.1`) > 4.56 * sd(ukb6221_SBP_0$`93-1`, na.rm = T),
    NA, ukb6221_SBP_0$`93-1`
  ))

  #########################
  ### Medication 1.0 ###
  #########################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "93-1" =
      ifelse(is.na(ukb6221_SBP_0$`93-1`) |
        (is.na(ukb6221_SBP_0$`6153-1.0`) &
          is.na(ukb6221_SBP_0$`6153-1.1`) &
          is.na(ukb6221_SBP_0$`6153-1.2`) &
          is.na(ukb6221_SBP_0$`6153-1.3`) &
          is.na(ukb6221_SBP_0$`6177-1.0`) &
          is.na(ukb6221_SBP_0$`6177-1.1`) &
          is.na(ukb6221_SBP_0$`6177-1.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-1.0` == 2 |
        ukb6221_SBP_0$`6153-1.1` == 2 |
        ukb6221_SBP_0$`6153-1.2` == 2 |
        ukb6221_SBP_0$`6153-1.3` == 2 |
        ukb6221_SBP_0$`6177-1.0` == 2 |
        ukb6221_SBP_0$`6177-1.1` == 2 |
        ukb6221_SBP_0$`6177-1.2` == 2),
      ukb6221_SBP_0$`93-1` + 15, ukb6221_SBP_0$`93-1`
      )
      )
  )

  #########################
  ### Summeray SBP_man 2.0 #
  #########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-2" = ifelse
  (is.na(ukb6221_SBP$`93-2.0`) & is.na(ukb6221_SBP$`93-2.1`), NA,
    ifelse(is.na(ukb6221_SBP$`93-2.1`), ukb6221_SBP$"93-2.0",
      ifelse(is.na(ukb6221_SBP$`93-2.0`), ukb6221_SBP$"93-2.1",
        rowMeans(ukb6221_SBP[, c("93-2.0", "93-2.1")])
      )
    )
  ))
  #########################
  ### Outlier SBP_man 2.0 ##
  #########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("93-2" = ifelse(
    !is.na(ukb6221_SBP_0$`93-2.0`) & !is.na(ukb6221_SBP_0$`93-2.1`) &
      abs(ukb6221_SBP_0$`93-2.0` - ukb6221_SBP_0$`93-2.1`) > 4.56 * sd(ukb6221_SBP_0$`93-2`, na.rm = T),
    NA, ukb6221_SBP_0$`93-2`
  ))

  #########################
  ### Medication 2.0 ###
  #########################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "93-2" =
      ifelse(is.na(ukb6221_SBP_0$`93-2`) |
        (is.na(ukb6221_SBP_0$`6153-2.0`) &
          is.na(ukb6221_SBP_0$`6153-2.1`) &
          is.na(ukb6221_SBP_0$`6153-2.2`) &
          is.na(ukb6221_SBP_0$`6153-2.3`) &
          is.na(ukb6221_SBP_0$`6177-2.0`) &
          is.na(ukb6221_SBP_0$`6177-2.1`) &
          is.na(ukb6221_SBP_0$`6177-2.2`)), NA,
      ifelse(!is.na(ukb6221_SBP_0$`6153-2.0` == 2 |
        ukb6221_SBP_0$`6153-2.1` == 2 |
        ukb6221_SBP_0$`6153-2.2` == 2 |
        ukb6221_SBP_0$`6153-2.3` == 2 |
        ukb6221_SBP_0$`6177-2.0` == 2 |
        ukb6221_SBP_0$`6177-2.1` == 2 |
        ukb6221_SBP_0$`6177-2.2` == 2),
      ukb6221_SBP_0$`93-2` + 15, ukb6221_SBP_0$`93-2`
      )
      )
  )

  ######################
  ### Select SBP read###
  ######################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "SBP" =
      ifelse(!is.na(ukb6221_SBP_0$`4080-0`), ukb6221_SBP_0$`4080-0`,
        ifelse(!is.na(ukb6221_SBP_0$`4080-1`), ukb6221_SBP_0$`4080-1`,
          ifelse(!is.na(ukb6221_SBP_0$`4080-2`), ukb6221_SBP_0$`4080-2`,
            ifelse(!is.na(ukb6221_SBP_0$`93-0`), ukb6221_SBP_0$`93-0`,
              ifelse(!is.na(ukb6221_SBP_0$`93-1`), ukb6221_SBP_0$`93-1`, ukb6221_SBP_0$`93-2`)
            )
          )
        )
      )
  )
  #########################
  ### Outlier SBP read   ##
  #########################
  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate("SBP" = ifelse(
    abs(mean(ukb6221_SBP_0$`SBP`, na.rm = T) - ukb6221_SBP_0$`SBP`) > 4.56 * sd(ukb6221_SBP_0$`SBP`, na.rm = T), NA,
    ukb6221_SBP_0$`SBP`
  ))

  #########################
  ### Age SBP read   ##
  #########################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "21003-2.0" =
      ifelse(!is.na(ukb6221_SBP_0$`53-2.0`),
        as.numeric(substr(ukb6221_SBP_0$`53-2.0`, 1, 4)) - ukb6221_SBP_0$`34-0.0`,
        NA
      )
  )

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "Age" =
      ifelse(!is.na(ukb6221_SBP_0$`4080-0`), ukb6221_SBP_0$`21003-0.0`,
        ifelse(!is.na(ukb6221_SBP_0$`4080-1`), ukb6221_SBP_0$`21003-1.0`,
          ifelse(!is.na(ukb6221_SBP_0$`4080-2`), ukb6221_SBP_0$`21003-2.0`,
            ifelse(!is.na(ukb6221_SBP_0$`93-0`), ukb6221_SBP_0$`21003-0.0`,
              ifelse(!is.na(ukb6221_SBP_0$`93-1`), ukb6221_SBP_0$`21003-1.0`,
                ifelse(!is.na(ukb6221_SBP_0$`93-2`), ukb6221_SBP_0$`21003-2.0`, NA)
              )
            )
          )
        )
      )
  )

  #########################
  ### Centre SBP read   ##
  #########################

  ukb6221_SBP_0 <- ukb6221_SBP_0 %>% mutate(
    "Centre" =
      ifelse(!is.na(ukb6221_SBP_0$`4080-0`), ukb6221_SBP_0$`54-0.0`,
        ifelse(!is.na(ukb6221_SBP_0$`4080-1`), ukb6221_SBP_0$`54-1.0`,
          ifelse(!is.na(ukb6221_SBP_0$`4080-2`), ukb6221_SBP_0$`54-2.0`,
            ifelse(!is.na(ukb6221_SBP_0$`93-0`), ukb6221_SBP_0$`54-0.0`,
              ifelse(!is.na(ukb6221_SBP_0$`93-1`), ukb6221_SBP_0$`54-1.0`,
                ifelse(!is.na(ukb6221_SBP_0$`93-2`), ukb6221_SBP_0$`54-2.0`, NA)
              )
            )
          )
        )
      )
  )

  ###################
  ### Write files ###
  ###################
  write.table(ukb6221_SBP_0, output_path, row.names = F, sep = "\t", quote = F, na = "")
}

extract_SBP(
  raw_data_path = snakemake@input,
  output_path = snakemake@output
)
