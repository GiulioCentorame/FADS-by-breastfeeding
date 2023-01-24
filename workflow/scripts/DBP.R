##################################################################################################################################
######
###### Program:             PhenotypePreparationDBP.R
###### Written by:		      Geng Wang
###### R packages loaded:	  dyplr
###### Datafiles :		      ukb6221.csv
###### Objective :		      select field and clean files
######
##################################################################################################################################

library(dplyr)

# Save image for debugging
save.image("debug.RData")

extract_DBP <- function(raw_data_path,
                        output_path) {
  ######################
  ### Open datafiles ###
  ######################
  ukb6221 <- read.csv(raw_data_path, header = T, sep = ",", check.names = FALSE)
  dim(ukb6221)

  ######################
  ### Select fields  ###
  ######################
  ukb6221_DBP <- select(ukb6221, c(eid, "31-0.0", "34-0.0", "54-0.0":"54-2.0", "94-0.0":"94-2.1", "4079-0.0":"4079-2.1", "6153-0.0":"6153-2.3", "6177-0.0":"6177-2.2", "21003-0.0", "21003-1.0", "53-0.0":"53-2.0"))

  ## summary(!is.na(ukb6221_DBP$`4079-0.0`)&is.na(ukb6221_DBP$`4079-0.1`))
  ## summary(!is.na(ukb6221_DBP$`4079-1.0`)&is.na(ukb6221_DBP$`4079-1.1`))
  ## summary(!is.na(ukb6221_DBP$`4079-2.0`)&is.na(ukb6221_DBP$`4079-2.1`))

  ######################
  ### Summeray DBP_at 0.0 #
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP %>% mutate("4079-0" = ifelse
  (is.na(ukb6221_DBP$`4079-0.0`) & is.na(ukb6221_DBP$`4079-0.1`), NA,
    ifelse(is.na(ukb6221_DBP$`4079-0.1`), ukb6221_DBP$"4079-0.0",
      ifelse(is.na(ukb6221_DBP$`4079-0.0`), ukb6221_DBP$"4079-0.1",
        rowMeans(ukb6221_DBP[, c("4079-0.0", "4079-0.1")])
      )
    )
  ))
  ######################
  ### Outlier DBP_at 0.0 ##
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("4079-0" = ifelse(
    !is.na(ukb6221_DBP_0$`4079-0.0`) & !is.na(ukb6221_DBP_0$`4079-0.1`) &
      abs(ukb6221_DBP_0$`4079-0.0` - ukb6221_DBP_0$`4079-0.1`) > 4.56 * sd(ukb6221_DBP_0$`4079-0`, na.rm = T),
    NA, ukb6221_DBP_0$`4079-0`
  ))


  ######################
  ### Medication 0.0 ###
  ######################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "4079-0" =
      ifelse(is.na(ukb6221_DBP_0$`4079-0`) |
        (is.na(ukb6221_DBP_0$`6153-0.0`) &
          is.na(ukb6221_DBP_0$`6153-0.1`) &
          is.na(ukb6221_DBP_0$`6153-0.2`) &
          is.na(ukb6221_DBP_0$`6153-0.3`) &
          is.na(ukb6221_DBP_0$`6177-0.0`) &
          is.na(ukb6221_DBP_0$`6177-0.1`) &
          is.na(ukb6221_DBP_0$`6177-0.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-0.0` == 2 |
        ukb6221_DBP_0$`6153-0.1` == 2 |
        ukb6221_DBP_0$`6153-0.2` == 2 |
        ukb6221_DBP_0$`6153-0.3` == 2 |
        ukb6221_DBP_0$`6177-0.0` == 2 |
        ukb6221_DBP_0$`6177-0.1` == 2 |
        ukb6221_DBP_0$`6177-0.2` == 2),
      ukb6221_DBP_0$`4079-0` + 10, ukb6221_DBP_0$`4079-0`
      )
      )
  )

  ##########################
  ### Summeray DBP_at 1.0 #
  ##########################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("4079-1" = ifelse
  (is.na(ukb6221_DBP$`4079-1.0`) & is.na(ukb6221_DBP$`4079-1.1`), NA,
    ifelse(is.na(ukb6221_DBP$`4079-1.1`), ukb6221_DBP$"4079-1.0",
      ifelse(is.na(ukb6221_DBP$`4079-1.0`), ukb6221_DBP$"4079-1.1",
        rowMeans(ukb6221_DBP[, c("4079-1.0", "4079-1.1")])
      )
    )
  ))
  ######################
  ### Outlier DBP_at 1.0 ##
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("4079-1" = ifelse(
    !is.na(ukb6221_DBP_0$`4079-1.0`) & !is.na(ukb6221_DBP_0$`4079-1.1`) &
      abs(ukb6221_DBP_0$`4079-1.0` - ukb6221_DBP_0$`4079-1.1`) > 4.56 * sd(ukb6221_DBP_0$`4079-1`, na.rm = T),
    NA, ukb6221_DBP_0$`4079-1`
  ))


  ######################
  ### Medication 1.0 ###
  ######################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "4079-1" =
      ifelse(is.na(ukb6221_DBP_0$`4079-1`) |
        (is.na(ukb6221_DBP_0$`6153-1.0`) &
          is.na(ukb6221_DBP_0$`6153-1.1`) &
          is.na(ukb6221_DBP_0$`6153-1.2`) &
          is.na(ukb6221_DBP_0$`6153-1.3`) &
          is.na(ukb6221_DBP_0$`6177-1.0`) &
          is.na(ukb6221_DBP_0$`6177-1.1`) &
          is.na(ukb6221_DBP_0$`6177-1.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-1.0` == 2 |
        ukb6221_DBP_0$`6153-1.1` == 2 |
        ukb6221_DBP_0$`6153-1.2` == 2 |
        ukb6221_DBP_0$`6153-1.3` == 2 |
        ukb6221_DBP_0$`6177-1.0` == 2 |
        ukb6221_DBP_0$`6177-1.1` == 2 |
        ukb6221_DBP_0$`6177-1.2` == 2),
      ukb6221_DBP_0$`4079-1` + 10, ukb6221_DBP_0$`4079-1`
      )
      )
  )

  ######################
  ### Summeray DBP_at 2.0 #
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("4079-2" = ifelse
  (is.na(ukb6221_DBP$`4079-2.0`) & is.na(ukb6221_DBP$`4079-2.1`), NA,
    ifelse(is.na(ukb6221_DBP$`4079-2.1`), ukb6221_DBP$"4079-2.0",
      ifelse(is.na(ukb6221_DBP$`4079-2.0`), ukb6221_DBP$"4079-2.1",
        rowMeans(ukb6221_DBP[, c("4079-2.0", "4079-2.1")])
      )
    )
  ))
  ######################
  ### Outlier DBP_at 2.0 ##
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("4079-2" = ifelse(
    !is.na(ukb6221_DBP_0$`4079-2.0`) & !is.na(ukb6221_DBP_0$`4079-2.1`) &
      abs(ukb6221_DBP_0$`4079-2.0` - ukb6221_DBP_0$`4079-2.1`) > 4.56 * sd(ukb6221_DBP_0$`4079-2`, na.rm = T),
    NA, ukb6221_DBP_0$`4079-2`
  ))


  ######################
  ### Medication 2.0 ###
  ######################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "4079-2" =
      ifelse(is.na(ukb6221_DBP_0$`4079-2`) |
        (is.na(ukb6221_DBP_0$`6153-2.0`) &
          is.na(ukb6221_DBP_0$`6153-2.1`) &
          is.na(ukb6221_DBP_0$`6153-2.2`) &
          is.na(ukb6221_DBP_0$`6153-2.3`) &
          is.na(ukb6221_DBP_0$`6177-2.0`) &
          is.na(ukb6221_DBP_0$`6177-2.1`) &
          is.na(ukb6221_DBP_0$`6177-2.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-2.0` == 2 |
        ukb6221_DBP_0$`6153-2.1` == 2 |
        ukb6221_DBP_0$`6153-2.2` == 2 |
        ukb6221_DBP_0$`6153-2.3` == 2 |
        ukb6221_DBP_0$`6177-2.0` == 2 |
        ukb6221_DBP_0$`6177-2.1` == 2 |
        ukb6221_DBP_0$`6177-2.2` == 2),
      ukb6221_DBP_0$`4079-2` + 10, ukb6221_DBP_0$`4079-2`
      )
      )
  )

  ##########################
  ### Summeray DBP_man 0.0 #
  ##########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-0" = ifelse
  (is.na(ukb6221_DBP$`94-0.0`) & is.na(ukb6221_DBP$`94-0.1`), NA,
    ifelse(is.na(ukb6221_DBP$`94-0.1`), ukb6221_DBP$"94-0.0",
      ifelse(is.na(ukb6221_DBP$`94-0.0`), ukb6221_DBP$"94-0.1",
        rowMeans(ukb6221_DBP[, c("94-0.0", "94-0.1")])
      )
    )
  ))
  ##########################
  ### Outlier DBP_man 0.0 ##
  ##########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-0" = ifelse(
    !is.na(ukb6221_DBP_0$`94-0.0`) & !is.na(ukb6221_DBP_0$`94-0.1`) &
      abs(ukb6221_DBP_0$`94-0.0` - ukb6221_DBP_0$`94-0.1`) > 4.56 * sd(ukb6221_DBP_0$`94-0`, na.rm = T),
    NA, ukb6221_DBP_0$`94-0`
  ))


  ######################
  ### Medication 0.0 ###
  ######################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "94-0" =
      ifelse(is.na(ukb6221_DBP_0$`94-0`) |
        (is.na(ukb6221_DBP_0$`6153-0.0`) &
          is.na(ukb6221_DBP_0$`6153-0.1`) &
          is.na(ukb6221_DBP_0$`6153-0.2`) &
          is.na(ukb6221_DBP_0$`6153-0.3`) &
          is.na(ukb6221_DBP_0$`6177-0.0`) &
          is.na(ukb6221_DBP_0$`6177-0.1`) &
          is.na(ukb6221_DBP_0$`6177-0.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-0.0` == 2 |
        ukb6221_DBP_0$`6153-0.1` == 2 |
        ukb6221_DBP_0$`6153-0.2` == 2 |
        ukb6221_DBP_0$`6153-0.3` == 2 |
        ukb6221_DBP_0$`6177-0.0` == 2 |
        ukb6221_DBP_0$`6177-0.1` == 2 |
        ukb6221_DBP_0$`6177-0.2` == 2),
      ukb6221_DBP_0$`94-0` + 10, ukb6221_DBP_0$`94-0`
      )
      )
  )
  #############################
  ### Summeray DBP_man 1.0 #
  #############################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-1" = ifelse
  (is.na(ukb6221_DBP$`94-1.0`) & is.na(ukb6221_DBP$`94-1.1`), NA,
    ifelse(is.na(ukb6221_DBP$`94-1.1`), ukb6221_DBP$"94-1.0",
      ifelse(is.na(ukb6221_DBP$`94-1.0`), ukb6221_DBP$"94-1.1",
        rowMeans(ukb6221_DBP[, c("94-1.0", "94-1.1")])
      )
    )
  ))
  #########################
  ### Outlier DBP_man 1.0 ##
  #########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-1" = ifelse(
    !is.na(ukb6221_DBP_0$`94-1.0`) & !is.na(ukb6221_DBP_0$`94-1.1`) &
      abs(ukb6221_DBP_0$`94-1.0` - ukb6221_DBP_0$`94-1.1`) > 4.56 * sd(ukb6221_DBP_0$`94-1`, na.rm = T),
    NA, ukb6221_DBP_0$`94-1`
  ))

  #########################
  ### Medication 1.0 ###
  #########################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "94-1" =
      ifelse(is.na(ukb6221_DBP_0$`94-1`) |
        (is.na(ukb6221_DBP_0$`6153-1.0`) &
          is.na(ukb6221_DBP_0$`6153-1.1`) &
          is.na(ukb6221_DBP_0$`6153-1.2`) &
          is.na(ukb6221_DBP_0$`6153-1.3`) &
          is.na(ukb6221_DBP_0$`6177-1.0`) &
          is.na(ukb6221_DBP_0$`6177-1.1`) &
          is.na(ukb6221_DBP_0$`6177-1.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-1.0` == 2 |
        ukb6221_DBP_0$`6153-1.1` == 2 |
        ukb6221_DBP_0$`6153-1.2` == 2 |
        ukb6221_DBP_0$`6153-1.3` == 2 |
        ukb6221_DBP_0$`6177-1.0` == 2 |
        ukb6221_DBP_0$`6177-1.1` == 2 |
        ukb6221_DBP_0$`6177-1.2` == 2),
      ukb6221_DBP_0$`94-1` + 10, ukb6221_DBP_0$`94-1`
      )
      )
  )

  #########################
  ### Summeray DBP_man 2.0 #
  #########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-2" = ifelse
  (is.na(ukb6221_DBP$`94-2.0`) & is.na(ukb6221_DBP$`94-2.1`), NA,
    ifelse(is.na(ukb6221_DBP$`94-2.1`), ukb6221_DBP$"94-2.0",
      ifelse(is.na(ukb6221_DBP$`94-2.0`), ukb6221_DBP$"94-2.1",
        rowMeans(ukb6221_DBP[, c("94-2.0", "94-2.1")])
      )
    )
  ))
  #########################
  ### Outlier DBP_man 2.0 ##
  #########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("94-2" = ifelse(
    !is.na(ukb6221_DBP_0$`94-2.0`) & !is.na(ukb6221_DBP_0$`94-2.1`) &
      abs(ukb6221_DBP_0$`94-2.0` - ukb6221_DBP_0$`94-2.1`) > 4.56 * sd(ukb6221_DBP_0$`94-2`, na.rm = T),
    NA, ukb6221_DBP_0$`94-2`
  ))

  #########################
  ### Medication 2.0 ###
  #########################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "94-2" =
      ifelse(is.na(ukb6221_DBP_0$`94-2`) |
        (is.na(ukb6221_DBP_0$`6153-2.0`) &
          is.na(ukb6221_DBP_0$`6153-2.1`) &
          is.na(ukb6221_DBP_0$`6153-2.2`) &
          is.na(ukb6221_DBP_0$`6153-2.3`) &
          is.na(ukb6221_DBP_0$`6177-2.0`) &
          is.na(ukb6221_DBP_0$`6177-2.1`) &
          is.na(ukb6221_DBP_0$`6177-2.2`)), NA,
      ifelse(!is.na(ukb6221_DBP_0$`6153-2.0` == 2 |
        ukb6221_DBP_0$`6153-2.1` == 2 |
        ukb6221_DBP_0$`6153-2.2` == 2 |
        ukb6221_DBP_0$`6153-2.3` == 2 |
        ukb6221_DBP_0$`6177-2.0` == 2 |
        ukb6221_DBP_0$`6177-2.1` == 2 |
        ukb6221_DBP_0$`6177-2.2` == 2),
      ukb6221_DBP_0$`94-2` + 10, ukb6221_DBP_0$`94-2`
      )
      )
  )

  ######################
  ### Select DBP read###
  ######################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "DBP" =
      ifelse(!is.na(ukb6221_DBP_0$`4079-0`), ukb6221_DBP_0$`4079-0`,
        ifelse(!is.na(ukb6221_DBP_0$`4079-1`), ukb6221_DBP_0$`4079-1`,
          ifelse(!is.na(ukb6221_DBP_0$`4079-2`), ukb6221_DBP_0$`4079-2`,
            ifelse(!is.na(ukb6221_DBP_0$`94-0`), ukb6221_DBP_0$`94-0`,
              ifelse(!is.na(ukb6221_DBP_0$`94-1`), ukb6221_DBP_0$`94-1`, ukb6221_DBP_0$`94-2`)
            )
          )
        )
      )
  )
  #########################
  ### Outlier DBP read   ##
  #########################
  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate("DBP" = ifelse(
    abs(mean(ukb6221_DBP_0$`DBP`, na.rm = T) - ukb6221_DBP_0$`DBP`) > 4.56 * sd(ukb6221_DBP_0$`DBP`, na.rm = T), NA,
    ukb6221_DBP_0$`DBP`
  ))

  #########################
  ### Age DBP read   ##
  #########################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "21003-2.0" =
      ifelse(!is.na(ukb6221_DBP_0$`53-2.0`),
        as.numeric(substr(ukb6221_DBP_0$`53-2.0`, 1, 4)) - ukb6221_DBP_0$`34-0.0`,
        NA
      )
  )

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "Age" =
      ifelse(!is.na(ukb6221_DBP_0$`4079-0`), ukb6221_DBP_0$`21003-0.0`,
        ifelse(!is.na(ukb6221_DBP_0$`4079-1`), ukb6221_DBP_0$`21003-1.0`,
          ifelse(!is.na(ukb6221_DBP_0$`4079-2`), ukb6221_DBP_0$`21003-2.0`,
            ifelse(!is.na(ukb6221_DBP_0$`94-0`), ukb6221_DBP_0$`21003-0.0`,
              ifelse(!is.na(ukb6221_DBP_0$`94-1`), ukb6221_DBP_0$`21003-1.0`,
                ifelse(!is.na(ukb6221_DBP_0$`94-2`), ukb6221_DBP_0$`21003-2.0`, NA)
              )
            )
          )
        )
      )
  )

  #########################
  ### Centre DBP read   ##
  #########################

  ukb6221_DBP_0 <- ukb6221_DBP_0 %>% mutate(
    "Centre" =
      ifelse(!is.na(ukb6221_DBP_0$`4079-0`), ukb6221_DBP_0$`54-0.0`,
        ifelse(!is.na(ukb6221_DBP_0$`4079-1`), ukb6221_DBP_0$`54-1.0`,
          ifelse(!is.na(ukb6221_DBP_0$`4079-2`), ukb6221_DBP_0$`54-2.0`,
            ifelse(!is.na(ukb6221_DBP_0$`94-0`), ukb6221_DBP_0$`54-0.0`,
              ifelse(!is.na(ukb6221_DBP_0$`94-1`), ukb6221_DBP_0$`54-1.0`,
                ifelse(!is.na(ukb6221_DBP_0$`94-2`), ukb6221_DBP_0$`54-2.0`, NA)
              )
            )
          )
        )
      )
  )



  ###################
  ### Write files ###
  ###################

  ukb6221_DBP_0 %>%
    select(eid, DBP, Age, Centre) %>%
    write.table(., output_path, row.names = F, sep = "\t", quote = F, na = "")
}

extract_DBP(
  raw_data_path = snakemake@input[[1]],
  output_path = snakemake@output[[1]]
)
