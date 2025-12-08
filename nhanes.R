install.packages("tidyverse")
library(tidyverse)
library(dplyr)
load("~/Desktop/datasci306_final/nhanes.RData")

dep2score <- function(x) {
  dplyr::case_match(as.integer(x),
                    1L ~ 0L,
                    2L ~ 1L,
                    3L ~ 2L,
                    4L ~ 3L,
                    .default = NA_integer_
  )
}

phq_items <- paste0("DPQ0", seq(10, 90, 10))

nhanes_clean <- nhanes_dpq |>
  filter(RIDAGEYR >= 18) |>
  # convert each DPQ item to 0–3 score
  mutate(across(all_of(phq_items), dep2score))|>
  rowwise() |>
  mutate(
    DepScore  = sum(c_across(all_of(phq_items)), na.rm = TRUE),
    Depressed = DepScore >= 10
  ) |>
  ungroup()|>
  mutate(
    AgeGroup = case_when(
      RIDAGEYR < 40 ~ "18–39",
      RIDAGEYR < 60 ~ "40–59",
      TRUE          ~ "60+"
    ),
    Sex = RIAGENDR  
  )
saveRDS(nhanes_clean, "nhanes_clean.rds")
cat("Saved cleaned NHANES data to nhanes_clean.rds\n")

