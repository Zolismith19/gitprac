

library(tidyverse)
library(gt)

data <- read_csv("/Users/zolismith/Desktop/COVID19_Vaccine_Distribution_Analysis/data/DATASET.csv")

table_data <- data %>%
  filter(!is.na(`1st Dose Allocations`), !is.na(`2nd Dose Allocations`)) %>%
  group_by(Jurisdiction) %>%
  summarise(
    First_Dose = sum(`1st Dose Allocations`),
    Second_Dose = sum(`2nd Dose Allocations`)
  ) %>%
  arrange(desc(First_Dose + Second_Dose)) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(
    title = "Top 10 Jurisdictions by Vaccine Allocations",
    subtitle = "Summed allocations of first and second doses"
  )

dir.create("tables", showWarnings = FALSE)
gtsave(table_data, "tables/required_table.html")
