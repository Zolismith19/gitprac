

library(tidyverse)
library(scales)

data <- read_csv("/Users/zolismith/Desktop/COVID19_Vaccine_Distribution_Analysis/data/DATASET.csv")

data_cleaned <- data %>%
  filter(!is.na(`1st Dose Allocations`), !is.na(`2nd Dose Allocations`))

summary_data <- data_cleaned %>%
  group_by(Jurisdiction) %>%
  summarise(
    First_Dose = sum(`1st Dose Allocations`, na.rm = TRUE),
    Second_Dose = sum(`2nd Dose Allocations`, na.rm = TRUE)
  ) %>%
  arrange(desc(First_Dose + Second_Dose)) %>%
  head(10)

summary_data_long <- summary_data %>%
  pivot_longer(cols = c(First_Dose, Second_Dose), names_to = "Dose Type", values_to = "Allocations")

fig <- ggplot(summary_data_long, aes(x = reorder(Jurisdiction, Allocations), y = Allocations, fill = `Dose Type`)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  scale_fill_manual(values = c("First_Dose" = "#1f77b4", "Second_Dose" = "#ff7f0e"),
                    labels = c("First Dose", "Second Dose")) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Jurisdictions by Total Vaccine Allocations",
       x = "Jurisdiction",
       y = "Total Allocations",
       fill = "Dose Type") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

dir.create("figures", showWarnings = FALSE)
ggsave(filename = "figures/required_figure.png", plot = fig, width = 10, height = 6)
