# ----------------------------------------------------------------------------
# R SCRIPT FOR CITY GOVERNMENT THEFT REDUCTION REPORT
# ----------------------------------------------------------------------------
# Goal: Load, clean, and visualize car theft data for city decision-makers.
# Focus: Kia/Hyundai theft patterns and geographic hotspots.
#
# Packages needed:
#   - tidyverse (ggplot2, dplyr, readr, etc.) for cleaning and plotting
#   - treemapify (for Treemap visualization)
#   - lubridate (for date manipulation)
#   - stringr (for text cleaning)
#   - readxl (for loading .xlsx files)
# ----------------------------------------------------------------------------

# -----------------------------------------------
# 1. INSTALL AND LOAD PACKAGES
# -----------------------------------------------
# Run this block once to ensure packages are installed
# install.packages(c("tidyverse", "treemapify", "lubridate", "stringr", "scales", "readxl"))

library(tidyverse)
library(treemapify)
library(lubridate)
library(stringr)
library(scales) # For better formatting in charts
library(readxl) # Added for Excel file support

# Set a consistent, accessible theme
theme_set(theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
              axis.title = element_text(size = 12),
              legend.position = "bottom"
            ))

# -----------------------------------------------
# 2. LOAD AND CLEAN DATASETS
# -----------------------------------------------

# --- Data 1: carTheftsMap.csv (Geographic Data) ---
# Used for Treemap (Top Agencies) and Stacked Bar (Change over time)
df_map <- read_csv("carTheftsMap.csv") %>%
  # Coerce yearly columns to numeric, removing any non-numeric characters
  mutate(across(starts_with("countCarThefts"), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  # Fill missing agency_ori (a technical ID) with a placeholder for filtering
  mutate(agency_ori = replace_na(agency_ori, "NO_ID")) %>%
  # Calculate total thefts across the four years for ranking
  rowwise() %>%
  mutate(TotalThefts = sum(c_across(starts_with("countCarThefts")), na.rm = TRUE)) %>%
  ungroup() %>%
  # Select and rename columns
  select(Agency = geo_name, `2019` = countCarThefts2019, `2020` = countCarThefts2020, `2021` = countCarThefts2021, `2022` = countCarThefts2022, TotalThefts)

# --- Data 2: kiaHyundaiThefts.csv (Multi-City Monthly Trends) ---
# Used for Stacked Area Chart (Kia/Hyundai vs. Others over time)
df_multi <- read_csv("kiaHyundaiThefts.csv") %>%
  # Create a proper Date column
  mutate(Date = myd(paste(month, year, "01"))) %>%
  # Combine theft counts into a total column for easier pivoting
  mutate(TotalThefts = countKiaHyundaiThefts + countOtherThefts) %>%
  # Convert counts to numeric
  mutate(across(starts_with("count"), as.numeric)) %>%
  # Select and rename columns
  select(Date, City = city, State = state, KiaHyundai = countKiaHyundaiThefts, Other = countOtherThefts)

# --- Data 3: KiaHyundaiMilwaukeeData.csv (Milwaukee-Specific Monthly Trends) ---
# Used for Area Chart (Kia/Hyundai trend in Milwaukee)
df_milwaukee <- read_csv("KiaHyundaiMilwaukeeData.csv") %>%
  # Create a proper Date column
  mutate(Date = myd(paste(month, year, "01"))) %>%
  # Convert counts to numeric
  mutate(across(starts_with("count"), as.numeric)) %>%
  select(Date, City = city, KiaHyundai = countKiaHyundaiThefts)

# --- Data 4: Motherboard VICE News Kia Hyundai Theft Data.xlsx ---
df_summary <- readxl::read_excel("Motherboard VICE News Kia Hyundai Theft Data.xlsx")

# Clean column names
colnames(df_summary) <- make.names(colnames(df_summary), unique = TRUE)

# Identify the city name columns (actual names like "Denver", "El Paso", etc.)
city_cols <- grep("^[A-Za-z]", colnames(df_summary), value = TRUE)

df_list <- list()

# Loop through each 3-column block: City | Total | KiaHyundai
for (city in city_cols) {
  i <- which(colnames(df_summary) == city)
  
  total_col <- colnames(df_summary)[i + 1]
  kiahy_col <- colnames(df_summary)[i + 2]
  
  temp <- df_summary %>%
    select(
      City = !!city,
      TotalThefts = !!total_col,
      KiaHyundaiThefts = !!kiahy_col
    ) %>%
    mutate(City = city)
  
  df_list[[city]] <- temp
}

# Combine all city blocks
df_summary_long <- bind_rows(df_list) %>%
  mutate(
    TotalThefts = as.numeric(TotalThefts),
    KiaHyundaiThefts = as.numeric(KiaHyundaiThefts),
    OtherThefts = TotalThefts - KiaHyundaiThefts
  ) %>%
  drop_na(TotalThefts) %>%
  group_by(City) %>%
  summarise(
    TotalKiaHyundai = sum(KiaHyundaiThefts, na.rm = TRUE),
    TotalAllThefts  = sum(TotalThefts, na.rm = TRUE),
    OtherThefts     = sum(OtherThefts, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(TotalAllThefts > 100)

print(head(df_summary_long))

# -----------------------------------------------
# 3. VISUALIZATION GENERATION
# -----------------------------------------------
plots <- list()

# --- V1: Pie Chart (Theft Composition by Type) ---
# Data: df_summary_long (Overall Theft Breakdown)
pie_data <- df_summary_long %>%
  summarise(
    KiaHyundai = sum(TotalKiaHyundai),
    Other = sum(OtherThefts)
  ) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
  mutate(
    Percentage = Count / sum(Count),
    Label = paste0(Type, " (", scales::percent(Percentage), ")")
  )

plots$pie_chart <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  # Add labels to the segments
  geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 0.5), size = 6, color = "white") +
  scale_fill_manual(values = c("KiaHyundai" = "#E3703E", "Other" = "#5D85A7")) +
  labs(
    title = "Kia/Hyundai's Share of Total Car Thefts (Aggregate)",
    subtitle = "Analysis of combined data from major US cities.",
    fill = "Vehicle Type"
  ) +
  theme_void()

# --- V2: Donut Chart (Top 5 Cities by Kia/Hyundai Theft Volume) ---
# Data: df_summary_long (Top 5 Kia/Hyundai contributors)
donut_data <- df_summary_long %>%
  arrange(desc(TotalKiaHyundai)) %>%
  slice(1:5) %>%
  mutate(
    City = factor(City, levels = City), # Ensure ordering
    Fraction = TotalKiaHyundai / sum(TotalKiaHyundai),
    ymax = cumsum(Fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymin + ymax) / 2,
    label = paste0(City, "\n", scales::number(TotalKiaHyundai, big.mark = ","))
  )

plots$donut_chart <- ggplot(donut_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = City)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  # Add labels outside the donut
  geom_text(x = 4.2, aes(y = labelPosition, label = City), size = 4.5) +
  labs(
    title = "Top 5 Cities Contributing to Kia/Hyundai Thefts",
    subtitle = "Focusing effort on these locations could yield the greatest reduction.",
    fill = "City"
  ) +
  theme_void() +
  theme(legend.position = "none")

# --- V3: Stacked Bar with Categorical Data (Theft Growth by Agency) ---
# Data: df_map (Top 10 Agencies by Total Theft)
stacked_bar_data <- df_map %>%
  arrange(desc(TotalThefts)) %>%
  slice(1:10) %>%
  pivot_longer(cols = c(`2019`, `2020`, `2021`, `2022`), names_to = "Year", values_to = "Count")

plots$stacked_bar <- ggplot(stacked_bar_data, aes(x = reorder(Agency, Count), y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() + # Flip to make agency names readable
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("2019" = "#B1D4E0", "2020" = "#73A5C6", "2021" = "#4682B4", "2022" = "#2A4D69")) +
  labs(
    title = "Total Car Thefts by Top 10 Agencies (2019-2022)",
    subtitle = "Identifies the highest-volume areas and their growth trends.",
    x = "Reporting Agency/Location",
    y = "Total Number of Car Thefts"
  )

# --- V4: Treemap (Relative Volume of Top 15 Agencies) ---
# Data: df_map (Top 15 Agencies by Total Theft)
treemap_data <- df_map %>%
  arrange(desc(TotalThefts)) %>%
  slice(1:15)

plots$treemap <- ggplot(treemap_data, aes(area = TotalThefts, fill = TotalThefts, label = Agency, subgroup = Agency)) +
  geom_treemap() +
  # Add the label inside the rectangles
  geom_treemap_text(
    aes(label = paste0(Agency, "\n", scales::number(TotalThefts, big.mark = ","))),
    colour = "white", place = "centre", size = 15, grow = TRUE
  ) +
  scale_fill_viridis_c(option = "plasma", labels = comma) +
  labs(
    title = "Volume of Car Thefts by Agency (Treemap)",
    subtitle = "Area size is proportional to total thefts across 2019-2022 for the top 15 agencies.",
    fill = "Total Thefts"
  ) +
  theme(legend.position = "none") # Legend unnecessary since color is continuous volume

# --- V5: Area Chart (Kia/Hyundai Theft Trend in Milwaukee) ---
# Data: df_milwaukee (Kia/Hyundai trend in Milwaukee)
plots$area_chart <- ggplot(df_milwaukee, aes(x = Date, y = KiaHyundai)) +
  geom_area(fill = "#FFC300", alpha = 0.7) +
  geom_line(color = "#DAA520", size = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Kia/Hyundai Thefts in Milwaukee: Monthly Trend",
    subtitle = "A specific look at the monthly volatility and peak periods for this vehicle type.",
    x = "Time",
    y = "Number of Kia/Hyundai Thefts"
  )

# --- V6: Stacked Area Chart (Kia/Hyundai vs. Other Thefts in Multi-City Aggregate) ---
# Data: df_multi (Multi-City trends, pivoted to long format)
stacked_area_data <- df_multi %>%
  # Aggregate by date across all cities for a combined view
  group_by(Date) %>%
  summarise(
    KiaHyundai = sum(KiaHyundai, na.rm = TRUE),
    Other = sum(Other, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Pivot to long format for ggplot2 stacked area
  pivot_longer(cols = c(KiaHyundai, Other), names_to = "Type", values_to = "Count")

plots$stacked_area <- ggplot(stacked_area_data, aes(x = Date, y = Count, fill = Type)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c("KiaHyundai" = "#C70039", "Other" = "#388E3C")) +
  labs(
    title = "Shift in Theft Composition Over Time (Multi-City Total)",
    subtitle = "Visualizing the rising proportion of Kia/Hyundai thefts (red) relative to all other vehicles (green).",
    x = "Time",
    y = "Total Monthly Car Thefts",
    fill = "Vehicle Type"
  )

# -----------------------------------------------
# 4. PRINT ALL PLOTS
# -----------------------------------------------
# Note: In a real R environment (like RStudio), this would open 6 separate plot windows.
# In a compiled environment, you would use ggsave() for each plot.

plots$pie_chart
plots$donut_chart
plots$stacked_bar
plots$treemap
plots$area_chart
plots$stacked_area

# Example of saving all plots (uncomment to run)
# ggsave("V1_PieChart.png", plot = plots$pie_chart, width = 10, height = 7)
# ggsave("V2_DonutChart.png", plot = plots$donut_chart, width = 10, height = 7)
# ggsave("V3_StackedBar.png", plot = plots$stacked_bar, width = 10, height = 7)
# ggsave("V4_Treemap.png", plot = plots$treemap, width = 10, height = 7)
# ggsave("V5_AreaChart.png", plot = plots$area_chart, width = 10, height = 7)
# ggsave("V6_StackedArea.png", plot = plots$stacked_area, width = 10, height = 7)

# ----------------------------------------------------------------------------
# CALL TO ACTION NARRATIVE (FOR THE DECISION MAKERS)
# ----------------------------------------------------------------------------
# The visualizations above tell a clear story across three key findings:
#
# 1. THE PROBLEM: The Pie Chart and Stacked Area Chart demonstrate that Kia/Hyundai vehicles
#    have rapidly moved from a small percentage of overall theft to a major driver.
#    The Stacked Area Chart (V6) clearly shows this transition over time.
#
# 2. THE HOTSPOTS: The Treemap (V4) and Stacked Bar Chart (V3) pinpoint the agencies
#    and jurisdictions experiencing the highest volume and fastest growth in thefts.
#    These are the mandatory targets for resource allocation.
#
# 3. THE URGENCY: The Area Chart (V5 - Milwaukee specific) shows how quickly the
#    theft trend can accelerate (the "Kia Challenge" effect). Proactive measures are
#    necessary to prevent this rapid acceleration in other vulnerable cities (identified
#    in the Donut Chart, V2).
#
# RECOMMENDATION:
#   - Focus immediate police and community resources on the top 5 agencies identified
#     in the Treemap and Donut Chart.
#   - Launch targeted public awareness campaigns and offer steering wheel locks or
#     immobilizer kits for pre-2022 Kia/Hyundai models in those identified hotspots.
#
# ----------------------------------------------------------------------------