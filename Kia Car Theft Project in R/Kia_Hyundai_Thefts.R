# ------------------------------------------------------------------------------
# Kia and Hyundai Car Thefts: A Report for City Decision-Makers
# ------------------------------------------------------------------------------
# Question: how much of the recent rise in car thefts comes from Kia and Hyundai
#           vehicles, and where should a city focus its prevention efforts?
#
# Data (Motherboard/VICE News Kia and Hyundai theft data):
#   carTheftsMap.csv                         yearly car thefts by reporting agency, 2019-2022
#   kiaHyundaiThefts.csv                     monthly Kia/Hyundai and other thefts in several cities
#   KiaHyundaiMilwaukeeData.csv              monthly Kia/Hyundai thefts in Milwaukee
#   Motherboard VICE News Kia Hyundai Theft Data Upd.xlsx   theft totals by city
#
# Output: six charts saved to the charts/ folder, plus key numbers printed to the console.
#
# Run from this folder:  Rscript Kia_Hyundai_Thefts.R
# Packages: tidyverse, readxl, treemapify, lubridate, scales
#   install.packages(c("tidyverse", "readxl", "treemapify", "lubridate", "scales"))
#
# Context: master's coursework (DSC640, Weeks 5 and 6), cleaned up for this portfolio.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(treemapify)
  library(lubridate)
  library(scales)
})

# consistent, uncluttered theme and a color-blind friendly palette (Okabe-Ito colors)
theme_set(theme_minimal(base_size = 14) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
                  plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
                  axis.title = element_text(size = 12),
                  legend.position = "bottom"))
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
dir.create("charts", showWarnings = FALSE)

# LOAD AND CLEAN ---------------------------------------------------------------

# yearly thefts by reporting agency: make the year columns numeric and add a 4-year total
df_carTheftsMap <- read_csv("carTheftsMap.csv", show_col_types = FALSE) %>%
  mutate(across(starts_with("countCarThefts"), ~ as.numeric(gsub("[^0-9.]", "", .)))) %>%
  rowwise() %>%
  mutate(TotalThefts = sum(c_across(starts_with("countCarThefts")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Agency = geo_name, `2019` = countCarThefts2019, `2020` = countCarThefts2020,
         `2021` = countCarThefts2021, `2022` = countCarThefts2022, TotalThefts)

# monthly Kia/Hyundai vs. other thefts across cities
df_kiaHyundaiThefts <- read_csv("kiaHyundaiThefts.csv", show_col_types = FALSE) %>%
  mutate(Date = myd(paste(month, year, "01"))) %>%
  mutate(across(starts_with("count"), as.numeric)) %>%
  select(Date, City = city, State = state, KiaHyundai = countKiaHyundaiThefts, Other = countOtherThefts)

# monthly Kia/Hyundai thefts in Milwaukee
df_milwaukee <- read_csv("KiaHyundaiMilwaukeeData.csv", show_col_types = FALSE) %>%
  mutate(Date = myd(paste(month, year, "01"))) %>%
  mutate(across(starts_with("count"), as.numeric)) %>%
  select(Date, City = city, KiaHyundai = countKiaHyundaiThefts)

# theft totals by city: the sheet has one "City_All" and one "City_Kia/Hyundais" column per city
df_summary_long_format <- read_excel("Motherboard VICE News Kia Hyundai Theft Data Upd.xlsx") %>%
  rename(Date = 1) %>%
  mutate(across(-Date, as.numeric)) %>%
  drop_na(Date) %>%
  pivot_longer(cols = -Date, names_to = c("City", ".value"), names_sep = "_") %>%
  mutate(City = str_trim(City),
         City = str_remove(City, "[A-Z]{2}$"),                       # some names end in a state code, e.g. "MilwaukeeWI"
         City = str_replace_all(City, "([a-z])([A-Z])", "\\1 \\2")) %>%   # "LosAngeles" -> "Los Angeles"
  group_by(City) %>%
  summarise(TotalKiaHyundai = sum(`Kia/Hyundais`, na.rm = TRUE),
            TotalAllThefts = sum(All, na.rm = TRUE), .groups = "drop") %>%
  mutate(OtherThefts = TotalAllThefts - TotalKiaHyundai) %>%
  filter(TotalAllThefts > 100)          # leave out cities with very few reported thefts

# KEY NUMBERS ------------------------------------------------------------------

cat("\n--- Key numbers ---\n")
cat(sprintf("Cities in the summary data (over 100 thefts): %d\n", nrow(df_summary_long_format)))
kia_share <- sum(df_summary_long_format$TotalKiaHyundai) / sum(df_summary_long_format$TotalAllThefts)
cat(sprintf("Kia/Hyundai share of all thefts in those cities: %.1f%% (%s of %s)\n", 100 * kia_share,
            comma(sum(df_summary_long_format$TotalKiaHyundai)), comma(sum(df_summary_long_format$TotalAllThefts))))
top5 <- df_summary_long_format %>% arrange(desc(TotalKiaHyundai)) %>% slice(1:5)
cat("Top 5 cities by Kia/Hyundai thefts:\n"); print(top5 %>% mutate(KiaShare = round(100 * TotalKiaHyundai / TotalAllThefts, 1)))
cat(sprintf("Top 5 cities' share of all Kia/Hyundai thefts in the data: %.1f%%\n",
            100 * sum(top5$TotalKiaHyundai) / sum(df_summary_long_format$TotalKiaHyundai)))

yearly <- df_carTheftsMap %>% summarise(across(c(`2019`, `2020`, `2021`, `2022`), ~ sum(.x, na.rm = TRUE)))
cat("Total thefts across all reporting agencies by year:\n"); print(yearly)
cat(sprintf("Change 2019 to 2022: %.1f%%\n", 100 * (yearly$`2022` / yearly$`2019` - 1)))

monthly_share <- df_kiaHyundaiThefts %>% group_by(Date) %>%
  summarise(KiaHyundai = sum(KiaHyundai, na.rm = TRUE), Other = sum(Other, na.rm = TRUE), .groups = "drop") %>%
  mutate(Share = KiaHyundai / (KiaHyundai + Other))
cat(sprintf("Multi-city data: %s to %s\n", format(min(monthly_share$Date), "%b %Y"), format(max(monthly_share$Date), "%b %Y")))
cat(sprintf("Kia/Hyundai share of monthly thefts: %.1f%% in the first month, peak %.1f%% (%s), %.1f%% in the last month\n",
            100 * first(monthly_share$Share), 100 * max(monthly_share$Share),
            format(monthly_share$Date[which.max(monthly_share$Share)], "%b %Y"), 100 * last(monthly_share$Share)))

mke <- df_milwaukee %>% arrange(Date)
cat(sprintf("Milwaukee: %d Kia/Hyundai thefts in %s, peak %d in %s, %d in %s\n",
            first(mke$KiaHyundai), format(first(mke$Date), "%b %Y"), max(mke$KiaHyundai),
            format(mke$Date[which.max(mke$KiaHyundai)], "%b %Y"), last(mke$KiaHyundai), format(last(mke$Date), "%b %Y")))

# VISUALIZATIONS ---------------------------------------------------------------

plots <- list()

# 1: overall share of Kia/Hyundai thefts
pie_data <- df_summary_long_format %>%
  summarise(KiaHyundai = sum(TotalKiaHyundai), Other = sum(OtherThefts)) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
  mutate(Percentage = Count / sum(Count))
plots$pie_chart <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percent(Percentage, accuracy = 0.1)), position = position_stack(vjust = 0.5),
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("KiaHyundai" = cb_palette[6], "Other" = cb_palette[5]),
                    labels = c("Kia/Hyundai", "Other vehicles")) +
  labs(title = "Kia/Hyundai Share of Car Thefts", subtitle = "Combined theft counts from the cities in the data.",
       fill = "Type of car") +
  theme_void() + theme(legend.position = "right")

# 2: top 5 cities by Kia/Hyundai thefts (labels show each city's share of the top-5 total)
donut_data <- top5 %>%
  mutate(City = factor(City, levels = City), Fraction = TotalKiaHyundai / sum(TotalKiaHyundai),
         ymax = cumsum(Fraction), ymin = c(0, head(ymax, n = -1)), labelPosition = (ymin + ymax) / 2,
         Label = paste0(City, "\n", comma(TotalKiaHyundai)))
plots$donut_chart <- ggplot(donut_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = City)) +
  geom_rect() + coord_polar(theta = "y") + xlim(c(2, 4.6)) +
  geom_text(x = 4.4, aes(y = labelPosition, label = Label), size = 4.5, lineheight = 1.1) +
  scale_fill_manual(values = cb_palette[c(1, 2, 3, 5, 7)]) +
  labs(title = "Top 5 Cities for Kia/Hyundai Thefts",
       subtitle = "Number of Kia/Hyundai thefts; focusing here could create the greatest reduction.") +
  theme_void() + theme(legend.position = "none")

# 3: yearly thefts at the 10 highest-volume agencies
stacked_bar_data <- df_carTheftsMap %>% arrange(desc(TotalThefts)) %>% slice(1:10) %>%
  pivot_longer(cols = c(`2019`, `2020`, `2021`, `2022`), names_to = "Year", values_to = "Count")
plots$stacked_bar <- ggplot(stacked_bar_data, aes(x = reorder(Agency, TotalThefts), y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  scale_y_continuous(labels = comma) + scale_fill_manual(values = cb_palette[1:4]) +
  labs(title = "Yearly Vehicle Thefts at the Top 10 Reporting Agencies",
       subtitle = "2019-2022; growth shows which locations need more attention.",
       x = "Reporting agency", y = "Number of car thefts")

# 4: relative volume of the 15 highest-volume agencies
treemap_data <- df_carTheftsMap %>% arrange(desc(TotalThefts)) %>% slice(1:15)
plots$treemap <- ggplot(treemap_data, aes(area = TotalThefts, fill = TotalThefts, label = Agency)) +
  geom_treemap(colour = "white") +
  geom_treemap_text(aes(label = paste0(Agency, "\n", number(TotalThefts, big.mark = ","))),
                    colour = "white", place = "centre", size = 15, grow = TRUE, fontface = "bold") +
  scale_fill_gradient(low = "#56B4E9", high = "#0072B2") +
  labs(title = "Relative Scale of Car Thefts by Agency",
       subtitle = "Area is proportional to total thefts, 2019-2022, for the top 15 agencies.") +
  theme(legend.position = "none")

# 5: monthly Kia/Hyundai thefts in Milwaukee
plots$area_chart <- ggplot(df_milwaukee, aes(x = Date, y = KiaHyundai)) +
  geom_area(fill = cb_palette[1], alpha = 0.7) +
  geom_line(color = cb_palette[6], linewidth = 1.2) +
  scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = "Monthly Kia/Hyundai Thefts in Milwaukee",
       subtitle = "Thefts climbed from a few dozen a month to several hundred.", x = "", y = "Kia/Hyundai thefts")

# 6: Kia/Hyundai vs. other thefts over time, all cities in the monthly data
stacked_area_data <- monthly_share %>%
  pivot_longer(cols = c(KiaHyundai, Other), names_to = "Type", values_to = "Count") %>%
  mutate(Type = factor(Type, levels = c("Other", "KiaHyundai")))
plots$stacked_area <- ggplot(stacked_area_data, aes(x = Date, y = Count, fill = Type)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c("KiaHyundai" = cb_palette[6], "Other" = cb_palette[3]),
                    labels = c("KiaHyundai" = "Kia/Hyundai", "Other" = "Other vehicles")) +
  labs(title = "Monthly Car Thefts Across Cities", subtitle = "Kia/Hyundai thefts make up a growing share.",
       x = "", y = "Car thefts per month", fill = "Type of car")

# SAVE AND SHOW ----------------------------------------------------------------

for (name in names(plots)) {
  ggsave(file.path("charts", paste0(name, ".png")), plots[[name]], width = 10, height = 7, dpi = 150, bg = "white")
}
cat(sprintf("\nSaved %d charts to the charts/ folder\n", length(plots)))
if (interactive()) for (p in plots) print(p)
