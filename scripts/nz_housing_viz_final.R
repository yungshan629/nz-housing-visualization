# plot_all_slides.R
# 圖表總整，用於 Keynote Slide 1~6 繪圖（更新版：統一圖例位置、色彩、資料來源）

# ==== 套件載入 ====
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(zoo)
library(stringr)
library(colorspace)
# ==== 通用主題與色彩 ====
custom_theme <- theme_minimal(base_family = "Helvetica") +
  theme(legend.position = "bottom")
custom_colors <- c("#ACA4E2", "#87AEDF", "#5FB6D4", "#3DBCC2", "#3DBEAB", "#5CBD92")
cold_palette <- qualitative_hcl(6, "Cold")

# ==== Slide 1 ====
housing <- read_csv("data/nz-house.csv") %>%
  mutate(Dates = ymd(Dates)) %>%
  arrange(Dates) %>%
  mutate(median_price_ma12 = rollmean(median_price, 12, fill = NA, align = "right"))

housing_long <- housing %>%
  select(Dates, median_price, median_price_ma12) %>%
  pivot_longer(-Dates, names_to = "type", values_to = "value")

p1_1 <- ggplot(housing_long, aes(x = Dates, y = value, color = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cold_palette[c(3, 5)], labels = c("Median Price", "12-month MA")) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "Median House Price in New Zealand", subtitle = "Includes 12-month Moving Average", caption="Source: Bloomberg", x = NULL, y = "Price (NZD)", color = NULL) +
  custom_theme

print(p1_1)
ggsave("plots/median-price-trend.png", p1_1, width = 6, height = 4, dpi = 300)

p1_2 <- ggplot(housing, aes(x = Dates)) +
  geom_line(aes(y = median_price_yoy, color = "Price YoY"), size = 1) +
  geom_line(aes(y = (mortgage_rate - 5.5) * 10, color = "Mortgage Rate"), size = 1) +
  scale_y_continuous(name = "Price YoY (%)", sec.axis = sec_axis(~ . / 10 + 5.5, name = "Mortgage Rate (%)")) +
  scale_color_manual(values = cold_palette[c(3, 5)], name = NULL) +
  labs(title = "House Price YoY vs Mortgage Rate", subtitle = "Inverse relationship between interest rate and price growth", caption="Source: Bloomberg", x = NULL) +
  custom_theme
print(p1_2)
ggsave("plots/mortgage-rate-vs-price-yoy.png", p1_2, width = 6, height = 4, dpi = 300)

# ==== Slide 2 ====
regional_house <- read_csv("data/nz_regional_house_prices.csv") %>% rename(date = 1) %>% mutate(date = ymd(date))

selected_regions <- c("auckland", "wellington", "canterbury", "otago", "waikato")

filtered_data <- regional_house %>%
  pivot_longer(-date, names_to = "region", values_to = "price") %>%
  filter(region %in% selected_regions)

p2_1 <- ggplot(filtered_data, aes(x = date, y = price, color = region)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = cold_palette) +
  labs(title = "Regional Median House Prices", subtitle = "Selected New Zealand Regions", caption="Source: REINZ", x = NULL, y = "Price (NZD)", color = "Region") +
  custom_theme
print(p2_1)
ggsave("plots/slide2_price_trend.png", p2_1, width = 6, height = 4, dpi = 300)

house_long <- regional_house %>%
  pivot_longer(-date, names_to = "region", values_to = "price") %>%
  filter(!is.na(price)) %>%
  mutate(year = year(date), period = case_when(year == 2020 ~ "2020", year == 2024 ~ "2024")) %>%
  filter(!is.na(period))

summary_table <- house_long %>%
  group_by(region, period) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

p2_2 <- ggplot(summary_table, aes(x = reorder(region, -avg_price), y = avg_price, fill = period)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = cold_palette[c(3, 5)]) +
  labs(title = "Average Regional House Prices", subtitle = "Comparison of 2020 and 2024", caption="Source: REINZ", x = "Region", y = "Average Price (NZD)", fill = "Year") +
  custom_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2_2)
ggsave("plots/slide2_avg_bar.png", p2_2, width = 6, height = 4, dpi = 300)

# ==== Slide 3_1：租金趨勢 ====
rental_data <- read_csv("data/nz_regional_rental.csv") %>%
  rename(date = `Time Frame`, region = Location, rent = `Median Rent`) %>%
  filter(!is.na(region), !is.na(rent)) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= as.Date("2019-01-01"))

top_regions <- c("Auckland Region", "Wellington Region", "Canterbury Region", "Otago Region", "Waikato Region")

filtered <- rental_data %>% filter(region %in% top_regions)

p3_1 <- ggplot(filtered, aes(x = date, y = rent, color = region)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cold_palette[1:5]) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Regional Median Monthly Rents (Last 5 Years)",
    subtitle = "Top 5 NZ Regions", caption = "Source: MBIE",
    x = NULL,
    y = "Rent (NZD/week)",
    color = "Region"
  ) + custom_theme
print(p3_1)
ggsave("plots/slide3_rent_trend.png", p3_1, width = 6, height = 4, dpi = 300)

# ==== Slide 3_2：房價 vs 租金（含特定地區標示與年份回歸線） ====
# 圖 3_2 要畫租金 vs 房租，這部分花比較多時間 merge 資料
# 對應表
region_map <- tribble(
  ~house_region,             ~rental_region,
  "northland",               "northland_region",
  "auckland",                "auckland_region",
  "waikato",                 "waikato_region",
  "bay_of_plenty",           "bay_of_plenty_region",
  "gisborne",                "gisborne_region",
  "hawkes_bay",              "hawkes_bay_region",
  "taranaki",                "taranaki_region",
  "manawatu_whanganui",      "manawatu_wanganui_region",
  "wellington",              "wellington_region",
  "nelson_tasman_marlborough", "nelson",
  "nelson_tasman_marlborough", "tasman",
  "nelson_tasman_marlborough", "marlborough",
  "west_coast",              "west_coast_region",
  "canterbury",              "canterbury_region",
  "otago",                   "otago_region",
  "southland",               "southland_region"
)

# 房價處理：每年每地區平均
house <- read_csv("data/nz_regional_house_prices.csv") %>%
  mutate(date = ymd(date), year = year(date)) %>%
  filter(year %in% 2020:2024) %>%
  pivot_longer(cols = -c(date, year), names_to = "region", values_to = "price") %>%
  group_by(region = str_to_lower(region), year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

# 租金處理：合併 Nelson/Tasman/Marlborough
rental <- read_csv("data/nz_regional_rental.csv") %>%
  rename(date = `Time Frame`, region = Location, rent = `Median Rent`) %>%
  mutate(
    region = str_to_lower(region) %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("'s", "s") %>%
      str_replace_all("-", "_"),
    year = year(date)
  ) %>%
  filter(year %in% 2020:2024)

nelson_group <- rental %>%
  filter(region %in% c("nelson_region", "tasman_region", "marlborough_region")) %>%
  group_by(date, year) %>%
  summarise(rent = mean(rent, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "nelson_tasman_marlborough")

rental_clean <- rental %>%
  filter(!region %in% c("nelson_region", "tasman_region", "marlborough_region")) %>%
  bind_rows(nelson_group) %>%
  left_join(region_map, by = c("region" = "rental_region")) %>%
  mutate(region = coalesce(house_region, region)) %>%
  filter(!is.na(region)) %>%
  group_by(region, year) %>%
  summarise(avg_rent = mean(rent, na.rm = TRUE), .groups = "drop")

# 合併
merged_yearly <- inner_join(house, rental_clean, by = c("region", "year"))

# 檢查

merged_year <- merged_yearly %>%
  mutate(shape = case_when(
    region == "auckland" ~ "Auckland",
    region == "waikato" ~ "Waikato",
    region == "canterbury" ~ "Canterbury",
    TRUE ~ "Other"
  ))

p3_2 <- ggplot(merged_year, aes(x = avg_rent, y = avg_price, color = shape, shape = shape)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(
    data = merged_year %>% filter(year == 2020),
    method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.8
  ) +
  geom_smooth(
    data = merged_year %>% filter(year == 2024),
    method = "lm", se = FALSE, color = "black", linetype = "solid", linewidth = 0.8
  ) +
  annotate("text", x = 390, y = 0.55e6, label = "2020", color = "black", hjust = 0, size = 3.5) +
  annotate("text", x = 520, y = 0.55e6, label = "2024", color = "black", hjust = 0, size = 3.5) +
  scale_color_manual(
    values = c("Auckland" = cold_palette[1], "Waikato" = cold_palette[3], "Canterbury" = cold_palette[6], "Other" = "gray60")
  ) +
  scale_shape_manual(
    values = c("Auckland" = 18, "Waikato" = 17, "Canterbury" = 15, "Other" = 16)
  ) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Regional Median House Price vs Rent (2020–2024)",
    caption  = "Source: MBIE & REINZ",
    x = "Average Rent (NZD)",
    y = "Average House Price (NZD)",
    color = "Region",
    shape = "Region"
  ) +
  custom_theme
print(p3_2)
ggsave("plots/slide3_price_vs_rent.png", p3_2, width = 6, height = 4, dpi = 300)

# ==== Slide 6_1：建照趨勢圖 ====
p6_1 <- ggplot(housing, aes(x = Dates, y = building_value)) +
  geom_line(color = cold_palette[3], size = 1.2) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Value of Residential Building Consents",
    caption = "Source: Bloomberg",
    x = NULL,
    y = "Value (NZD)"
  ) + custom_theme
print(p6_1)
ggsave("plots/slide6_building_consent.png", p6_1, width = 6, height = 4, dpi = 300)

# ==== Slide 6_2：銷售年增率圖（含解封高峰註記） ====
p6_2 <- ggplot(housing, aes(x = Dates, y = sales_yoy)) +
  geom_line(color = cold_palette[3], size = 1.2) +
  annotate("text",
           x = as.Date("2017-6-01"),
           y = 48,
           label = "Post-lockdown:     ↑\nPeaked at 419.7%",
           color = "red", size = 3, hjust = 0) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  coord_cartesian(ylim=c(NA, 50)) +
  labs(
    title = "Year-over-Year Change in Dwelling Sales",
    caption = "Source: Bloomberg",
    x = NULL,
    y = "YoY (%)"
  ) + custom_theme
print(p6_2)
ggsave("plots/slide6_sales_yoy.png", p6_2, width = 6, height = 4, dpi = 300)

