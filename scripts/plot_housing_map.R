# plot_housing_map.R
# 繪製 2024 年 2 月紐西蘭區域房價地圖（REINZ 樣式 + 白底框 + 陰影文字 + 彩色三角形）

library(sf)
library(ggplot2)
library(dplyr)
library(scales)
library(viridis)
library(readr)
library(shadowtext)

# 讀入 shapefile
nz_map <- st_read("data/shapefile/regional-council-2023-generalised.shp") %>%
  rename(region = REGC2023_1)

# 讀入房價資料
price_data <- read_csv("data/nz_rent_202502.csv") %>%
  mutate(
    direction = case_when(
      change > 0 ~ "▲",
      change < 0 ~ "▼",
      TRUE ~ ""
    ),
    region_name = gsub(" Region", "", region)
  )

# 合併資料
map_data <- left_join(nz_map, price_data, by = "region")

# 建立標籤位置與文字內容（使用投影座標系的 X/Y 偏移）
label_points <- map_data %>%
  filter(!is.na(price)) %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(
    price_label = paste0("$", comma(price)),
    change_num = sprintf("%.1f%%", change),
    label = paste(region_name, price_label, change_num, sep = "\n"),
    
    triangle_color = case_when(
      change > 0 ~ "#008000",  # 綠色
      change < 0 ~ "#D00000",  # 紅色
      TRUE ~ "black"
    ),
    
    direction = case_when(
      change > 0 ~ "▲",
      change < 0 ~ "▼",
      TRUE ~ ""
    ),
    
    label_x = case_when(
      region_name == "Auckland" ~ X - 320000,
      region_name == "Wellington" ~ X + 150000,
      region_name == "Canterbury" ~ X + 200000,
      region_name == "Otago" ~ X + 240000,
      region_name == "Northland" ~ X + 140000,
      region_name == "Bay of Plenty" ~ X + 35000,
      region_name == "Gisborne" ~ X + 100000,
      region_name == "Hawke's Bay" ~ X + 150000,
      region_name == "Tasman" ~ X - 330000,
      region_name == "Southland" ~ X + 330000,
      region_name == "Waikato" ~ X - 300000,
      region_name == "Taranaki" ~ X - 250000,
      region_name == "Manawatū-Whanganui" ~ X + 350000,
      region_name == "Nelson" ~ X - 330000,
      region_name == "Marlborough" ~ X + 200000,
      region_name == "West Coast" ~ X - 310000,
      TRUE ~ X
    ),
    
    label_y = case_when(
      region_name == "Auckland" ~ Y + 10000,
      region_name == "Wellington" ~ Y - 50000,
      region_name == "Canterbury" ~ Y -100000,
      region_name == "Otago" ~ Y - 20000,
      region_name == "Northland" ~ Y + 10000,
      region_name == "Bay of Plenty" ~ Y + 150000,
      region_name == "Gisborne" ~ Y + 50000,
      region_name == "Hawke's Bay" ~ Y - 5000,
      region_name == "Tasman" ~ Y - 70000,
      region_name == "Southland" ~ Y - 115000,
      region_name == "Waikato" ~ Y + 5000,
      region_name == "Taranaki" ~ Y + 3000,
      region_name == "Manawatū-Whanganui" ~ Y - 133000,
      region_name == "Nelson" ~ Y + 68000,
      region_name == "Marlborough" ~ Y - 200000,
      region_name == "West Coast" ~ Y - 50000,
      TRUE ~ Y
    ),
    
    triangle_x = label_x + 105000,
    triangle_y = label_y - 46000,
    
    hjust = 0
  )

# 繪圖
p <- ggplot(map_data) +
  geom_sf(aes(fill = price), color = "white") +
  geom_segment(
    data = label_points,
    aes(x = X, y = Y, xend = label_x, yend = label_y),
    color = "gray50", size = 0.3, linetype = "dashed"
  ) +
  shadowtext::geom_shadowtext(
    data = label_points,
    aes(x = label_x, y = label_y, label = label, hjust = hjust),
    size = 3, fontface = "bold", color = "black", bg.color = "white", bg.r = 0.1
  ) +
  geom_text(
    data = label_points,
    aes(x = triangle_x, y = triangle_y, label = direction, color = triangle_color),
    size = 4, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Median Price (NZD)", labels = comma, na.value = "grey80")+
  theme_minimal(base_family = "Helvetica") + theme(axis.title= element_blank()) +
  coord_sf(crs = 2193) +
  labs(
    title = "Median Rent by Region – February 2025",
    subtitle = "Values in NZD. Includes regional median rent and monthly change.",
    caption = "Source: MBIE Tenancy Services"
  )

#title = "Median House Prices by Region – February 2025",
#subtitle = "Values in NZD. Includes regional price and monthly change.",
#caption = "Source: REINZ"

#title = "Median Rent by Region – February 2025",
#subtitle = "Values in NZD. Includes regional median rent and monthly change.",
#caption = "Source: MBIE Tenancy Services"

# 顯示與儲存
print(p)
ggsave("plots/housing_rent_map_202502.png", plot = p, width = 10, height = 8, dpi = 300)
