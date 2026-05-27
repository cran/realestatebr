## ----knitr-setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.asp = 0.618,
  fig.align = "center",
  message = FALSE,
  warning = FALSE
)

## ----load-packages------------------------------------------------------------
library(realestatebr)
library(dplyr)

## ----setup, message = FALSE---------------------------------------------------
library(ggplot2)

color_palette <- c(
  "#1E3A5F",
  "#DD6B20",
  "#2C7A7B",
  "#D69E2E",
  "#805AD5",
  "#C53030"
)

theme_series <- function() {
  theme_minimal(
    # swap for other font if needed
    base_family = "Avenir",
    base_size = 10
  ) +
    theme(
      plot.title = element_text(size = 16),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(color = "gray10", linewidth = 0.5),
      axis.ticks.x = element_line(color = "gray10", linewidth = 0.5),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      palette.color.discrete = color_palette
    )
}

## ----load-datasets-overview---------------------------------------------------
# igmi <- get_dataset("rppi", table = "igmi")
# ivar <- get_dataset("rppi", table = "ivar")
# fipezap <- get_dataset("rppi", table = "fipezap")

## ----load-stacked-datasets----------------------------------------------------
# sale <- get_dataset("rppi", table = "sale")
# rent <- get_dataset("rppi", table = "rent")

## ----ivgr-load----------------------------------------------------------------
ivgr <- get_dataset("rppi", "ivgr")

glimpse(ivgr)

## ----ivgr-plot----------------------------------------------------------------
ggplot(ivgr, aes(date, index)) +
  geom_line(color = color_palette[1], linewidth = 0.7) +
  labs(
    title = "IVG-R — National Sale Index",
    x = NULL,
    y = "Index"
  ) +
  theme_series()

## ----igmi-load----------------------------------------------------------------
igmi <- get_dataset("rppi", "igmi")

glimpse(igmi)

## ----igmi-plot----------------------------------------------------------------
main_cities <- c("São Paulo", "Rio De Janeiro", "Belo Horizonte", "Brasília")

subigmi <- igmi |>
  filter(name_muni %in% main_cities)

ggplot(subigmi, aes(date, index, color = name_muni)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "IGMI-R — Sale Index by City",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_series()

## ----fipezap-sale-load--------------------------------------------------------
fz <- get_dataset("rppi", table = "fipezap")

glimpse(fz)

## ----fipezap-sale-filter------------------------------------------------------
subzap <- fz |>
  filter(
    market == "residential",
    rent_sale == "sale",
    rooms == "total",
    variable == "index",
    name_muni %in% main_cities
  )

## ----fipezap-sale-plot--------------------------------------------------------
ggplot(subzap, aes(date, value, color = name_muni)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "FipeZap — Residential Sale Index",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_series()

## ----sale-stacked-load--------------------------------------------------------
sale_indices <- get_dataset("rppi", "sale")

glimpse(sale_indices)

## ----sale-stacked-filter------------------------------------------------------
comp_index <- sale_indices |>
  filter(name_muni == "Brazil", date >= as.Date("2015-01-01"))

## ----sale-stacked-plot--------------------------------------------------------
ggplot(comp_index, aes(date, acum12m * 100, color = source)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Comparing Sale Indices in Brazil",
    subtitle = "12-month accumulated change (%)",
    y = "%",
    color = NULL
  ) +
  theme_series()

## ----ivar-load----------------------------------------------------------------
ivar <- get_dataset("rppi", table = "ivar")

glimpse(ivar)

## ----ivar-trend---------------------------------------------------------------
library(trendseries)

ivar_trend <- ivar |>
  filter(name_muni != "Brazil") |>
  augment_trends(
    value_col = "index",
    group_cols = "name_muni",
    method = "ma",
    window = 5
  )

## ----ivar-plot----------------------------------------------------------------
ggplot(ivar_trend, aes(date, color = name_muni)) +
  geom_line(aes(y = index), lwd = 0.5, alpha = 0.5) +
  geom_line(aes(y = trend_ma), lwd = 0.7) +
  geom_hline(yintercept = 100) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IVAR — City Rent Indices",
    subtitle = "Smoothed moving average (5-month window)",
    x = NULL,
    y = "Index"
  ) +
  theme_series()

## ----iqa-iqaiw-load-----------------------------------------------------------
iqa <- get_dataset("rppi", "iqa")
iqaiw <- get_dataset("rppi", "iqaiw")

glimpse(iqaiw)

## ----iqa-plot-----------------------------------------------------------------
ggplot(iqa, aes(date, index, color = name_muni)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 100) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IQA — Rent Index",
    subtitle = "Index (2019/06 = 100)",
    y = "Index",
    color = NULL
  ) +
  theme_series()

## ----iqaiw-plot---------------------------------------------------------------
ggplot(
  subset(iqaiw, rooms == "total" & !is.na(acum12m)),
  aes(date, acum12m * 100, color = name_muni)
) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "IQAIW — Rent Index",
    subtitle = "Accumulated 12-month change (%)",
    y = "%",
    color = NULL
  ) +
  theme_series()

## ----quintoandar-combine------------------------------------------------------
quintoandar <- bind_rows(
  list("IQA" = iqa, "IQAIW" = iqaiw),
  .id = "source"
)

quintoandar_spo <- quintoandar |>
  filter(name_muni == "São Paulo", !rooms %in% c("1", "2", "3"))

## ----quintoandar-compare------------------------------------------------------
ggplot(quintoandar_spo, aes(date, index, color = source)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 100) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "QuintoAndar Rent Index — São Paulo",
    subtitle = "IQA (pre-2023) and IQAIW (post-2023) use different methodologies",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_series()

## ----fipezap-rent-load--------------------------------------------------------
fz <- get_dataset("rppi", table = "fipezap")

glimpse(fz)

## ----fipezap-rent-filter------------------------------------------------------
fz_rent <- fz |>
  filter(
    market == "residential",
    rent_sale == "rent",
    rooms == "total",
    variable == "acum12m",
    date >= as.Date("2019-01-01")
  )

sel_cities <- fz_rent |>
  filter(date == "2019-01-01", !is.na(value)) |>
  pull(name_muni)

## ----fipezap-rent-plot--------------------------------------------------------
ggplot(subset(fz_rent, name_muni %in% sel_cities), aes(date, value * 100)) +
  geom_line(linewidth = 0.7, color = color_palette[1]) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(name_muni)) +
  labs(
    title = "FipeZap — 12-month Rent Change by City",
    x = NULL,
    y = "Accumulated 12-month change (%)"
  ) +
  theme_series()

## ----secovi-load--------------------------------------------------------------
secovi <- get_dataset("rppi", "secovi_sp")

glimpse(secovi)

## ----secovi-plot--------------------------------------------------------------
ggplot(secovi, aes(date, acum12m * 100)) +
  geom_line(color = color_palette[1], linewidth = 0.7) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "SECOVI-SP — Residential Rent Index",
    subtitle = "12-month accumulated change (%)",
    x = NULL,
    y = "%"
  ) +
  theme_series()

## ----rent-stacked-load--------------------------------------------------------
rent_indices <- get_dataset("rppi", "rent")

## ----rent-stacked-filter------------------------------------------------------
rent_indices_comp <- rent_indices |>
  filter(
    name_muni %in% c("São Paulo", "Rio de Janeiro"),
    date >= as.Date("2019-01-01")
  )

## ----rent-stacked-plot--------------------------------------------------------
ggplot(rent_indices_comp, aes(date, acum12m, color = source)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(name_muni)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Rent Indices — São Paulo and Rio de Janeiro",
    subtitle = "12-month accumulated change",
    y = "Accumulated change",
    color = NULL
  ) +
  theme_series()

## ----sale-rebased-load--------------------------------------------------------
sales <- get_dataset("rppi", "sale")

national <- sales |>
  filter(
    name_muni == "Brazil",
    date >= as.Date("2018-01-01"),
    date <= as.Date("2023-12-01")
  )

national_rebased <- national |>
  mutate(
    index_rebased = index / first(index) * 100,
    .by = source
  )

total_growth <- national_rebased |>
  summarise(
    growth = last(index_rebased) - first(index_rebased),
    date = last(date),
    index_rebased = last(index_rebased),
    .by = source
  ) |>
  mutate(label = sprintf("%s:\n+%.1f%%", source, growth))

## ----sale-rebased-plot--------------------------------------------------------
ggplot(national_rebased, aes(date, index_rebased, color = source)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100) +
  geom_label(
    data = total_growth,
    aes(label = label),
    hjust = 0,
    nudge_x = 30,
    nudge_y = c(-5, 10, -5),
    show.legend = FALSE,
    size = 3
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.125))
  ) +
  labs(
    title = "Brazil National Sale Indices — Rebased to Jan 2018",
    x = NULL,
    y = "Index (Jan 2018 = 100)",
    color = NULL
  ) +
  theme_series()

## ----bis-load-----------------------------------------------------------------
bis <- get_dataset("rppi_bis")

bis_sub <- bis |>
  filter(
    ref_area_name %in% c("Brazil", "United States", "Germany", "Japan"),
    is_nominal == 0,
    unit == "index",
    date >= as.Date("2000-01-01")
  )

## ----bis-plot-----------------------------------------------------------------
ggplot(bis_sub, aes(date, value, color = ref_area_name)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.4) +
  labs(
    title = "Real Residential Property Prices",
    subtitle = "BIS, index 2010 = 100",
    x = NULL,
    y = "Index",
    color = NULL
  ) +
  theme_series()

