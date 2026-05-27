## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.asp = 0.618,
  fig.align = "center",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)

## -----------------------------------------------------------------------------
# # Default table
# abecip <- get_dataset("abecip")
# 
# # Specific table
# sbpe <- get_dataset("abecip", table = "units")

## -----------------------------------------------------------------------------
ds <- list_datasets()

## -----------------------------------------------------------------------------
ds |>
  select(name, title, source, available_tables, frequency) |>
  kable() |>
  kable_styling(bootstrap_options = "striped") |>
  scroll_box(width = "100%", height = "400px")

## -----------------------------------------------------------------------------
# info <- get_dataset_info("abecip")
# names(info$categories)
# #> [1] "sbpe"  "units"  "cgi"

## -----------------------------------------------------------------------------
# get_dataset("abecip", source = "cache") # local cache (instant, works offline)
# get_dataset("abecip", source = "github") # GitHub release
# get_dataset("abecip", source = "fresh") # direct from the original source

## -----------------------------------------------------------------------------
sbpe <- get_dataset("abecip", table = "sbpe")

glimpse(sbpe)

## -----------------------------------------------------------------------------
# Annual net credit flow
sbpe_annual <- sbpe |>
  filter(date >= as.Date("2019-01-01")) |>
  mutate(year = lubridate::year(date)) |>
  summarise(net_flow = sum(sbpe_netflow, na.rm = TRUE) / 1e3, .by = year) |>
  mutate(
    label_num = format(round(net_flow, 1)),
    ypos = if_else(net_flow > 0, net_flow + 10, net_flow - 10)
  )

ggplot(sbpe_annual, aes(year, net_flow)) +
  geom_col(fill = color_palette[1], alpha = 0.9, width = 0.8) +
  geom_text(aes(y = ypos, label = label_num), size = 3) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 2019:2026) +
  labs(
    title = "Annual Net Savings Flow (SBPE)",
    x = NULL,
    y = "R$ billions"
  ) +
  theme_series()

## -----------------------------------------------------------------------------
units <- get_dataset("abecip", table = "units")

glimpse(units)

## -----------------------------------------------------------------------------
# SBPE units financed per year
units_recent <- units |>
  filter(date >= as.Date("2019-01-01"))

ggplot(units_recent, aes(date, units_total)) +
  geom_point(alpha = 0.5, size = 0.8, color = color_palette[1]) +
  geom_smooth(
    color = color_palette[1],
    lwd = 0.7,
    se = FALSE,
    method = stats::loess,
    method.args = list(span = 0.4)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Monthly Financed Units",
    y = "Units"
  ) +
  theme_series()

## -----------------------------------------------------------------------------
bcb <- get_dataset("bcb_realestate")

# Get a specific series
sfh_pf <- bcb |>
  filter(series_info == "credito_estoque_carteira_credito_pf_sfh_br")

# Get the all the related series for 'estoque_carteira_credito_pf'
credit_stock <- bcb |>
  filter(
    category == "credito",
    type == "estoque",
    v1 == "carteira",
    v2 == "credito",
    v3 == "pf",
    # since v4 is left blank, we get all credit lines
    v5 == "br"
  )

# The helper columns essentially separate the 'series_info' column allowing
# for easier filtering. It's equivalent to filtering by regex
credit_stock <- bcb |>
  filter(grepl(
    "(?<=credito_estoque_carteira_credito_pf_).+_br$",
    series_info,
    perl = TRUE
  ))

## -----------------------------------------------------------------------------
ggplot(sfh_pf, aes(date, value / 1e9)) +
  geom_line(lwd = 0.7, color = color_palette[1]) +
  labs(title = "SFH", y = "R$ (billions)") +
  theme_series()

## -----------------------------------------------------------------------------
credit_labels <- c(
  "Home Equity" = "home-equity",
  "Comercial" = "comercial",
  "Livre" = "livre",
  "FGTS" = "fgts",
  "SFH" = "sfh"
)

credit_stock <- credit_stock |>
  mutate(
    credit_line_label = factor(
      v4,
      levels = credit_labels,
      labels = names(credit_labels)
    )
  )

ggplot(credit_stock, aes(date, value / 1e9)) +
  geom_area(aes(fill = credit_line_label), alpha = 0.9) +
  scale_fill_manual(values = rev(color_palette[1:5])) +
  scale_x_date(expand = expansion(mult = c(0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Real Estate Credit Stock",
    subtitle = "Household real estate credit stock (total debt) by credit line",
    y = "R$ (billions)",
    fill = NULL
  ) +
  theme_series()

