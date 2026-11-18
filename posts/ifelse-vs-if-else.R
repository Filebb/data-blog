# ─────────────────────────────────────────────────────────────
# 🧠 Comparison: ifelse() vs if_else()
# ─────────────────────────────────────────────────────────────
# Feature          | ifelse() (base R)                     | if_else() (dplyr)
# -----------------|---------------------------------------|-----------------------------------
# Type strictness  | Loosely typed; coerces types silently | Strictly typed; error on mismatch
# Date handling    | Converts Date objects to numeric      | Preserves Date class
# NA handling      | No dedicated missing argument         | Has `missing` argument for NA
# Performance      | Slightly slower on large vectors      | Faster and more predictable
# Error visibility | May silently introduce bugs           | Fails loudly, aiding debugging
# ─────────────────────────────────────────────────────────────





# TYPE COERCION ----------------------------------------------------------------

# Base R: silently coerces types
ifelse(c(TRUE, FALSE), "yes", 0)
# Output: "yes" "0"  ← character vector

# dplyr: throws error due to type mismatch
dplyr::if_else(c(TRUE, FALSE), "yes", 0)
# Error: `false` must be a character vector, not a double





# DATE PRESERVATION  -----------------------------------------------------------

dates <- as.Date(c("2022-01-01", "2022-01-02"))
# Base R: converts to numeric
ifelse(dates == "2022-01-01", dates + 1, dates)
# Output: numeric vector

# dplyr: preserves Date class
dplyr::if_else(dates == "2022-01-01", dates + 1, dates)
# Output: Date vector





# HANDLING NA   ----------------------------------------------------------------
vals <- c(80, 90, NA, 110)
dplyr::if_else(vals < 100, 1, 2, missing = 9)
# Output: 1 1 9 2


library(tidyverse)
library(rlang)

get_fun_name_rlang <- function(fun) {
  as_string(enexpr(fun))
}

run <- function(data, fun) {
  fun(data < 0, -1, 1)
}


# n <- 1e4
# data <- rnorm(n)
# bench::mark(ifelse(data > 0, 1, -1),
#             if_else(data > 0, 1, -1)) |>
#   select(median, mem_alloc) |>
#   mutate(across(everything(), as.double)) |>
#   unlist()

comparison <- tibble(
  n = c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7),
  data = map(n, rnorm),
  performance = map(data,
                   \(data) bench::mark(ifelse(data > 0, 1, -1),
                                       if_else(data > 0, 1, -1)) |>
                     select(median, mem_alloc) |>
                     mutate(across(everything(), as.double)))
) |>
  unnest_wider(performance)  |> 
  mutate(
    median = map(median, \(vec) set_names(vec, c("ifelse", "if_else"))),
    mem_alloc = map(mem_alloc, \(vec) set_names(vec, c("ifelse", "if_else")))
    ) |>
  unnest_wider(median, names_sep = "_") |> 
  unnest_wider(mem_alloc, names_sep = "_") |> 
  mutate(
    median_perc = (median_if_else / median_ifelse - 1) * 100,
    mem_alloc_perc = (mem_alloc_if_else / mem_alloc_ifelse - 1) * 100,
  )

comparison

comparison |>
  select(-data) |>
  view()

library(scales)

signed_log_trans <- trans_new(
  name = "signed_log10",
  transform = function(x) sign(x) * log10(abs(x) + 1),
  inverse = function(x) sign(x) * (10^abs(x) - 1),
  domain = c(-Inf, Inf)
)

comparison |>
  mutate(n = factor(n)) |> 
  ggplot() +
  aes(x = n, y = median_perc) +
  geom_col() +
  scale_y_continuous(trans = signed_log_trans) 



get_fun_name <- function(fun) {
  deparse(substitute(fun))
}

# Usage
get_fun_name(mean)   # "mean"
get_fun_name(sd)     # "sd"


get_fun_name_rlang(mean)  # "mean"
get_fun_name_rlang(sd)    # "sd"

as.character(expr(mean))
