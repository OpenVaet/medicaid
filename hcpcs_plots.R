# hcpcs_plots.R
# Input file: data/hcpcs_by_months_with_desc.csv
# Output:
#  - outputs/avg_yearly_by_code.csv
#  - outputs/no_desc/*.png
#  - outputs/with_desc/*.png
#  - outputs/summary_top50_spendings.png
#
# Notes:
# - total_unique_beneficiaries is monthly; summing it gives "beneficiary-months"
#   (it may overcount people across months). This script uses that measure consistently.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(lubridate)
  library(scales)
})

# ----------------------------
# Config
# ----------------------------
infile <- "data/hcpcs_by_months_with_desc.csv"
out_dir_no_desc   <- file.path("outputs", "no_desc")
out_dir_with_desc <- file.path("outputs", "with_desc")
out_dir           <- "outputs"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_no_desc, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_with_desc, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# Load + basic cleaning
# ----------------------------
df <- read_csv(infile, show_col_types = FALSE) %>%
  mutate(
    hcpcs_code = as.character(hcpcs_code),
    hcpcs_desc = ifelse(is.na(hcpcs_desc) | hcpcs_desc == "", "not_found", hcpcs_desc),
    year = as.integer(year),
    month = as.integer(month),
    ym = make_date(year, month, 1),
    total_paid = as.numeric(total_paid),
    total_unique_beneficiaries = as.numeric(total_unique_beneficiaries),
    total_claims = as.numeric(total_claims)
  )

# ----------------------------
# 1) Average yearly spendings & total claimants on each HCPCS code
# ----------------------------
yearly_by_code <- df %>%
  group_by(hcpcs_code, hcpcs_desc, hcpcs_source, year) %>%
  summarise(
    yearly_paid = sum(total_paid, na.rm = TRUE),
    yearly_beneficiary_months = sum(total_unique_beneficiaries, na.rm = TRUE),
    yearly_claims = sum(total_claims, na.rm = TRUE),
    .groups = "drop"
  )

avg_yearly_by_code <- yearly_by_code %>%
  group_by(hcpcs_code, hcpcs_desc, hcpcs_source) %>%
  summarise(
    years_covered = n_distinct(year),
    avg_yearly_paid = mean(yearly_paid, na.rm = TRUE),
    avg_yearly_beneficiary_months = mean(yearly_beneficiary_months, na.rm = TRUE),
    avg_yearly_claims = mean(yearly_claims, na.rm = TRUE),
    total_paid_all_years = sum(yearly_paid, na.rm = TRUE),
    total_beneficiary_months_all_years = sum(yearly_beneficiary_months, na.rm = TRUE),
    total_claims_all_years = sum(yearly_claims, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_paid_all_years))

write_csv(avg_yearly_by_code, file.path(out_dir, "avg_yearly_by_code.csv"))

# ----------------------------
# Helpers
# ----------------------------
safe_filename <- function(x) {
  x %>%
    str_replace_all("[/\\\\]", "-") %>%
    str_replace_all("[^A-Za-z0-9 _\\-\\.]", "") %>%
    str_squish() %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 180)
}

plot_code_monthly <- function(d, code, desc, out_path) {
  # d must contain: ym, total_unique_beneficiaries, total_paid
  d <- d %>% arrange(ym)
  
  # Scale expenses onto the beneficiaries axis for dual-axis display
  max_b <- max(d$total_unique_beneficiaries, na.rm = TRUE)
  max_p <- max(d$total_paid, na.rm = TRUE)
  scale_factor <- ifelse(is.finite(max_p) && max_p > 0, max_b / max_p, 1)
  
  title_txt <- ifelse(desc == "not_found",
                      paste0("HCPCS: ", code),
                      paste0("HCPCS: ", code, " — ", desc))
  
  p <- ggplot(d, aes(x = ym)) +
    geom_col(aes(y = total_unique_beneficiaries), alpha = 0.6) +
    geom_line(aes(y = total_paid * scale_factor), linewidth = 0.8) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(
      name = "Total recipients (unique beneficiaries per month)",
      labels = comma,
      sec.axis = sec_axis(~ . / scale_factor, name = "Total paid", labels = dollar_format(accuracy = 1))
    ) +
    labs(
      title = title_txt,
      x = "Year-Month"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(out_path, plot = p, width = 12, height = 6, dpi = 150)
}

# Monthly totals for plotting (already monthly, but ensure grouped properly)
monthly_by_code <- df %>%
  group_by(hcpcs_code, hcpcs_desc, ym) %>%
  summarise(
    total_unique_beneficiaries = sum(total_unique_beneficiaries, na.rm = TRUE),
    total_paid = sum(total_paid, na.rm = TRUE),
    .groups = "drop"
  )

total_spend_by_code <- monthly_by_code %>%
  group_by(hcpcs_code, hcpcs_desc) %>%
  summarise(
    total_paid = sum(total_paid, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 2) Top 50 (by spendings) codes with hcpcs_desc == "not_found"
#    Plot monthly recipients (bars) + expenses (line) into outputs/no_desc
# ----------------------------
top50_no_desc <- total_spend_by_code %>%
  filter(hcpcs_desc == "not_found") %>%
  arrange(desc(total_paid)) %>%
  slice_head(n = 50)

for (i in seq_len(nrow(top50_no_desc))) {
  code <- top50_no_desc$hcpcs_code[i]
  d <- monthly_by_code %>% filter(hcpcs_code == code, hcpcs_desc == "not_found")
  out_file <- file.path(out_dir_no_desc, paste0(sprintf("%02d", i), "_", safe_filename(code), ".png"))
  plot_code_monthly(d, code, "not_found", out_file)
}

# ----------------------------
# 3) Top 150 codes with description != "not_found"
#    Same plots, include description in title, into outputs/with_desc
# ----------------------------
top150_with_desc <- total_spend_by_code %>%
  filter(hcpcs_desc != "not_found") %>%
  arrange(desc(total_paid)) %>%
  slice_head(n = 150)

for (i in seq_len(nrow(top150_with_desc))) {
  code <- top150_with_desc$hcpcs_code[i]
  desc <- top150_with_desc$hcpcs_desc[i]
  d <- monthly_by_code %>% filter(hcpcs_code == code, hcpcs_desc == desc)
  out_file <- file.path(
    out_dir_with_desc,
    paste0(sprintf("%03d", i), "_", safe_filename(paste0(code, "_", desc)), ".png")
  )
  plot_code_monthly(d, code, desc, out_file)
}

# ----------------------------
# 4) Summary plot: top 50 codes overall as line chart
#    - No ggplot legend (too big)
#    - Numbers at end of lines
#    - Right-side legend panel with "n. code — description"
# ----------------------------

suppressPackageStartupMessages({
  # We'll try patchwork first, then fall back to cowplot/gridExtra
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  has_cowplot   <- requireNamespace("cowplot", quietly = TRUE)
  has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)
  library(grid)
})

top50_overall <- total_spend_by_code %>%
  arrange(desc(total_paid)) %>%
  slice_head(n = 50) %>%
  mutate(rank = row_number())

summary_data <- monthly_by_code %>%
  inner_join(top50_overall %>% select(hcpcs_code, hcpcs_desc, rank),
             by = c("hcpcs_code", "hcpcs_desc")) %>%
  mutate(
    # Shorten very long descriptions for legend readability
    hcpcs_desc_short = ifelse(nchar(hcpcs_desc) > 70,
                              paste0(substr(hcpcs_desc, 1, 67), "..."),
                              hcpcs_desc),
    label = paste0(rank, ". ", hcpcs_code, " — ", hcpcs_desc_short)
  )

# last point per series for placing the numeric label
last_pts <- summary_data %>%
  group_by(rank, hcpcs_code, hcpcs_desc, label) %>%
  filter(ym == max(ym, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Main plot: lines + numeric labels (NO legend)
p_main <- ggplot(summary_data, aes(x = ym, y = total_paid, group = label, color = label)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_text_repel(
    data = last_pts,
    aes(label = rank),
    direction = "y",
    nudge_x = 30,              # push labels to the right
    box.padding = 0.25,
    point.padding = 0.10,
    min.segment.length = 0,
    segment.size = 0.2,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.01, 0.25))
  ) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Top 50 HCPCS codes by total spendings — monthly total paid",
    x = "Year-Month",
    y = "Total paid"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Legend panel as a simple text plot on the right
legend_df <- summary_data %>%
  distinct(rank, hcpcs_code, hcpcs_desc_short, label) %>%
  arrange(rank) %>%
  mutate(y = rev(row_number()))

p_legend <- ggplot(legend_df, aes(x = 0, y = y)) +
  geom_text(aes(label = label), hjust = 0, size = 3) +
  xlim(0, 1) +
  labs(title = "Code & description") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine main plot + legend panel into one image
out_path <- file.path(out_dir, "summary_top50_spendings.png")

if (has_patchwork) {
  library(patchwork)
  p_combined <- p_main + p_legend + plot_layout(widths = c(3.8, 2.2))
  ggsave(out_path, plot = p_combined, width = 18, height = 9, dpi = 150)
  
} else if (has_cowplot) {
  library(cowplot)
  p_combined <- cowplot::plot_grid(p_main, p_legend, nrow = 1, rel_widths = c(3.8, 2.2))
  ggsave(out_path, plot = p_combined, width = 18, height = 9, dpi = 150)
  
} else if (has_gridExtra) {
  library(gridExtra)
  g <- gridExtra::arrangeGrob(p_main, p_legend, nrow = 1, widths = c(3.8, 2.2))
  ggsave(out_path, plot = g, width = 18, height = 9, dpi = 150)
  
} else {
  # Last resort: save main and legend separately
  warning("patchwork/cowplot/gridExtra not available. Saving separate files.")
  ggsave(out_path, plot = p_main, width = 18, height = 9, dpi = 150)
  ggsave(file.path(out_dir, "summary_top50_spendings_legend.png"),
         plot = p_legend, width = 10, height = 14, dpi = 150)
}

# ----------------------------
# 4b) Summary plot: top 50 codes by total_unique_beneficiaries (beneficiary-months)
#     - Lines = monthly total_unique_beneficiaries
#     - Numbers at end of lines (repelled)
#     - Right-side legend panel with "n. code — description"
# ----------------------------

top50_benes_overall <- monthly_by_code %>%
  group_by(hcpcs_code, hcpcs_desc) %>%
  summarise(total_benes = sum(total_unique_beneficiaries, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_benes)) %>%
  slice_head(n = 50) %>%
  mutate(rank = row_number())

summary_benes <- monthly_by_code %>%
  inner_join(top50_benes_overall %>% select(hcpcs_code, hcpcs_desc, rank),
             by = c("hcpcs_code", "hcpcs_desc")) %>%
  mutate(
    hcpcs_desc_short = ifelse(nchar(hcpcs_desc) > 70,
                             paste0(substr(hcpcs_desc, 1, 67), "..."),
                             hcpcs_desc),
    label = paste0(rank, ". ", hcpcs_code, " — ", hcpcs_desc_short)
  )

# label points: use LAST month (as you do for spendings)
last_pts_b <- summary_benes %>%
  group_by(rank, hcpcs_code, hcpcs_desc, label) %>%
  filter(ym == max(ym, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup()

p_main_b <- ggplot(summary_benes,
                   aes(x = ym, y = total_unique_beneficiaries, group = label, color = label)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_text_repel(
    data = last_pts_b,
    aes(label = rank),
    direction = "y",
    nudge_x = 30,
    box.padding = 0.25,
    point.padding = 0.10,
    min.segment.length = 0,
    segment.size = 0.2,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.01, 0.25))
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 50 HCPCS codes by total recipients — monthly total recipients",
    subtitle = "Recipients = sum of monthly unique beneficiaries (beneficiary-months)",
    x = "Year-Month",
    y = "Total recipients",
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

legend_df_b <- summary_benes %>%
  distinct(rank, hcpcs_code, hcpcs_desc_short, label) %>%
  arrange(rank) %>%
  mutate(y = rev(row_number()))

p_legend_b <- ggplot(legend_df_b, aes(x = 0, y = y)) +
  geom_text(aes(label = label), hjust = 0, size = 3) +
  xlim(0, 1) +
  labs(title = "Code & description") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )

out_path_b <- file.path(out_dir, "summary_top50_beneficiaries.png")

if (has_patchwork) {
  library(patchwork)
  p_combined_b <- p_main_b + p_legend_b + plot_layout(widths = c(3.8, 2.2))
  ggsave(out_path_b, plot = p_combined_b, width = 18, height = 9, dpi = 150)

} else if (has_cowplot) {
  library(cowplot)
  p_combined_b <- cowplot::plot_grid(p_main_b, p_legend_b, nrow = 1, rel_widths = c(3.8, 2.2))
  ggsave(out_path_b, plot = p_combined_b, width = 18, height = 9, dpi = 150)

} else if (has_gridExtra) {
  library(gridExtra)
  g_b <- gridExtra::arrangeGrob(p_main_b, p_legend_b, nrow = 1, widths = c(3.8, 2.2))
  ggsave(out_path_b, plot = g_b, width = 18, height = 9, dpi = 150)

} else {
  warning("patchwork/cowplot/gridExtra not available. Saving separate files.")
  ggsave(out_path_b, plot = p_main_b, width = 18, height = 9, dpi = 150)
  ggsave(file.path(out_dir, "summary_top50_beneficiaries_legend.png"),
         plot = p_legend_b, width = 10, height = 14, dpi = 150)
}
