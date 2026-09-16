############## Packages ################
# load packages
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here('01_code/functions'),
      full.names = TRUE,
      recursive  = TRUE),
    source))


# Displaying results
## Further sim results of the appendix ----
load(here::here('03_sim_eval', 'n_1000', 'n.1000_co.0.75_uncorr_ablation.RData'))

# plot rules
# data frame for plotting
plot_data_cases <- tidyr::expand_grid(
  model  = c("sbcf_iv", "bcf_iv", "bcf_iv_with_cost", "grf_iv", "grf_iv_with_cost", "sbcf_iv_without_cost"),
  ncov   = c(10, 50, 100),
  effect = as.character(seq(0, 2, 0.2))
)

# analyze performance on rules ----
plot_rule_data <- plot_data_cases %>%
  # add results
  dplyr::left_join(
    rules_metric,
    by = c("model", "ncov", "effect")
  ) %>%
  dplyr::mutate(across(everything(), ~ replace_na(., 0))) %>%
  dplyr::mutate(
    ncov = factor(paste0("$\\bm{P = ", ncov, "}$"),
                  levels = c("$\\bm{P = 10}$", "$\\bm{P = 50}$", "$\\bm{P = 100}$")),
    # original labels kept intact
    model = case_when(
      model == "sbcf_iv"              ~ "SBCF-IV (with cost)",
      model == "bcf_iv"               ~ "BCF-IV (without cost)",
      model == "bcf_iv_with_cost"     ~ "BCF-IV (with cost)",
      model == "grf_iv"               ~ "GRF-IV (without cost)",
      model == "grf_iv_with_cost"     ~ "GRF-IV (with cost)",
      model == "sbcf_iv_without_cost" ~ "SBCF-IV (without cost)"
    )
  ) %>%
  dplyr::mutate(effect = as.numeric(effect))

##### Figure DR, FDR #### 

p <- plot_rule_data %>%
  dplyr::select(model, ncov, effect, DR, FDR, model) %>%
  tidyr::pivot_longer(
    cols      = c(DR),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  ggplot2::ggplot(aes(x = effect, y = value,
                      color = model, linetype = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 2, size = 0.8) +
  facet_grid(metric ~ ncov) +
  labs(x = "Effect Size ($k$)", y = " ",
       color = "Method", linetype = "Method") +
  scale_color_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "#969799", "BCF-IV (with cost)"     = "#969799",
    "SBCF-IV (without cost)" = "#af8209", "SBCF-IV (with cost)"    = "#af8209",
    "GRF-IV (without cost)"  = "#762A83", "GRF-IV (with cost)"     = "#762A83")) +
  scale_linetype_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "solid", "BCF-IV (with cost)"     = "dotdash",
    "SBCF-IV (without cost)" = "solid", "SBCF-IV (with cost)"    = "dotdash",
    "GRF-IV (without cost)"  = "solid", "GRF-IV (with cost)"     = "dotdash")) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 2, .4)) +
  theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "#004c93", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing    = unit(4, "pt"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "plain")
  )

# show figure
print(p)


#
plot_ind_clas_data <- plot_data_cases %>%
  dplyr::left_join(
    ind_clf_metrics,
    by = c('effect', 'ncov', 'model')
  ) %>%
  dplyr::select(-ends_with("_sd")) %>%
  dplyr::rename_with(~ gsub("_mean", "", .x, fixed = TRUE), ends_with("_mean")) %>%
  dplyr::mutate(across(c("Recall", "Precision", "F_score", "TPR",
                         "FNR", "FPR", "TNR"), ~ replace_na(., 0))) %>%
  dplyr::mutate(
    ncov = factor(paste0('$\\bm{P = ', ncov, '}$'),
                  levels = c('$\\bm{P = 10}$', '$\\bm{P = 50}$', '$\\bm{P = 100}$')),
    # original labels kept intact
    model = case_when(
      model == "sbcf_iv"              ~ "SBCF-IV (with cost)",
      model == "bcf_iv"               ~ "BCF-IV (without cost)",
      model == "bcf_iv_with_cost"     ~ "BCF-IV (with cost)",
      model == "grf_iv"               ~ "GRF-IV (without cost)",
      model == "grf_iv_with_cost"     ~ "GRF-IV (with cost)",
      model == "sbcf_iv_without_cost" ~ "SBCF-IV (without cost)"
    )
  ) %>%
  dplyr::mutate(effect = as.numeric(effect))

#### figure f1, precision ####

p <- plot_ind_clas_data %>%
  dplyr::select(model, ncov, effect, Recall, Precision, F_score, FPR) %>%
  dplyr::rename("$F$-Score" = F_score) %>%
  tidyr::pivot_longer(
    cols      = c(Precision, `$F$-Score`),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  ggplot2::ggplot(aes(x = effect, y = value,
                      color = model, linetype = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(shape = 2, size = 0.8) +
  facet_grid(metric ~ ncov) +
  labs(x = "Effect Size ($k$)", y = " ",
       color = "Method", linetype = "Method") +
  scale_color_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "#969799", "BCF-IV (with cost)"     = "#969799",
    "SBCF-IV (without cost)" = "#af8209", "SBCF-IV (with cost)"    = "#af8209",
    "GRF-IV (without cost)"  = "#762A83", "GRF-IV (with cost)"     = "#762A83")) +
  scale_linetype_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "solid", "BCF-IV (with cost)"     = "dotdash",
    "SBCF-IV (without cost)" = "solid", "SBCF-IV (with cost)"    = "dotdash",
    "GRF-IV (without cost)"  = "solid", "GRF-IV (with cost)"     = "dotdash")) +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 2, .4)) +
  theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "#004c93", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing    = unit(4, "pt"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "plain")
  )


# show figure
print(p)

##### Figure MSE, Coverage ####

est_long <- subgroup_metrics %>%                       # numeric pre-pivot object
  select(ncov, effect, model, PEHE_mean, coverage_mean) %>%
  pivot_longer(ends_with("_mean"), names_to = "metric",
               values_to = "value", names_pattern = "(.*)_mean") %>%
  dplyr::mutate(
  ncov = factor(paste0('$\\bm{P = ', ncov, '}$'),
                levels = c('$\\bm{P = 10}$', '$\\bm{P = 50}$', '$\\bm{P = 100}$')),
  # original labels kept intact
  model = case_when(
      model == "sbcf_iv"              ~ "SBCF-IV (with cost)",
      model == "bcf_iv"               ~ "BCF-IV (without cost)",
      model == "bcf_iv_with_cost"     ~ "BCF-IV (with cost)",
      model == "grf_iv"               ~ "GRF-IV (without cost)",
      model == "grf_iv_with_cost"     ~ "GRF-IV (with cost)",
      model == "sbcf_iv_without_cost" ~ "SBCF-IV (without cost)"
    ), 
  effect = as.numeric(effect),
  metric = factor(recode(metric, PEHE = "MSE", coverage = "Coverage"),
                  levels = c("MSE", "Coverage"))) 

p <- ggplot2::ggplot(est_long, aes(x = effect, y = value,
                         color = model, linetype = model)) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    data = ~subset(., metric == "Coverage"),
    aes(yintercept = 0.95),
    colour = "red", linetype = "dashed", linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(shape = 2, size = 0.8) +
  facet_grid(metric ~ ncov, scales="free_y") +
  labs(x = "Effect Size ($k$)", y = " ",
       color = "Method", linetype = "Method") +
  scale_color_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "#969799", "BCF-IV (with cost)"     = "#969799",
    "SBCF-IV (without cost)" = "#af8209", "SBCF-IV (with cost)"    = "#af8209",
    "GRF-IV (without cost)"  = "#762A83", "GRF-IV (with cost)"     = "#762A83")) +
  scale_linetype_manual(name = "Method", values = c(
    "BCF-IV (without cost)"  = "solid", "BCF-IV (with cost)"     = "dotdash",
    "SBCF-IV (without cost)" = "solid", "SBCF-IV (with cost)"    = "dotdash",
    "GRF-IV (without cost)"  = "solid", "GRF-IV (with cost)"     = "dotdash")) +
  scale_x_continuous(breaks = seq(0, 2, .4)) +
  theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(fill = "#004c93", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing    = unit(4, "pt"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "plain")
  )


# show figure
print(p)

# Appendix Tables
metrics_tables <- subgroup_metrics %>%
  pivot_longer(
    cols = matches("_(mean|sd)$"),
    names_to = c("metric", "stat"),
    names_pattern = "^(.*)_(mean|sd)$",
    values_to = "val"
  ) %>%
  pivot_wider(names_from = stat, values_from = val) %>%
  mutate(cell = sprintf("%.3f (%.3f)", mean, sd)) %>%
  select(-mean, -sd) %>%
  pivot_wider(names_from = metric, values_from = cell) %>%
  tidyr::pivot_wider(
    id_cols = c(ncov, effect),
    names_from = model,
    values_from = c(PEHE, bias, abs_bias, coverage, conf_width),
    names_glue = "{model}.{.value}"
  ) %>%
  # sorting
  dplyr::select(ncov, effect,
                starts_with("bcf_iv"),
                starts_with("sbcf_iv"),
                starts_with("grf_iv")) %>%
  dplyr::arrange(ncov, effect)

write_csv(x = metrics_tables, file = here::here('03_sim_eval', 'n_1000', 'metrics_tables.csv'))

