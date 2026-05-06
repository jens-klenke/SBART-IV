# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here('01_code/functions'),
      full.names = TRUE, 
      recursive = TRUE),
    source))


# running summarize function
MCMC_summarise("uncorr", 0.75)


## tables and plots


load(here::here('03_sim_eval/n_1000/n.1000_co.0.75_uncorr.RData'))


# plot rules
# data frame for plotting
plot_data_cases <- tidyr::expand_grid(
  model = c("sbcf_iv", "bcf_iv"),
  ncov  = c(10, 50, 100),
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
  dplyr::mutate(ncov = factor(paste0("$\\bm{P = ", ncov, "}$"),
                              levels = c("$\\bm{P = 10}$",
                                         "$\\bm{P = 50}$", "$\\bm{P = 100}$")),
                model = ifelse(model == "bcf_iv", "BCF-IV", "SBCF-IV")) %>%
  dplyr::mutate(effect = as.numeric(effect))

# Figure 4.1
plot_rule_data %>%
  dplyr::select(model, ncov, effect, DR, FDR) %>%
  tidyr::pivot_longer(
    cols = c(DR, FDR),
    names_to = "metric",
    values_to = "value"
  ) %>%
  ggplot2::ggplot(aes(x = effect, y = value,
                      color = model, shape = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.8)  +
  ylim(0, 1) +
  facet_grid(metric ~ ncov) +
  labs(x = "Effect Size ($k$)", y = " ",
       color = "Model", shape = "Model") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.4)) +
  scale_color_manual(values  = c('#969799', '#af8209')) +
  scale_shape_manual(values = c(2, 4)) +
  theme(legend.position = "bottom")

# Figure 4.2 
plot_ind_clas_data %>%
  ggplot2::ggplot(aes(x = effect, y = Precision, color = model, shape = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.8) +
  ylim(0, 1) +
  facet_wrap(~ncov) +
  labs(x = "Effect Size ($k$)", y = "Precision \n",
       color = "Model", shape = "Model") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.4)) +
  scale_color_manual(values  = c('#969799', '#af8209')) +
  scale_shape_manual(values = c(2, 4))

plot_ind_clas_data %>%
  dplyr::select(model, ncov, effect, Recall, Precision, F_score, FPR) %>%
  dplyr::rename("$F$-Score" = F_score) %>%
  tidyr::pivot_longer(
    cols = c(Precision, `$F$-Score`),
    names_to = "metric",
    values_to = "value"
  ) %>%
  ggplot2::ggplot(aes(x = effect, y = value,
                      color = model, shape = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.8)  +
  ylim(0, 1) +
  facet_grid(metric ~ ncov) +
  labs(x = "Effect Size ($k$)", y = " ",
       color = "Model", shape = "Model") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.4)) +
  scale_color_manual(values  = c('#969799', '#af8209')) +
  scale_shape_manual(values = c(2, 4)) +

  theme(legend.position = "bottom")


# Table 1 and Table 4 (Appendix)
subgroup_metrics %>%
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
                starts_with("sbcf_iv")) %>%
  dplyr::arrange(ncov, effect)
