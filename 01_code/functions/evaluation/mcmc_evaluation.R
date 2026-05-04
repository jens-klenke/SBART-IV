# summarise wrapper to clean memory after the analysis
MCMC_summarise <- function(path, case, compliance, n_case = "n_1000", M = 500, ...) {
  
  # Function parameters
  # case - corr or uncorr
  # compliance - compliance rate; example 'co.0.75'
  # n_case - number of ind. data points per MCMC run with; example 'n_1000'
  # M - number of MCMCM runs
  
  # Suppress summarise info in dplyr
  options(dplyr.summarise.inform = FALSE)
  
  # calling the summarise function itself
  summarise_case_data(path, case, compliance, n_case, M = M)
  
  # clean up memory
  invisible(gc(verbose = FALSE))
}

# summarize per case
summarise_case_data <- function(path, case, compliance, n_case = "n_1000", M = 500, ...) {
  
  # Function parameters
  # case - corr or uncorr
  # compliance - compliance rate; example 'co.0.75'
  # n_case - number of ind. data points per MCMC run with; example 'n_1000'
  # M - number of MCMCM runs
  
  
  # Variables
  case <- case
  compliance_number <- as.numeric(sub("co.", "", compliance))
  n <- as.numeric(sub("n_", "", n_case))
  M <- M
  
  # load data files -----
  # path to sim_reuslts
  sim_result_path <- here::here(path, case, compliance)
  sim_result_path <- here::here(path) # How to store results hier? 
  
  if(verbose){
    cat(
    '\n\n Setting / Analysis of: \n
          Data path:', crayon::yellow(sim_result_path), '\n',
    '\t\t Case:', crayon::blue(case), '\n',
    '\t\t Compliance:', crayon::blue(compliance_number), '\n',
    '\t\t Observations per MCMC run:', crayon::blue(n), '\n\n\n')
  }
  
  # get files
  results_path <- tibble::tibble(
    # path for loading data
    path_in = list.files(sim_result_path,  full.names = TRUE, pattern = '*.RData', recursive = TRUE)
  ) %>%
    dplyr::mutate(effect = stringr::str_extract(path_in,  paste0("(?<=ef.).*(?=_co\\.)")))
  
  if (NROW(results_path) == 0) {
    stop(glue::glue("❌ the target folder is empty. \n expecting MCMC data at: {crayon::yellow(sim_result_path)}"))
  }
  # load data of the files 
  data <- results_path %>%
    dplyr::rowwise() %>%
    dplyr::mutate(data = list({
      e <- new.env()
      obj_name <- load(path_in, envir = e)
      e[[obj_name]]
    })) %>%
    dplyr::select(-path_in)
  
  # unnest data for each effect size (NROW = 11 effects * 500 runs * 3 ncov) ----
  data %<>%
    tidyr::unnest(data) %>%
    #    dplyr::select(-path_in) %>%
    dplyr::mutate(dataset_num = as.numeric(str_extract(row_num, "^[^ of]+")))
  
  # wrangling data and unnest results 500 MCMCs * 2 Models * 11 Effects * 3 ncov
  data_long <- data %>%
    # important variables
    dplyr::select(path_in, effect, ncov, dataset_num, sbcf_iv, bcf_iv) %>%
    # stacking models 
    tidyr::pivot_longer(c(sbcf_iv, bcf_iv), names_to = 'model', values_to = 'results')
  
  ## unnest results
  data_long %<>%
    dplyr::select(-path_in) %>%
    tidyr::unnest(results)
  
  # analyze performance on rules ----
  
  # getting ruled data
  rules_metric <- data_long %>%
    dplyr::select(effect,  ncov, model, dataset_num, rule_results) %>%
    tidyr::unnest(rule_results) %>%
    dplyr::group_by(model, ncov, effect) %>%
    dplyr::summarise(
      # Discovery Rate
      DR = sum(rule_det)/(2*M),
      # Discovery Rate l1
      DR_l1 = sum(rule_det_l1)/M,
      # Discovery Rate l2
      DR_l2 = sum(rule_det_l2)/M,
      # False Discovery Rate
      FDR = sum(rule_false_det)/M
    )
  
  # subgroup metrics ----
  data_ind <- data_long %>%
    dplyr::select(effect,  ncov, model, dataset_num, individual_results) %>%
    tidyr::unnest(individual_results)
  
  # Individual classification metrics 
  ind_clf_metrics <- data_ind %>%
    # summary for each MCMC run
    dplyr::group_by(model, ncov, effect, dataset_num) %>%
    dplyr::summarise(
      TP = sum(TP),
      FN = sum(FN),
      FP = sum(FP),
      TN = sum(TN),
      TPR = sum(TP) / sum(c(TP, FN)),
      FNR = sum(FN) / sum(c(TP, FN)),
      FPR = sum(FP) / sum(c(FP, TN)),
      TNR = sum(TN) / sum(c(FP, TN)),
      Recall = TP / (TP + TN),
      Precision = TP / (TP + FP),
      # harmnoic mean of precision and recall
      F_score = TP / (TP + 0.5 * (FP + FN))
    ) %>%
    dplyr::mutate(
      Precision = ifelse(is.nan(Precision), 0, Precision), 
      Recall = ifelse(is.nan(Recall), NA, Recall)
    ) %>%
    # summary for each effect/ ncov case 
    dplyr::group_by(model, ncov, effect) %>%
    dplyr::summarise(
      dplyr::across(
        c(Recall, Precision, F_score, FNR, FPR, TNR, TPR),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # score functions ----
  subgroup_data_metrics <- data_long %>%
    dplyr::select(effect,  ncov, model, dataset_num, subgroup_data_metrics) %>%
    tidyr::unnest(subgroup_data_metrics)
  
  # Metric
  subgroup_metrics <- subgroup_data_metrics %>%
    dplyr::group_by(model, ncov, effect) %>%
    dplyr::summarise(
      dplyr::across(
        c(PEHE, bias, abs_bias, coverage, conf_width),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # separated for each subgroup ----
  subgroup_sep_data <- data_long %>%
    dplyr::select(effect,  ncov, model, dataset_num, subgroup_sep_data) %>%
    tidyr::unnest(subgroup_sep_data)
  
  # Metric
  subgroup_sep_metrics <- subgroup_sep_data %>%
    dplyr::group_by(real_subgroup, model, ncov, effect) %>%
    dplyr::summarise(
      dplyr::across(
        c(PEHE, bias, abs_bias, coverage, conf_width),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # saving data
  save(rules_metric, ind_clf_metrics, subgroup_metrics, subgroup_sep_metrics,
       file = here::here(glue::glue('03_sim_eval/{n_case}_{compliance}_{case}.RData')))
  
  if(verbose){
    # safe information
    cat(crayon::silver(' ----------------------------------------------------------------------------------'),
        '\n\t\t\t\t', crayon::green('*** Finished ***'), '\n',
        crayon::silver('---------------------------------------------------------------------------------- \n')
    )
  }
}

# make pipe visible to linters / R CMD check
`%>%` <- magrittr::`%>%`
