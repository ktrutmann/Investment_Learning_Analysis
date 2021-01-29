# This script generates variabes that are later used for the analysis

library(tidyverse)
set.seed(9876)


# Constants ############################################################
n_blocks <- 4
data_path <- file.path('..', 'Data', 'Clean')

study_stage <- 'main_study'  # Name the tables should be saved under.
# e.g. 'test_x', 'pilot_x', 'main_study', 'param_recov'

dat_main_long <- read_delim(file.path(data_path,
  str_c('all_participants_long_main_', study_stage, '.csv')),
delim = ';')

n_subj <- length(unique(dat_main_long$participant_code))


##########################################################################
# Create output tables where the participants can be compared

de_table_vars <- c('pgr', 'plr', 'de', 'n_sales',
  'rational_pgr', 'rational_plr', 'rational_de', 'rational_n_sales')
de_table_vars <- de_table_vars %>%
  expand.grid(1:n_blocks) %>%
  apply(1, paste, collapse = '_')
de_table_vars <- c(de_table_vars, 'de_diff',
  'main_condition', 'session_code', 'participant_code',
  'pgr_12', 'pgr_34', 'plr_12', 'plr_34', 'pgr_diff_12_34',
  'plr_diff_12_34', 'de_diff_12_34',
  'rational_pgr_12', 'rational_pgr_34', 'rational_plr_12',
  'rational_plr_34', 'rational_pgr_diff_12_34',
  'rational_plr_diff_12_34', 'rational_de_diff_12_34')

de_table <- as_tibble(
  matrix(NA, ncol = length(de_table_vars),
    nrow = n_subj, dimnames = list(NULL, de_table_vars))) %>%
  mutate_all(~ as.numeric(.)) %>%
  mutate(across(contains(c('code', 'condition')), as.character))

rt_table_vars <- c('mean_RT', 'mean_lottery_RT')
rt_table_vars <- rt_table_vars %>%
  expand.grid(1:n_blocks) %>%
  apply(1, paste, collapse = '_')
rt_table_vars <- c(rt_table_vars, 'RT_diff')
rt_table <- as_tibble(matrix(NA, ncol = length(rt_table_vars),
                             nrow = n_subj,
                             dimnames = list(NULL, rt_table_vars))) %>%
  mutate_all(~ as.numeric(.))

outcome_table_vars <- c('avg_hold_length', 'avg_short_length',
  'rational_avg_hold_length', 'rational_avg_short_length',
  'n_holds', 'n_shorts', 'hit_rate', 'rational_hit_rate')
outcome_table_vars <- outcome_table_vars %>%
  expand.grid(1:n_blocks) %>%
  apply(1, paste, collapse = '_')
outcome_table_vars <- c(outcome_table_vars, 'ravens_matrices_score',
    'participant_payoff')
outcome_table <- as_tibble(matrix(NA, ncol = length(outcome_table_vars),
                             nrow = n_subj,
                             dimnames = list(NULL, outcome_table_vars))) %>%
  mutate_all(~ as.numeric(.))

demographics_vars <- c(
  'strategy', 'strategy_random', 'strategy_feeling', 'strategy_rational',
  'strategy_risk_averse', 'strategy_inertia', 'strategy_DE',
  'strategy_anti_DE', 'age', 'gender', 'is_student', 'study_field',
  'investment_experience', 'purpose', 'engagement', 'interest',
  'general_comments', 'soep_general', 'soep_drive', 'soep_finance',
  'soep_sport', 'soep_career', 'soep_health', 'soep_trust',
  'ambiguity_aversion', 'loss_aversion')

demographics_list <- list()


# Main loop over subjects ##############################################
all_subj_ids <- unique(dat_main_long$participant_code)
for (vpn in seq_along(all_files)) {
  dat <- filter(dat_main_long, participant_code == all_subj_ids[vpn])

# Demographics, Strategy and SOEP ---------------------------------------
  if (study_stage != 'param_recov') {
    demographics_list[[vpn]] <- dat %>%
    dplyr::slice(2) %>%
    dplyr::select(contains(demographics_vars))
  }


# DE Measure --------------------------------------------------------------
  de_list <- list()
  rational_de_list <- list()

  de_list$n_sales <- de_list$n_losses <- de_list$n_gains <-
    de_list$n_sold_losses <- de_list$n_sold_gains <-
    rational_de_list$n_sales <- rational_de_list$n_losses <-
    rational_de_list$n_gains <- rational_de_list$n_sold_losses <-
    rational_de_list$n_sold_gains <- rational_de_list$earnings <-
    vector(mode = 'numeric', length = n_blocks)

  for (block_nr in seq_len(n_blocks)) {
    block_dat <- filter(dat, i_block == block_nr - 1)

    # A "liquidation" is selling an asset while holding it or buying one while shorted.
    liqu <- with(block_dat,
      transaction < 0 & hold == 1 | transaction > 0 & hold == -1)
    de_list$n_sales[block_nr] <- sum(liqu)
    de_list$n_losses[block_nr] <- sum(block_dat$returns < 0)
    de_list$n_gains[block_nr] <- sum(block_dat$returns > 0)
    de_list$n_sold_losses[block_nr] <- block_dat %>% filter(returns < 0 & liqu) %>% nrow()
    de_list$n_sold_gains[block_nr] <- block_dat %>% filter(returns > 0 & liqu) %>% nrow()

    plr <- ifelse(de_list$n_losses[block_nr] == 0, 0,
                  de_list$n_sold_losses[block_nr] / de_list$n_losses[block_nr])
    pgr <- ifelse(de_list$n_gains[block_nr] == 0, 0,
                  de_list$n_sold_gains[block_nr] / de_list$n_gains[block_nr])

    de_measure <- pgr - plr

    de_table[vpn, str_c('n_sales_', block_nr)] <- de_list$n_sales[block_nr]
    de_table[vpn, str_c('pgr_', block_nr)] <- pgr
    de_table[vpn, str_c('plr_', block_nr)] <- plr
    de_table[vpn, str_c('de_', block_nr)] <- de_measure


# DE of a rational trader -------------------------------------------------

    # A "sale" in this measure is selling an asset while holding it or buying one while shorted.
    liqu <- block_dat$rational_trade != 0
    rational_de_list$n_sales[block_nr] <- sum(liqu, na.rm = TRUE)

    rational_de_list$n_losses[block_nr] <- sum(block_dat$rational_returns < 0)
    rational_de_list$n_gains[block_nr] <- sum(block_dat$rational_returns > 0)
    rational_de_list$n_sold_losses[block_nr] <- block_dat %>%
      dplyr::filter(rational_returns < 0 & liqu) %>%
      nrow()
    rational_de_list$n_sold_gains[block_nr] <- block_dat %>%
      dplyr::filter(rational_returns > 0 & liqu) %>%
      nrow()

    plr <- ifelse(rational_de_list$n_losses[block_nr] == 0, 0,
                  rational_de_list$n_sold_losses[block_nr] /
                  rational_de_list$n_losses[block_nr])
    pgr <- ifelse(rational_de_list$n_gains[block_nr] == 0, 0,
                  rational_de_list$n_sold_gains[block_nr] /
                  rational_de_list$n_gains[block_nr])

    de_measure <- pgr - plr

    de_table[vpn, str_c('rational_n_sales_', block_nr)] <- rational_de_list$n_sales[block_nr]
    de_table[vpn, str_c('rational_pgr_', block_nr)] <- pgr
    de_table[vpn, str_c('rational_plr_', block_nr)] <- plr
    de_table[vpn, str_c('rational_de_', block_nr)] <- de_measure


# Trading Indicators ------------------------------------------------------
    # How long do they hold/short on average?
    long_inv_streaks <- c()
    short_pos_streaks <- c()
    this_inv_count <- 0
    rational_long_inv_streaks <- c()
    rational_short_pos_streaks <- c()
    rational_this_inv_count <- 0
    for (i_time in seq(nrow(block_dat))) {
      # Participant:
      if (block_dat$hold[i_time] != 0) {
        this_inv_count <- this_inv_count + 1
      }
      if ((block_dat$transaction[i_time] != 0 |
          i_time == nrow(block_dat)) & this_inv_count != 0) {
        if (block_dat$hold[i_time] == 1) {
          long_inv_streaks <- c(long_inv_streaks, this_inv_count)
        } else if (block_dat$hold[i_time] == -1) {
          short_pos_streaks <- c(short_pos_streaks, this_inv_count)
        }
        this_inv_count <- 0
      }

        # Rational trader:
      if (is.na(block_dat$rational_hold[i_time])) {  # first period
        rational_this_inv_count <- rational_this_inv_count + 2 / 3
      } else {
        if (block_dat$rational_hold[i_time] != 0) {
          rational_this_inv_count <- rational_this_inv_count + 1
        }
        if ((block_dat$rational_trade[i_time] != 0 |
            i_time == nrow(block_dat)) & rational_this_inv_count != 0) {
          if (block_dat$rational_hold[i_time] == 1) {
            rational_long_inv_streaks <-
              c(rational_long_inv_streaks, rational_this_inv_count)
          } else if (block_dat$rational_hold[i_time] == -1) {
            rational_short_pos_streaks <-
              c(rational_short_pos_streaks, rational_this_inv_count)
          }
          rational_this_inv_count <- 0
        }
      }
    }

    outcome_table[vpn, str_c('avg_hold_length_', block_nr)] <- ifelse(
      is.null(long_inv_streaks), 0, mean(long_inv_streaks))
    outcome_table[vpn, str_c('avg_short_length_', block_nr)] <- ifelse(
      is.null(short_pos_streaks), 0, mean(short_pos_streaks))

    outcome_table[vpn, str_c('n_holds_', block_nr)] <- ifelse(
      is.null(long_inv_streaks), 0, length(long_inv_streaks))
    outcome_table[vpn, str_c('n_shorts_', block_nr)] <- ifelse(
      is.null(short_pos_streaks), 0, length(short_pos_streaks))

    outcome_table[vpn, str_c('rational_avg_hold_length_', block_nr)] <-
      mean(rational_long_inv_streaks)
    outcome_table[vpn, str_c('rational_avg_short_length_', block_nr)] <-
      mean(rational_short_pos_streaks)

    outcome_table[vpn, str_c('rational_n_holds_', block_nr)] <-
      length(rational_long_inv_streaks)
    outcome_table[vpn, str_c('rational_n_shorts_', block_nr)] <-
      length(rational_short_pos_streaks)

    # What was their hit-rate
    outcome_table[vpn, str_c('hit_rate_', block_nr)] <-
      sum((block_dat$price_diff_from_last > 0 & block_dat$hold == 1) |
      (block_dat$price_diff_from_last < 0 & block_dat$hold == -1),
      na.rm = TRUE) / nrow(block_dat)

    outcome_table[vpn, str_c('rational_hit_rate_', block_nr)] <-
      (sum((block_dat$price_diff_from_last > 0 &
        block_dat$rational_hold == 1) |
      (block_dat$price_diff_from_last < 0 & block_dat$rational_hold == -1),
      na.rm = TRUE)) / nrow(block_dat)

# Reaction Times -----------------------------------------------------
    rt_table[vpn, str_c('mean_RT_', block_nr)] <- block_dat %>%
        dplyr::select(time_to_order) %>%
        colMeans(na.rm = TRUE)

    rt_table[vpn, str_c('mean_lottery_RT_', block_nr)] <- block_dat %>%
        dplyr::select(time_to_belief_report) %>%
        colMeans(na.rm = TRUE)
  } # End Block loop

  # # DE over blocks:
  de_table$pgr_12[vpn] <- sum(de_list$n_sold_gains[1:2]) /
    sum(de_list$n_gains[1:2])
  de_table$plr_12[vpn] <- sum(de_list$n_sold_losses[1:2]) /
    sum(de_list$n_losses[1:2])
  de_table$pgr_34[vpn] <- sum(de_list$n_sold_gains[3:4]) /
    sum(de_list$n_gains[3:4])
  de_table$plr_34[vpn] <- sum(de_list$n_sold_losses[3:4]) /
    sum(de_list$n_losses[3:4])

  de_table$rational_pgr_12[vpn] <- sum(rational_de_list$n_sold_gains[1:2]) /
    sum(rational_de_list$n_gains[1:2])
  de_table$rational_plr_12[vpn] <- sum(rational_de_list$n_sold_losses[1:2]) /
    sum(rational_de_list$n_losses[1:2])
  de_table$rational_pgr_34[vpn] <- sum(rational_de_list$n_sold_gains[3:4]) /
    sum(rational_de_list$n_gains[3:4])
  de_table$rational_plr_34[vpn] <- sum(rational_de_list$n_sold_losses[3:4]) /
    sum(rational_de_list$n_losses[3:4])

  # Administratives:
  de_table$main_condition[vpn] <- as.character(block_dat$condition[1])

  de_table$participant_code[vpn] <- as.character(dat$participant_code[1])
  de_table$session_code[vpn] <- as.character(dat$session.code[1])

  # Payoff:
  outcome_table[vpn, 'participant_payoff'] <- dat$participant.payoff[1]

  # Other variables
  if (study_stage != 'param_recov')
    outcome_table$ravens_matrices_score[vpn] <- dat$cogn_rpm_total_points[1]

  print(str_c('Done with subj ', vpn))
} # End participant loop


###########################################################################

de_table$pgr_diff_12_34 <- de_table$pgr_34 - de_table$pgr_12
de_table$plr_diff_12_34 <- de_table$plr_34 - de_table$plr_12

# Compute actual DE measure
de_table$de_12 <- de_table$pgr_12 - de_table$plr_12
de_table$de_34 <- de_table$pgr_34 - de_table$plr_34
de_table$de_diff_12_34 <- de_table$de_34 - de_table$de_12

# Benchmark versions
de_table$rational_pgr_diff_12_34 <-
  de_table$rational_pgr_34 - de_table$rational_pgr_12
de_table$rational_plr_diff_12_34 <-
  de_table$rational_plr_34 - de_table$rational_plr_12

# Compute actual DE measure
de_table$rational_de_12 <- de_table$rational_pgr_12 - de_table$rational_plr_12
de_table$rational_de_34 <- de_table$rational_pgr_34 - de_table$rational_plr_34
de_table$rational_de_diff_12_34 <-
  de_table$rational_de_34 - de_table$rational_de_12

# Merge the tables for easyer handling ----------------------------------
demographics_table <- bind_rows(demographics_list)

complete_table <- bind_cols(de_table, rt_table,
  outcome_table, demographics_table)


# Exclusion ########################################################
if (study_stage != 'param_recov') {
  complete_table$excluded_reason <- ''

  # Excluding the inattentives:
  complete_table$excluded <- complete_table$engagement <= 3
  complete_table$excluded_reason[complete_table$engagement <= 3] <-
    str_c(complete_table$excluded_reason[complete_table$engagement <= 3], 'Low_Engagement, ')

  # Excluding those who never held an asset
  never_held_at_all <- dat_main_long %>%
    group_by(participant_code, i_block) %>%
    summarise(never_held = all(hold == 0)) %>%
    filter(!never_held) %>%
    summarise(never_held_all_blocks = !all(!never_held))

  # Join the exclusion rows together and expand to the long dataframe
  temp_excluded <- left_join(never_held_at_all,
    dplyr::select(complete_table, participant_code, excluded),
      by = 'participant_code')
  temp_excluded$excluded <- temp_excluded$excluded | temp_excluded$never_held_all_blocks

  complete_table <- left_join(dplyr::select(complete_table, -excluded),
    temp_excluded, by = 'participant_code')
  complete_table$excluded_reason[complete_table$never_held_all_blocks] <-
    str_c(complete_table$excluded_reason[complete_table$never_held_all_blocks],
          'Never_Held, ')

  dat_main_long <- left_join(
    dplyr::select(dat_main_long, -contains('excluded')),
    complete_table[, c('participant_code', 'excluded', 'excluded_reason')],
    by = 'participant_code')

# Saving the Output ###################################################
  write_delim(de_table,
              file.path('Output', 'Tables', str_c('Descriptives_DE_',
                Sys.Date(), '_', study_stage, '.csv')),
              delim = ';')
  write_delim(rt_table,
              file.path('Output', 'Tables', str_c('Descriptives_RT_',
                Sys.Date(), '_', study_stage, '.csv')),
              delim = ';')
  write_delim(outcome_table,
              file.path('Output', 'Tables', str_c('Descriptives_outcomes_',
                Sys.Date(), '_', study_stage, '.csv')),
              delim = ';')
  write_delim(demographics_table,
              file.path('Output', 'Tables', str_c('Descriptives_demographics_',
                Sys.Date(), '_', study_stage, '.csv')),
              delim = ';')
  write_delim(complete_table,
              file.path('Output', 'Tables', str_c('Descriptives_complete_',
                Sys.Date(), '_', study_stage, '.csv')),
              delim = ';')
} # End param_recov condition

write_delim(dat_main_long, file.path('..', 'Data', 'Clean',
  str_c('all_participants_long_main_', study_stage, '.csv')),
  delim = ';')

# Loading the Output back in ----------------------------------------------

if (FALSE)
  complete_table <- read_delim(file.path('Output', 'Tables',
    'Descriptives_complete_2019-10-17_main.csv'), delim = ';')