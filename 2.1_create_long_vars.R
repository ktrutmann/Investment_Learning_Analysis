library(tidyverse)

# Loading Data: ################################################################

study_stage <- 'param_recov'  # With what part of the study are we dealing here?
clean_dat_path <- file.path('..', 'data', 'clean')

dat_main_long <- read_delim(file.path(clean_dat_path,
  str_c('all_participants_long_main_', study_stage, '.csv')),
delim = ';', guess_max = 5000)

# Constants ###############################################################
rl_alpha <- .3  # Learning rate for a hypothetical RL agent
seed <- 1234

# Creating Variables ######################################################
# A function to generate a "cumulative amount of price moves while invested"
count_n_moves <- function(moves, invested) {
  n_moves_vec <- vector(mode = 'numeric', length = length(moves))
  n_moves_now <- 0
  for (i in seq_along(moves)) {
    if (is.na(moves[i]) | is.na(invested[i])) {  # Guard against NAs
      n_moves_vec[i] <- NA
      n_moves_now <- 0
      next
    }
    # Register "switches" as restarts:
    if (!is.na(invested[i - 1]) & invested[i - 1] != invested[i])
      n_moves_now <- 0

    if (invested[i] != 0) {
      if (moves[i] == 'Favorable') {
        n_moves_now <- n_moves_now + 1
      } else {
        n_moves_now <- n_moves_now - 1
      }
      n_moves_vec[i] <- n_moves_now
    } else {
      n_moves_now <- 0
      n_moves_vec[i] <- NA
    }
  }
  return(n_moves_vec)
}

# Model Beliefs -----------------------------------------------------------
dat_main_long$rl_belief <- NA
signal <- as.integer(c(NA, diff(dat_main_long$price) > 0))

progress <- 5
for (i in seq_len(nrow(dat_main_long))) {
  if (i == 1) {
    this_block_i <- dat_main_long$i_block[1]
    dat_main_long$rl_belief[1] <- .5
    next
  }

  if (is.na(dat_main_long$i_block[i - 1])) {
    dat_main_long$rl_belief[i] <- .5
    this_block_i <- dat_main_long$i_block[i]
    next
  }

  prev_belief <- dat_main_long$rl_belief[i - 1]
  dat_main_long$rl_belief[i] <- prev_belief + rl_alpha * (signal[i] - prev_belief)
  this_block_i <- dat_main_long$i_block[i]

  if (i %% as.integer(nrow(dat_main_long) / 20) == 0) {
    cat(str_c('\rModel beliefs: ',
      progress, '%'))
    progress <- progress + 5
  }
}


# Creating the variables-------------------------------------------------------
dat_main_long <- dat_main_long %>%
  mutate(participant_code = as.factor(participant_code),
   belief_diff_from_last = c(NA, diff(belief)),
   rational_belief = case_when(
    condition == 'states_shown' & state == 1 ~ .65,
    condition == 'states_shown' & state == 0 ~ .35,
    TRUE ~ bayes_prob_up),
   bayes_diff_from_last = c(NA, diff(rational_belief)),
   rl_diff_from_last = c(NA, diff(rl_belief)),
   belief_diff_bayes_corrected = belief_diff_from_last -
   (bayes_diff_from_last * 100),
   belief_diff_rl_corrected = belief_diff_from_last -
   (rl_diff_from_last * 100),
   updating_type = case_when(sign(belief_diff_from_last) !=
    sign(bayes_diff_from_last) ~ 'Wrong',
    abs(belief_diff_from_last) > abs(bayes_diff_from_last * 100)
    ~ 'Over', TRUE ~ 'Under'),
   price_diff_from_last = c(NA, diff(price)),
   price_diff_to_next = c(diff(price), NA),
   price_move_from_last = as.factor(c(NA, if_else(diff(price) > 0,
    'Up', 'Down'))),
   price_move_from_last_corrected = as.factor(
    case_when(
      i_round_in_block == 0 ~ NA_character_,
      hold == -1 & price_diff_from_last < 0 ~ 'Favorable',
      hold == -1 & price_diff_from_last > 0 ~ 'Unfavorable',
      hold != -1 & price_diff_from_last < 0 ~ 'Unfavorable',
      hold != -1 & price_diff_from_last > 0 ~ 'Favorable')),
   position = case_when(
      hold != 0 & returns > 0 ~ 'Gain',
      hold != 0 & returns < 0 ~ 'Loss',
      hold != 0 & returns == 0 ~ 'No Returns',
      TRUE ~ 'Not Invested'),
   position_end_of_last_period = as.factor(dplyr::lag(case_when(
      position == 'Not Invested' & abs(transaction) == 1 ~ 'No Returns',
      position != 'Not Invested' & abs(transaction) == 1 ~ 'Not Invested',
      abs(transaction) == 2 ~ 'No Returns',
      TRUE ~ position))),
   updated_from = case_when(
      is.na(position_end_of_last_period) |
        is.na(price_move_from_last_corrected) ~ NA_character_,
      str_detect(position_end_of_last_period, 'No') ~ 'Not Invested',
      TRUE ~paste(price_move_from_last_corrected, position_end_of_last_period)),
   cumulative_moves_while_invested = count_n_moves(
    price_move_from_last_corrected, hold),
   belief_diff_flipped = if_else(price_diff_from_last > 0,
    belief_diff_from_last, - belief_diff_from_last),
   bayes_diff_flipped = if_else(price_diff_from_last > 0,
    bayes_diff_from_last, - bayes_diff_from_last),
   belief_diff_bayes_corrected_flipped = if_else(
    price_diff_from_last > 0,
    belief_diff_bayes_corrected, - belief_diff_bayes_corrected),
   belief_diff_rl_corrected_flipped = if_else(
    price_diff_from_last > 0,
    belief_diff_rl_corrected, - belief_diff_rl_corrected),
   implied_alpha = (belief - lag(belief)) /
      (c(0, if_else(diff(price) > 0, 100, 0)) - pmax(pmin(lag(belief), 99.99), .01))
  )

  dat_main_long$belief_bayes_residual <- NA
  dat_main_long$belief_bayes_residual[!is.na(dat_main_long$belief)] <-
    lm(belief / 100 ~ bayes_prob_up, data = dat_main_long)$residuals

  # TODO: (1) We need the diff() for the updating and we need to flip them!
  
# "Optimal Trading" -----------------------------------------------------------
dat_main_long$rational_hold <- case_when(
  dplyr::lag(dat_main_long$bayes_prob_up) > .5 ~ 1,
  dplyr::lag(dat_main_long$bayes_prob_up) < .5 ~ -1)

dat_main_long$rational_hold <- if_else(
  lag(dat_main_long$condition) == 'states_shown',
  if_else(dplyr::lag(dat_main_long$state) == 1, 1, -1),
  dat_main_long$rational_hold)

dat_main_long$rational_trade <- c(diff(dat_main_long$rational_hold), NA)

set.seed(seed = seed)
lottery_probs <- runif(nrow(dat_main_long))
# Whether the lottery is won if played:
lottery_wins <- sapply(lottery_probs,
  function(x) sample(c(TRUE, FALSE), 1, prob = c(x, 1 - x)))

dat_main_long$rational_lottery_earnings <-
  as.integer(if_else(lottery_probs < dat_main_long$rational_belief,
    lottery_wins,
    dat_main_long$price_diff_to_next > 0)) %>%
  replace_na(0)

# Rational returns
dat_main_long$rational_returns <- 0
progress <- 5
cat('\n')
for (i in seq_len(nrow(dat_main_long))) {
 if (i %% as.integer(nrow(dat_main_long) / 20) == 0) {
    cat(str_c('\rRational Returns: ',
      progress, '%'))
    progress <- progress + 5
  }
  if (is.na(dat_main_long$i_round_in_block[i]) |
    dat_main_long$i_round_in_block[i] == 0 |
    is.na(dat_main_long$rational_hold[i]) |
    dat_main_long$rational_hold[i] == 0)
      next

  dat_main_long$rational_returns[i] <-
    dat_main_long$price_diff_from_last[i] * dat_main_long$rational_hold[i]

  if (!is.na(dat_main_long$rational_hold[i - 1]) &
    dat_main_long$rational_hold[i - 1] == dat_main_long$rational_hold[i]) {

    dat_main_long$rational_returns[i] <- dat_main_long$rational_returns[i] +
      dat_main_long$rational_returns[i - 1]
  }
}

# Save Data ---------------------------------------------------------------

# Re-save that dataframe with the additional variables
# TODO: general comments are saved incorrectly! Fix that!
write_delim(dat_main_long, file.path(clean_dat_path,
  str_c('all_participants_long_main_', study_stage, '.csv')), delim = ';')