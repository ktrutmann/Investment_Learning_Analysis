# Cuttingroom floor:

# A) Outcome tables per Condition:
DE_table <- tibble(PRG_baseline=temp, PRL_baseline=temp, DE_baseline=temp,
                   n_sales_baseline=temp,
                   PRG_probs_shown=temp, PRL_probs_shown=temp, DE_probs_shown=temp,
                   n_sales_probs_shown=temp,
                   PRG_states_shown=temp, PRL_states_shown=temp, DE_states_shown=temp,
                   n_sales_states_shown=temp)

RT_table <- tibble(mean_RT_baseline=temp, mean_RT_probs_shown=temp,
                   mean_RT_states_shown=temp, mean_unfocused_RT=temp,
                   mean_lottery_RT_baseline = temp, mean_lottery_RT_probs_shown=temp,
                   mean_lottery_RT_states_shown=temp, mean_unfocused_RT_lottery=temp)

outcome_table <- tibble(cor_lottery_bayes_baseline=temp, cor_lottery_bayes_probs_shown=temp,
                        cor_lottery_bayes_states_shown=temp,
                        p_inertia_baseline=temp, p_inertia_probs_shown=temp,
                        p_inertia_states_shown=temp,
                        p_lottery_predictiveness_baseline=temp,
                        p_lottery_predictiveness_probs_shown=temp,
                        p_lottery_predictiveness_states_shown=temp)


# From create_descriptive_vars.R -------------------------------------------

# DE_EV and DE_EU measure:
        # This measure shows how an EV and EU trader would have done given the reported beliefs
        # It is simulated 1000 times, to get rid of the random element if q == .5
        # TODO: Would it also work (and be far simpler) to just count each q == .5 as .5 sells / buys?
        DE_EV_vector <- rep(NA, 5000)
        DE_EU_vector <- rep(NA, 5000)
        this_belief <- pull(this_dat, belief)

        for (i_sim in 1:length(DE_EV_vector)){
            hold_EV <- this_belief %>%
                `>`(.5) %>%
                {ifelse(this_dat$belief == 5, runif(nrow(this_dat)) > .5, .)}  # randomise where q == .5
            transaction_EV <- c(diff(hold_EV), ifelse(hold_EV[length(hold_EV)], -1, 0))

            hold_EU <- (this_dat$cash + this_dat$returns + c(5, 10, 15))**de_eu_alpha * this_belief +
                (this_dat$cash + this_dat$returns - c(5, 10, 15))**de_eu_alpha * (1 - this_belief) >
                (this_dat$cash + this_dat$returns)**de_eu_alpha
            transaction_EU <- c(diff(hold_EU), ifelse(hold_EU[length(hold_EU)], -1, 0))

            # Track the returns
            returns_EV <- c(0)
            returns_EU <- c(0)
            for (i in 1:(nrow(this_dat)-1)){
                returns_EV <- c(returns_EV,
                                ifelse(hold_EV[i + 1],
                                       returns_EV[length(returns_EV)] + diff(this_dat$price)[i],
                                       0))
                returns_EU <- c(returns_EU,
                                ifelse(hold_EU[i + 1],
                                       returns_EU[length(returns_EU)] + diff(this_dat$price)[i],
                                       0))
            }

            # EV
            n_losses_EV <- sum(returns_EV < 0)
            n_gains_EV <- sum(returns_EV > 0)
            n_sold_losses_EV <- sum(returns_EV < 0 & transaction_EV < 0)
            n_sold_gains_EV <- sum(returns_EV > 0 & transaction_EV < 0)

            plr_EV <- ifelse(n_losses_EV == 0, 0, n_sold_losses_EV / n_losses_EV)
            pgr_EV <- ifelse(n_gains_EV == 0, 0, n_sold_gains_EV / n_gains_EV)

            DE_EV_vector[i_sim] <- pgr_EV - plr_EV

            # EU
            n_losses_EU <- sum(returns_EU < 0)
            n_gains_EU <- sum(returns_EU > 0)
            n_sold_losses_EU <- sum(returns_EU < 0 & transaction_EU < 0)
            n_sold_gains_EU <- sum(returns_EU > 0 & transaction_EU < 0)

            plr_EU <- ifelse(n_losses_EU == 0, 0, n_sold_losses_EU / n_losses_EU)
            pgr_EU <- ifelse(n_gains_EU == 0, 0, n_sold_gains_EU / n_gains_EU)

            DE_EU_vector[i_sim] <- pgr_EU - plr_EU
        }

        de_measure_EV <- mean(DE_EV_vector)
        de_table[vpn, str_c('DE_EV_', block_nr)] <- de_measure_EV

        de_measure_EU <- mean(DE_EU_vector)
        de_table[vpn, str_c('DE_EU_', block_nr)] <- de_measure_EU


# price_after_trades split by gain/loss ###############################
target_moves <- with(dat_main_long,
  which(abs(transaction) == 1))

moves_after_decision <- tibble(
  original_move_ix = rep(target_moves, each = 5 + 1),
  move_id = rep(seq_along(target_moves), each = 5 + 1),
  target_shift = rep(0:5, length(target_moves)))  %>%
  mutate(move_ix = original_move_ix + target_shift)

full_shift_df <- dat_main_long %>%
  dplyr::select(hold, transaction, price,
    bayes_prob_up, position) %>%
  dplyr::slice(moves_after_decision$move_ix) %>%
  bind_cols(moves_after_decision) %>%
  group_by(move_id) %>%  # Normalizing price
  mutate(price_normal = price - price[target_shift == 0],
    original_hold = hold[target_shift == 0],
    original_move = factor(transaction[target_shift == 0]),
    price_normal = case_when(
      original_hold == 0 & original_move == -1 ~ - price_normal,
      original_hold == -1 ~ - price_normal,
      TRUE ~ price_normal),
    original_position = factor(position[target_shift == 0]),
    n_blocks = length(unique(i_block))) %>%  # Removing block overlaps
  ungroup() %>%
  filter(n_blocks == 1,
    original_position != 'No Returns')

dat_prepared <- full_shift_df %>%
  group_by(original_position, target_shift) %>%
  summarize(avg_price = mean(price_normal),
    se = sd(price_normal) / sqrt(n()),
    ci_upper = avg_price + qt(.9, n()) * se,
    ci_lower = avg_price - qt(.9, n()) * se,
    n = n())

ggplot(dat_prepared,
  aes(x = target_shift, y = avg_price, color = original_position)) +
  geom_line(size = 1.25) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(), width = .05) +
  scale_color_manual(values = c('Black', '#888888', '#cccccc'),
                    labels = c('Invest', 'Liq. Gain', 'Liq. Loss')) +
  labs(x = 'Periods since Transaction', y = 'Normalised Mean Price',
       color = 'Transaction', group = 'Transaction')