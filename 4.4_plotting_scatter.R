# Scatterplots -------------------------------------------------------------

# Incentivasation: DE vs. Payoff
ggplot(data = complete_table,
       aes(x = DE_12, y = participant_payoff, color = main_condition)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = blue_set_3,
                     labels = c('Baseline', 'Partial Info.', 'States Shown')) +
  labs(x = 'DE in Baseline Blocks', y = 'Payoff Points', color = 'Condition')

# DE per Condition
ggplot(data = filter(complete_table, !is.na(main_condition)),
       aes(x = DE_12, y = DE_34, color = main_condition)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    theme_minimal() +
    # scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
    #                    values = c('blue1', 'cyan3', 'skyblue4')) +
    labs(color = 'Condition', x = 'DE in Baseline Blocks', y = 'DE in Treatment Blocks')
    # Main exp: People stayed pretty consitant between Block 1 & 2.


# DE vs. rational DE
ggplot(complete_table,
  aes(rational_de_34, de_34, color = main_condition)) +
  geom_point() +
  geom_smooth(method = 'lm')

# To prevent sourcing
if(FALSE){
  # DE per Study Field
  ggplot(data = filter(complete_table, !is.na(main_condition)),
         aes(x = DE_0, y = DE_1, color = study_field_coarse)) +
      geom_point() +
      geom_smooth(method = 'lm', alpha = .25) +
      theme_minimal() +
      labs(color = 'Field', x = 'DE Block 1', y = 'DE Block 2')
}


#  Discrepancy between bayes and beliefs over time (in the first block)
# ggplot(data = filter(dat_main_long, i_block < 2),
#        aes(x = round_number, y = abs(bayes_prob_up - belief / 100))) +
#     geom_point(alpha = .15, color = 'skyblue4') +
#     geom_smooth(method = 'lm', col = 'red3') +
#     labs(x = 'Round Number', y = 'abs(Bayesian - Belief)') +
#     theme_minimal()

# To prevent sourcing
if(FALSE){
  # DE and final payoff: Does not work for main study
  ggplot(data = complete_table, aes(x = DE_12, y = payoff_trading_1)) +
      geom_point(color = 'skyblue4') +
      geom_smooth(method = 'lm', col = 'red3') +
      labs(x = 'Disposition Effect', y = 'Payoff') +
      theme_minimal()
}

# RTs over time by main condition
p <- ggplot(data = na.omit(dplyr::select(dat_main_long, round_number,
                                  time_to_order, main_condition, i_block)),
       aes(x = round_number, y = log(time_to_order), color = main_condition)) +
    geom_point(alpha = .2) +
    geom_smooth(method = 'lm',
                aes(group = interaction(main_condition, i_block))) +
    labs(x = 'Round Number', y = 'Log Decision Time', color = 'Main Condition') +
    theme_minimal() +
    scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
                       values = c('cyan3', 'blue2', 'black'))

p
ggplotly(p)

# RTs over time general
# p <- ggplot(data = na.omit(dplyr::select(dat_main_long, round_number,
#                                   time_to_order, main_condition, i_block)),
#        aes(x = round_number, y = log(time_to_order), color = main_condition)) +
#     geom_point(alpha = .2) +
#     geom_smooth(method = 'lm', color = 'red4', aes(group = i_block)) +
#     labs(x = 'Round Number', y = 'log(Decision Time)', color = 'Main Condition') +
#     theme_minimal() +
#     scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
#                        values = c('cyan3', 'blue2', 'black'))
#
# p
# ggplotly(p)


# Beliefs vs. rational beliefs (Steveplot)
ggplot(filter(dat_main_long, i_block < 2, position != 'No Returns', hold != -1),
       aes(x = bayes_prob_up, y = belief / 100, color = position)) +
    # geom_point(alpha = .2) +
    geom_abline(slope = 1, intercept = 0, color = 'red', size = 1) +
    # stat_function(fun = function(x) 10*x**(1/.75), size = 1) + # TODO: How to dashed line?
    stat_smooth(method = 'auto') +
    labs(x = 'Bayesian Probability', y = 'Belief', color = 'Position') +
    scale_color_manual(values = c('cyan', '#007A00', '#40EA40', 'blue4')) +
    ggtitle('Bayesian Probability vs. Reported Belief', '(Shorts not included)') +
    theme_minimal() +
    theme(text = element_text(size=14))

ggplotly()


# Belief Vs. Number of Ups/downs
# plot_list$belief_by_cum_n_moves <-
ggplot(filter(dat_main_long, i_block < 2, !is.na(cumulative_moves_while_invested)),
       aes(x = cumulative_moves_while_invested, y = belief / 100, color = position)) +
  geom_point(alpha = .2, position = position_jitter(width = .25)) +
  stat_smooth(method = 'gam') +
  labs(x = 'Cumulative Price Moves', y = 'Belief', color = 'Position') +
  scale_color_manual(values = c('cyan', '#007A00', '#40EA40', 'blue4')) +
  theme_minimal() +
  theme(text = element_text(size=14))
ggplotly()


# Belief update vs. Bayes update by Position and move
# TODO: Make two plots for long/short!
# plot_list$belief_bayes_by_pos_move_scatter <-
  ggplot(
    filter(dat_main_long, i_block < 2, position_laged %in% c('Gain', 'Loss'),
           !is.na(price_move_from_last_corrected)),
    aes(x = bayes_diff_from_last, y = belief_diff_from_last / 100,
        color = interaction(position_laged, price_move_from_last_corrected))) +
  stat_smooth(method = 'lm') +
  # stat_smooth(data = filter(dat_main_long, position_laged == 'Not Invested'),
  #             aes(x = bayes_diff_from_last, y = belief_diff_from_last / 100),
  #             method = 'lm') +
  # stat_smooth(method = 'auto', linetype = 'dotted', alpha = 0) +
  geom_abline(slope = 1, intercept = 0, color = 'red', size = 1) +
  labs(x = 'Bayesian Probability Update', y = 'Difference in reported Beliefs',
       color = 'Position/Price Move') +
  # scale_color_manual(values = c('cyan', '#007A00', '#40EA40', 'blue4')) +
  theme_minimal() +
  theme(text = element_text(size=14))
# TODO: Look at this again, cause this tells a different story...

ggplotly(plot_list$belief_bayes_by_pos_move_scatter)

# Ben-David style plot of propensities to sell gains/losses
# This includes all but the manipulated blocks.
ggplot(filter(dat_main_long, i_block < 2 | main_condition == 'baseline', hold != 0),
       aes(x = returns, y = as.numeric(transaction != 0), group = returns > 0)) +
    geom_point(alpha = .1, position = position_jitter(height = .1, width = .5), color = 'skyblue4') +
    stat_summary_bin(geom = 'point', bins = 50, fun = 'mean') +
    geom_smooth(method = 'lm', formula = y ~ poly(x, 3), color = 'red3') +
    labs(x = 'Return', y = 'Propensity to Sell') +
    theme_minimal()

# Number of Long investments vs. number of short investments
# Only baseline blocks

dat_main_long %>%
  filter(i_block < 2) %>%
  group_by(participant_code) %>%
  summarise(prop_long = mean(hold == 1),
            prop_short = mean(hold == -1)) %>%

  ggplot(aes(prop_long, prop_short)) +
  geom_point() +
  stat_smooth(method = 'lm')

# Hit rates:
complete_table %>%
  mutate(baseline_hit_rates = (hit_rate_1 + hit_rate_2) / 2,
    rational_baseline_hit_rates = (rational_hit_rate_1 +
      rational_hit_rate_2) / 2) %>%
  ggplot(aes(rational_baseline_hit_rates, baseline_hit_rates)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_abline(intercept = 0, slope = 1, color = 'red')

# Hit rates by treatment
complete_table %>%
  mutate(treatment_hit_rates = (hit_rate_3 + hit_rate_4) / 2,
    rational_treatment_hit_rates = (rational_hit_rate_3 +
      rational_hit_rate_4) / 2) %>%
  ggplot(aes(rational_treatment_hit_rates, treatment_hit_rates,
    color = main_condition)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    geom_abline(intercept = 0, slope = 1, color = 'red')