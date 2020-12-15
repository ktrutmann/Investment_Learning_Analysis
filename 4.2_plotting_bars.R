library(ggplot2)
library(plotly)

theme_set(theme_minimal())

# DE ############################################################

# DE by Condition phase 1: ---------------------------------------------------
dat_prepared <- complete_table %>%
  group_by(main_condition) %>%
  summarise(mean_DE = mean(de_12),
            SE = sd(de_12) / sqrt(length(de_12)),
            CI_90 = SE * qt(.95, length(de_12)),
            lower = mean(de_12) - CI_90,
            upper = mean(de_12) + CI_90)


ggplot(dat_prepared,
       aes(x = main_condition, y = mean_DE)) +
  geom_bar(stat = 'identity', fill = 'skyblue4') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1, color = 'darkred') +
  ggtitle('DE Phase 1 (Blocks 1 & 2)') +
  scale_x_discrete(labels = c('Baseline', 'Probabilities Shown', 'States Shown')) +
  labs(x = 'Condition', y = 'Mean DE')


# DE to Benchmark by Condition phase 2: --------------------------------------
dat_prepared <- complete_table %>%
  mutate(de_34 = ifelse(
    main_condition != 'states_shown', de_34 + .78, de_34)) %>%
  group_by(main_condition) %>%
  summarise(mean_DE = mean(de_34),
            SE = sd(de_34) / sqrt(length(de_34)),
            CI_90 = SE * qt(.95, length(de_34)),
            lower = mean(de_34) - CI_90,
            upper = mean(de_34) + CI_90)


ggplot(dat_prepared,
       aes(x = main_condition, y = mean_DE)) +
  geom_bar(stat = 'identity', fill = 'skyblue4') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1, color = 'darkred') +
  ggtitle('DE distance to Benchmark in Block 3 & 4', 'Treatment Part') +
  scale_x_discrete(labels = c('Baseline', 'Probabilities Shown', 'States Shown')) +
  labs(x = 'Condition', y = 'Mean DE')


# DE (to benchmark) difference between phases by Condition: -------------------
dat_prepared <- complete_table %>%
  mutate(de_diff_12_34 = ifelse(
    main_condition == 'states_shown', de_diff_12_34 - .78, de_diff_12_34)) %>%
  group_by(main_condition) %>%
  summarise(mean_DE = mean(de_diff_12_34),
            SE = sd(de_diff_12_34) / sqrt(length(de_diff_12_34)),
            CI_90 = SE * qt(.95, length(de_diff_12_34)),
            lower = mean(de_diff_12_34) - CI_90,
            upper = mean(de_diff_12_34) + CI_90)

ggplot(dat_prepared, aes(x = main_condition, y = mean_DE,
  fill = main_condition)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, #color = 'darkred'
    ) +
  # ggtitle('Difference of DE',
  #         'Treatment minus Baseline Blocks, corrected for benchmark') +
  labs(x = 'Condition', y = 'Difference in DE Distance to Benchmark') +
  scale_x_discrete(labels = c('Baseline', 'Partial Info.', 'Full Info.')) +
  scale_fill_manual(values = c(rgb(0.77, 0.92, 0.92),
    rgb(0.656, 0.856, 0.836), rgb(0.56, 0.76, 0.74))) +
  annotate('text', 2, -.21, label = '*', size = 25) +
  annotate('text', 2, -.81, label = '*', size = 25) +
  annotate('text', 3, -.81, label = '*', size = 25) +
  annotate('segment', 1, -.77, xend = 3, yend = -.77, size = 1) +
  annotate('segment', 1, -.77, xend = 1, yend = -.75, size = 1) +
  annotate('segment', 3, -.77, xend = 3, yend = -.75, size = 1) +
  theme(legend.position = 'none',
    text = element_text(size = 44))

ggsave('de_diff_conditions_bars_col.png',  device = 'png',
  width = 20, height = 14, path = file.path('Output', 'Plots', 'Bars'))


# Belief updating: #####################################################

# Blocks 1 & 2 -----------------------------------------------------
dat_prepared <- dat_main_long %>%
  filter(position_end_of_last_period %in% c('Gain', 'Loss'), !is.na(position_end_of_last_period),
         i_block < 2, updating_type != 'Wrong') %>%
    droplevels() %>%
  group_by(position_end_of_last_period, price_move_from_last_corrected) %>%
  summarise(mean_update = mean(belief_diff_bayes_corrected_flipped),
            SE = sd(belief_diff_bayes_corrected_flipped) / sqrt(length(belief_diff_bayes_corrected_flipped)),
            CI_90_lower = mean_update - SE * qt(.95, length(belief_diff_bayes_corrected_flipped)),
            CI_90_upper = mean_update + SE * qt(.95, length(belief_diff_bayes_corrected_flipped)))


  ggplot(dat_prepared,
         aes(x = position_end_of_last_period, y = mean_update,
             fill = price_move_from_last_corrected)) +
  facet_grid(cols = vars(price_move_from_last_corrected)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = CI_90_lower,
                    ymax = CI_90_upper), width = .1, color = tail(col_set, 1)) +
  scale_fill_manual(name = '', values = c('Favorable' = col_set[1],
                                          'Unfavorable' = col_set[3])) +
  ggtitle('Corrected and flipped Belief Updates',
          subtitle = 'Baseline Part, wrong sign updates excluded') +
  labs(x = 'Position and Price Move', y = 'Mean Corrected Belief Update') +
  theme(text = element_text(size = text_size)) +
  theme(legend.position = 'none')
ggplotly()


# In blocks 3 & 4 -----------------------------------------------------
dat_prepared <- dat_main_long %>%
  filter(position_end_of_last_period %in% c('Gain', 'Loss'),
         !is.na(position_end_of_last_period), i_block > 1,
         updating_type != 'Wrong') %>%
  droplevels() %>%
  group_by(position_end_of_last_period, price_move_from_last_corrected, main_condition) %>%
  summarise(mean_update = mean(belief_diff_bayes_corrected_flipped),
            SE = sd(belief_diff_bayes_corrected) / sqrt(n()),
            CI_90_lower = mean_update - SE * qt(.95, n()),
            CI_90_upper = mean_update + SE * qt(.95, n()))

ggplot(dat_prepared,
       aes(x = position_end_of_last_period, y = mean_update, fill = main_condition,
           group = main_condition)) +
  facet_grid(cols = vars(price_move_from_last_corrected)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(ymin = CI_90_lower, ymax = CI_90_upper),
                position = position_dodge(.9), width = .1, color = 'darkred') +
  scale_fill_manual(values = c('cyan3', 'skyblue4', 'darkblue'),
                    labels = c('Baseline', 'Partial Info.', 'Full Info.')) +
  ggtitle('Corrected and flipped Beleif Updates',
          subtitle = 'Treatment Part, wrong sign updates excluded') +
  labs(x = 'Price Movement', y = 'Mean Corrected Belief Update', fill = 'Condition') +
  theme(text = element_text(size = text_size))
ggplotly()

# Diff in Diff of of blocks 3 & 4 -----------------------------------------
{
dat_prepared_diff <- dat_main_long %>%
    filter(position_end_of_last_period %in% c('Gain', 'Loss'),
           !is.na(position_end_of_last_period), i_block > 1,
           updating_type != 'Wrong') %>%
    droplevels() %>%
    group_by(position_end_of_last_period, price_move_from_last_corrected, main_condition) %>%
    summarise(mean_update = mean(belief_diff_bayes_corrected_flipped),
              mean_var = var(belief_diff_bayes_corrected_flipped)) %>%
    ungroup() %>%
    group_by(main_condition, price_move_from_last_corrected) %>%
    summarise(diff = diff(mean_update),
              SE = sqrt(sum(mean_var)),
              CI_dist = SE * qt(.95, n()),
              CI_90_l = diff - CI_dist,
              CI_90_u = diff + CI_dist
              )

  ggplot(dat_prepared_diff,
         aes(x = price_move_from_last_corrected, y = diff, fill = main_condition,
             group = main_condition)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    # geom_errorbar(aes(ymin = CI_90_l, ymax = CI_90_u),
                  # position = position_dodge(.9), width = .1, color = 'darkred') +
    scale_fill_manual(values = c('cyan3', 'skyblue4', 'darkblue'),
                      labels = c('Baseline', 'Partial Info.', 'Full Info.')) +
    ggtitle('Loss - Gain Updates',
            subtitle = '(Condition Blocks)') +
    labs(x = 'Price Move', y = 'Mean Belief Update Difference',
         fill = 'Condition') +
    theme(text = element_text(size = text_size))

  }
}

# Updating as in Paper ---------------------------------------------------
# This is the colored version from presentations
dat_prepared <- dat_main_long %>%
  filter(position_end_of_last_period != 'No Returns',
         updating_type != 'Wrong',
         i_block < 2) %>%
  mutate(belief_diff_bayes_corrected_flipped =
    belief_diff_bayes_corrected_flipped / 100)

dat_prepared_1 <- dat_prepared %>%
  filter(position_end_of_last_period != 'Not Invested') %>%
  group_by(position_end_of_last_period, price_move_from_last_corrected) %>%
  summarise(mean_update = mean(
              belief_diff_bayes_corrected_flipped, na.rm = TRUE),
            SE = sd(belief_diff_bayes_corrected_flipped, na.rm = TRUE) /
              sqrt(n()),
            CI_90_lower = mean_update - SE * qt(.95, n()),
            CI_90_upper = mean_update + SE * qt(.95, n()))

dat_prepared_2 <- dat_prepared %>%
  filter(position_end_of_last_period == 'Not Invested') %>%
  summarise(mean_update = mean(
              belief_diff_bayes_corrected_flipped, na.rm = TRUE),
            SE = sd(belief_diff_bayes_corrected_flipped, na.rm = TRUE) /
              sqrt(n()),
            CI_90_lower = mean_update - SE * qt(.95, n()),
            CI_90_upper = mean_update + SE * qt(.95, n())) %>%
  add_column(tibble(position_end_of_last_period = 'Not Invested',
    price_move_from_last_corrected = ''))

dat_prepared <- full_join(dat_prepared_1, dat_prepared_2)

ggplot(dat_prepared,
       aes(x = position_end_of_last_period, y = mean_update,
           fill = price_move_from_last_corrected)) +
  facet_grid(cols = vars(price_move_from_last_corrected),
    scale = 'free_x', space = 'free_x') +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = CI_90_lower, ymax = CI_90_upper),
                    width = .05, color = tail(col_set, 1)) +
  scale_fill_manual(name = '', values = c(rgb(0.77, 0.92, 0.92),
    rgb(0.656, 0.856, 0.836), rgb(0.56, 0.76, 0.74))) +
  labs(x = 'Position and Price Move',
    y = 'Average Belief Update') +
  theme(legend.position = 'none',
    text = element_text(size = 22))

ggsave('updating_baseline_wrong_excluded_bars_col.pdf',  device = 'pdf',
  width = 10, height = 7, path = file.path('Output', 'Plots', 'Bars'))


# Updating RL corrected -----------------------------------------------------
# The belief updating but not corrected for a bayesian updater but a RL learner
# with alpha = .3
dat_prepared <- dat_main_long %>%
dplyr::filter(position_end_of_last_period %in% c('Gain', 'Loss'),
  !is.na(position_end_of_last_period), i_block < 2,
  updating_type != 'Wrong'
     ) %>%
  droplevels() %>%
group_by(position_end_of_last_period, price_move_from_last_corrected) %>%
summarise(mean_update = mean(belief_diff_rl_corrected_flipped),
          SE = sd(belief_diff_rl_corrected_flipped) / sqrt(length(belief_diff_rl_corrected_flipped)),
          CI_90_lower = mean_update - SE * qt(.95, length(belief_diff_rl_corrected_flipped)),
          CI_90_upper = mean_update + SE * qt(.95, length(belief_diff_rl_corrected_flipped)))

ggplot(dat_prepared,
       aes(x = position_end_of_last_period, y = mean_update,
           fill = price_move_from_last_corrected)) +
facet_grid(cols = vars(price_move_from_last_corrected)) +
geom_bar(stat = 'identity') +
geom_errorbar(aes(ymin = CI_90_lower, ymax = CI_90_upper),
                  width = .1, color = tail(col_set, 1)) +
scale_fill_manual(name = '', values = c('Favorable' = col_set[1],
                                        'Unfavorable' = col_set[3])) +
labs(x = 'Position and Price Move', y = 'Average Corrected Belief Update') +
theme_minimal() +
theme(legend.position = 'none')


# Other:
{
  # Ravens Scores -------------------------------------------------------------
ggplot(complete_table,
       aes(x = ravens_matrices_score)) +
  stat_count(fill = 'skyblue4') +
  labs(x = 'Score', y = 'Count')
}