# Violins ------------------------------------------------------------------

# Belief updating
{
# Updating by position and return sign
CIs <- dat_main_long %>%
  filter(position_laged != 'Not Invested' & i_block < 2) %>%
  group_by(position_laged, price_move_from_last_corrected) %>%
  summarise(mean_update = mean(belief_diff_from_last),
            SE = sd(belief_diff_from_last) / sqrt(length(belief_diff_from_last)),
            lower = mean_update - SE * qt(.95, length(belief_diff_from_last)),
            upper = mean_update + SE * qt(.95, length(belief_diff_from_last)),
            belief_diff_from_last = mean(belief_diff_bayes_corrected_flipped),
            x = price_move_from_last_corrected)

  ggplot(filter(dat_main_long,
                position_laged %in% c('Gain', 'Loss') & i_block < 2),
         aes(x = price_move_from_last_corrected,
             y = belief_diff_bayes_corrected_flipped,
             fill = price_move_from_last_corrected)) +
  facet_grid(cols = vars(position_laged)) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_point(alpha = .08, position = position_jitter(width = .2), color = 'skyblue4') +
  geom_violin(alpha = .8) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.25, color = 'darkred') +
  geom_errorbar(aes(x = x, ymin = lower, ymax = upper), inherit.aes = FALSE,
    data = CIs, color = 'darkred', width = .05) +
  labs(x = 'Last Price Movement', y = 'Belief update',
       facet = 'Position') +
  scale_fill_manual(values = c('skyblue4', 'cyan3')) +
  theme_minimal() +
  theme(legend.position = 'none',
        text = element_text(size = 14))
  ggplotly()
# Well shiver me timbers, looks like I might be right... in the pilot at least.

# Did the intensity of the price movement matter?
ggplot(filter(dat_main_long, position_laged %in% c('Gain', 'Loss'),
                condition == 'baseline', main_condition != 'baseline',
              belief_diff_from_last != 0),
       aes(x = as.factor(price_diff_from_last), y = belief_diff_from_last,
           fill = position_laged)) +
  geom_violin(alpha = .7) +
  geom_point(aes(color = position_laged), alpha = .5, position = position_jitter(.2)) +
  scale_fill_manual(values = c('skyblue4', 'cyan3')) +
  scale_color_manual(values = c('skyblue4', 'cyan3')) +
  labs(x = 'Last Price Movement', y = 'Difference in Beliefs',
       color = 'Position', fill = 'Position') +
  theme_minimal() +
  theme(text = element_text(size = 14))
# Aparently not.
}

# DE
{
# DE diff in diff between blocks:
dat_prepared <- complete_table %>%
  mutate(de_diff_12_34 = ifelse(main_condition == 'states_shown',
                                de_diff_12_34 - .78, de_diff_12_34))

confidence_intervalls <- dat_prepared %>%
  group_by(main_condition) %>%
  summarise(SE = sd(de_diff_12_34) / sqrt(length(de_diff_12_34)),
            CI_90 = SE * qt(.95, length(de_diff_12_34) - 1),
            lower = mean(de_diff_12_34) - CI_90,
            upper = mean(de_diff_12_34) + CI_90,
            mean = mean(de_diff_12_34),
            de_diff_12_34 = mean(de_diff_12_34))

# plot_list$DE_violin_diff_in_diff <-
ggplot(data = dat_prepared, aes(x = main_condition, y = de_diff_12_34)) +
    geom_hline(aes(yintercept = 0), alpha = .5) +
    geom_point(#aes(color = excluded),
               position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.25, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                  data = confidence_intervalls,
                  width = .05, color = 'red') +
    scale_color_manual(name = 'Low Engagement', labels = c('No', 'Yes'),
                       values = c('black', 'red')) +
    labs(x = 'Condition', y = 'Difference of DE') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    ggtitle('Difference of DE by Treatment',
            subtitle = 'Blocks 3&4 - Blocks 1&2, Benchmark Corrected') +
    theme_minimal() +
    theme(text = element_text(size = text_size))
ggplotly()

# Main study DE in Block 1 & 2:
{
confidence_intervalls <- complete_table %>%
  group_by(main_condition) %>%
  summarise(SE = sd(DE_12)/sqrt(length(DE_12)),
            CI_90 = SE * qt(.95, length(DE_12) - 1),
            lower = mean(DE_12)-CI_90,
            upper = mean(DE_12)+CI_90,
            DE_12 = mean(DE_12))

# plot_list$DE_violin_baseline_blocks <-
  ggplot(data = complete_table,
            aes(x = main_condition, y = DE_12)) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_hline(aes(yintercept = -.78), alpha = .5, color = 'darkred') +
  geom_point(position = position_jitter(width = .1), alpha = .5) +
  geom_violin(alpha = .6, fill = 'skyblue4') +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.1, color = 'red') +
  geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                data = confidence_intervalls,
              color = 'red', width = .05) +
  ggtitle('DE in Blocks 1 & 2', subtitle = '(Baseline Blocks)') +
  labs(x = 'Condition', y = 'Disposition Effect') +
  scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

ggplotly(plot_list$DE_violin_baseline_blocks)}
# Main study DE in Block 1 & 2 relative  to benchmark:
{
  confidence_intervalls <- complete_table %>%
    group_by(main_condition) %>%
    summarise(SE = sd(DE_12)/sqrt(length(DE_12)),
              CI_90 = SE * qt(.95, length(DE_12) - 1),
              lower = mean(DE_12)-CI_90,
              upper = mean(DE_12)+CI_90,
              DE_12 = mean(DE_12))

  # plot_list$DE_violin_baseline_blocks <-
  ggplot(data = complete_table,
         aes(x = main_condition, y = DE_12 + .78)) +
    geom_hline(aes(yintercept = 0), alpha = .5) +
    geom_point(position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.1, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower + .78, ymax = upper + .78),
                  data = confidence_intervalls,
                  color = 'red', width = .05) +
    ggtitle('DE relative to Benchmark', subtitle = 'Blocks 1 & 2 (Baseline Blocks)') +
    labs(x = 'Condition', y = 'DE from Benchmark') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  ggplotly()}

# Main study DE in Block 3 & 4:
{  confidence_intervalls <- complete_table %>%
  group_by(main_condition) %>%
  summarise(SE = sd(DE_34)/sqrt(length(DE_34)),
            CI_90 = SE * qt(.95, length(DE_34)),
            lower = mean(DE_34)-CI_90,
            upper = mean(DE_34)+CI_90,
            DE_34 = mean(DE_34))

# p <-
  ggplot(data = complete_table,
            aes(x = main_condition, y = DE_34)) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_line(aes(x, y, group = 1),
            data = tibble(x = c(0, 2.5, 2.5, 3), y = c(-.5, -.5, 0, 0)),
            alpha = .75, color = 'darkred') +
  geom_point(position = position_jitter(width = .1), alpha = .5) +
  geom_violin(alpha = .6, fill = 'skyblue4') +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.1, color = 'red') +
  geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                data = confidence_intervalls,
              color = 'red', width = .05) +
  ggtitle('DE in Blocks 3 & 4', subtitle = '(Treatment Blocks)') +
  labs(x = 'Condition', y = 'Disposition Effect') +
  scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

  ggplotly()}
# Main study DE in Block 3 & 4 relative to benchmark:
{  confidence_intervalls <- complete_table %>%
  group_by(main_condition) %>%
  mutate(DE_34 = ifelse(main_condition == 'states_shown', DE_34, DE_34 + .78)) %>%
  summarise(SE = sd(DE_34)/sqrt(length(DE_34)),
            CI_90 = SE * qt(.95, length(DE_34)),
            lower = mean(DE_34)-CI_90,
            upper = mean(DE_34)+CI_90,
            DE_34 = mean(DE_34))

# p <-
  ggplot(data = complete_table,
            aes(x = main_condition,
                y = ifelse(main_condition == 'states_shown', DE_34, DE_34 + .78))) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_point(position = position_jitter(width = .1), alpha = .5) +
  geom_violin(alpha = .6, fill = 'skyblue4') +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.1, color = 'red') +
  geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                data = confidence_intervalls,
              color = 'red', width = .05) +
  ggtitle('DE in Blocks 3 & 4', subtitle = '(Treatment Blocks)') +
  labs(x = 'Condition', y = 'Disposition Effect') +
  scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
  theme_minimal() +
  theme(text = element_text(size = text_size))

  ggplotly()}


# Main Study gender difference:
CIs <- complete_table %>%
  group_by(gender) %>%
  summarise(SE = sd(DE_12)/sqrt(length(DE_12)),
            lower = mean(DE_12) - SE * qt(.95, length(DE_12)),
            upper = mean(DE_12) + SE * qt(.95, length(DE_12)),
            DE_12 = mean(DE_12))

ggplotly(ggplot(complete_table, aes(x = gender, y = DE_12, fill = gender)) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_hline(aes(yintercept = -.78), alpha = .78, color = 'darkred') +
  geom_point(position = position_jitter(width = .1), alpha = .5) +
  geom_violin(alpha = .6) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.1, color = 'darkred') +
  geom_errorbar(aes(ymin = lower, ymax = upper), data = CIs,
                color = 'red', width = .05) +
  scale_fill_manual(values = c('skyblue4', 'cyan3')) +
  labs(x = 'Gender', y = 'DE') +
  ggtitle('Gender Difference in DE') +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = 'none'))


# Batch effects:
CIs <- complete_table %>%
  group_by(session_code) %>%
  summarise(SE = sd(de_12)/sqrt(length(de_12)),
            lower = mean(de_12) - SE * qt(.95, length(de_12)),
            upper = mean(de_12) + SE * qt(.95, length(de_12)),
            de_12 = mean(de_12))

ggplotly(ggplot(complete_table, aes(x = session_code, y = de_12)) +
  geom_hline(aes(yintercept = 0), alpha = .5) +
  geom_point(position = position_jitter(width = .1), alpha = .5) +
  geom_violin(alpha = .6, fill = 'skyblue4') +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = 0.1, color = 'darkred') +
  geom_errorbar(aes(ymin = lower, ymax = upper), data = CIs,
                color = 'red', width = .05) +
  labs(x = 'Session', y = 'DE Difference between Blocks') +
  ggtitle('DE differences over Sessions') +
  theme_minimal() +
  theme(text = element_text(size = 14), legend.position = 'none'))
}

# PGR and PLR
{
  # PGR in Block 1 & 2
  confidence_intervalls <- complete_table %>%
    group_by(main_condition) %>%
    summarise(SE = sd(pgr_12) / sqrt(length(pgr_12)),
              CI_90 = SE * qt(.95, length(pgr_12) - 1),
              lower = mean(pgr_12) - CI_90,
              upper = mean(pgr_12) + CI_90,
              pgr_12 = mean(pgr_12))

  ggplot(data = complete_table,
         aes(x = main_condition, y = pgr_12)) +
    geom_point(position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.1, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                  data = confidence_intervalls,
                  color = 'red', width = .05) +
    ggtitle('PGR in Blocks 1 & 2', subtitle = '(Baseline Blocks)') +
    labs(x = 'Condition', y = 'Proportions of Gains Realised') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  # PLR in Block 1 & 2
  confidence_intervalls <- complete_table %>%
    group_by(main_condition) %>%
    summarise(SE = sd(pgr_12) / sqrt(length(plr_12)),
              CI_90 = SE * qt(.95, length(plr_12) - 1),
              lower = mean(plr_12) - CI_90,
              upper = mean(plr_12) + CI_90,
              plr_12 = mean(plr_12))

  ggplot(data = complete_table,
         aes(x = main_condition, y = plr_12)) +
    geom_point(position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.1, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                  data = confidence_intervalls,
                  color = 'red', width = .05) +
    ggtitle('PLR in Blocks 1 & 2', subtitle = '(Baseline Blocks)') +
    labs(x = 'Condition', y = 'Proportions of Losses Realised') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  # Difference in PGR between blocks 1 & 2 and 3 & 4
  confidence_intervalls <- complete_table %>%
    group_by(main_condition) %>%
    summarise(SE = sd(pgr_12) / sqrt(length(pgr_diff_12_34)),
              CI_90 = SE * qt(.95, length(pgr_diff_12_34) - 1),
              lower = mean(pgr_diff_12_34) - CI_90,
              upper = mean(pgr_diff_12_34) + CI_90,
              pgr_diff_12_34 = mean(pgr_diff_12_34))

  ggplot(data = complete_table,
         aes(x = main_condition, y = pgr_diff_12_34)) +
    geom_point(position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.1, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                  data = confidence_intervalls,
                  color = 'red', width = .05) +
    ggtitle('Difference in PGR between Blocks 1 & 2 and 3 & 4') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  # Difference in PLR between blocks 1&2 and 3 & 4
  confidence_intervalls <- complete_table %>%
    group_by(main_condition) %>%
    summarise(SE = sd(plr_12) / sqrt(length(plr_diff_12_34)),
              CI_90 = SE * qt(.95, length(plr_diff_12_34) - 1),
              lower = mean(plr_diff_12_34) - CI_90,
              upper = mean(plr_diff_12_34) + CI_90,
              plr_diff_12_34 = mean(plr_diff_12_34))

  ggplot(data = complete_table,
         aes(x = main_condition, y = plr_diff_12_34)) +
    geom_point(position = position_jitter(width = .1), alpha = .5) +
    geom_violin(alpha = .6, fill = 'skyblue4') +
    stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                 geom = "crossbar", width = 0.1, color = 'red') +
    geom_errorbar(aes(x = main_condition, ymin = lower, ymax = upper),
                  data = confidence_intervalls,
                  color = 'red', width = .05) +
    ggtitle('Difference in PLR between Blocks 1 & 2 and 3 & 4') +
    labs(x = 'Condition', y = 'Diff. in Prop. of Losses Realised') +
    scale_x_discrete(labels = c('Baseline', 'Probabilities shown', 'States shown')) +
    theme_minimal() +
    theme(text = element_text(size = text_size))
}
