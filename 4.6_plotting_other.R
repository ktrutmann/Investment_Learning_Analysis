# TODO: Compare beliefs in treatment vs. baseline group

library(tidyverse)
library(plotly)
library(grid)
library(ggtext)

theme_set(theme_minimal())

text_size <- 14

# Loading data
# Getting the most recent one
all_complete_tables <- str_subset(dir('Output/Tables/'), 'complete')
most_recent <- max(str_extract(all_complete_tables, '\\d{4}-\\d{2}-\\d{2}'))

complete_table <- read_delim(str_c('Output//Tables//',
                                   str_subset(all_complete_tables, most_recent))[1], delim = ';')

dat_main_long <- read_delim('..//Data//Clean//all_participants_long_main_main_study.csv',
                            delim = ';')

# Density --------------------------------------------------------------

  # Belief update Bayesian and data
  ggplot(filter(dat_main_long,
                i_block < 2,
                position_laged %in% c('Gain', 'Loss')) %>%
                  dplyr::select(belief_diff_from_last, bayes_diff_from_last) %>%
                  gather(key = 'Source', value = 'Update') %>%
                  mutate(Update = Update / 100),
         aes(x = Update, fill = Source)) +
  geom_density(alpha = .35, bw = .005) +
  scale_fill_manual(values =  c('cyan3', 'skyblue4'),
                    labels = c('Bayesian\nUpdating', 'Data')) +
  scale_y_continuous(trans = 'sqrt') +
  ylab('Square root of Density') +
  ggtitle('Belief Updating Density') +
  theme_minimal() +
  theme(text = element_text(size = text_size))
  ggplotly()

  # Belief by holding
  ggplot(dat_main_long %>%
           filter(i_block < 2, !is.na(hold_lead)),
         aes(x = belief / 100, fill = hold_lead)) +
    geom_density(alpha = .35, bw = .05) +
    scale_fill_manual(labels = c('Short', 'Not Invested', 'Hold'),
                       values = c('cyan3', 'skyblue4', 'blue4')) +
    labs(x = 'Reported Beliefs', y = 'Density', fill = 'Next Position',
         title = 'Density of Beliefs by Next Position') +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  # Beliefs by holding in the next period
  ggplot(dat_main_long %>%
           filter(main_condition != 'baseline' &
                    i_block == 0 & !is.na(hold_lead)),
         aes(x = belief, color = position)) +
    geom_line(stat = 'bin', binwidth = 10, size = 1) +
    labs(x = 'Belief', y = 'Frequency', fill = 'Current Position') +
    facet_grid(cols = vars(as.factor(hold_lead))) +
    theme_minimal() +
    theme(panel.spacing.x = unit(2, 'lines')) +
    scale_fill_manual(values = c('cyan3', 'skyblue4', 'blue1', 'black'))
    ggplotly()

  # Density-plot of relative updating:
  # Note: The variable follows a Gauchy-distribution (Actually just a ratio-distribution).
  # The center 24% provides the best estimate for the actual mean (Bloch, 2012
  # in the Journal of the Americal Stat. Assoc.)
  if (FALSE) {
    # plot_list$updating_rel_to_bayes_density <-
      ggplot(filter(dat_main_long,
                    main_condition == 'baseline', i_block < 2,
                    position_laged %in% c('Gain', 'Loss'),
                    belief_diff_rel_to_bayes < 25, belief_diff_rel_to_bayes > -25),
             aes(x = belief_diff_rel_to_bayes,
                 fill = interaction(price_move_from_last_corrected, position_laged))) +
      geom_density(alpha = .35, bw = .5) +
      geom_vline(aes(xintercept = quantile(belief_diff_rel_to_bayes, .38)), alpha = .5) +
      geom_vline(aes(xintercept = quantile(belief_diff_rel_to_bayes, .76)), alpha = .5) +
      scale_fill_manual(values = c('cyan2', 'skyblue4', 'cyan4', 'grey4')) +
      ggtitle('Belief Updates in Block 1 & 2', subtitle = ' (Baseline Blocks)') +
      labs(x = 'Mean Relative Belief Update', y = 'Density', fill = 'Situation') +
      theme_minimal() +
      theme(text = element_text(size = text_size))

    ggplotly(plot_list$updating_rel_to_bayes_density)
  }


# Other -------------------------------------------------------------------
  # Test whether DE measures are normally distributed:
  qqnorm(complete_table$DE_12)
  qqline(complete_table$DE_12)
  # Fat tails, but mostly yes!

  if (FALSE) { # To prevent this from being sourced
    # Boxplot of relative belief updating:
    ggplot(filter(dat_main_long,
                  main_condition == 'baseline', i_block < 2, position_laged != 'Not Invested',
                  belief_diff_rel_to_bayes < 20, belief_diff_rel_to_bayes > -20),
           aes(x = price_move_from_last_corrected, y = belief_diff_rel_to_bayes,
               fill = price_move_from_last_corrected)) +
      facet_grid(cols = vars(position_laged)) +
      geom_boxplot() +
      scale_fill_manual(values = c('cyan3', 'skyblue4'),
                        labels = c('Last Position Gain', 'Last Position Loss')) +
      ggtitle('Belief Updates in Block 1 & 2', subtitle = ' (Baseline Blocks)') +
      labs(x = 'Price Movement', y = 'Mean Relative Belief Update', fill = 'Position') +
      theme_minimal()


    # Compare rational to Willingnes to Pay
    ggplot(drop_na(dat_main_long, rational_belief, belief, condition),
           aes(x = rational_belief, y = belief/10, color = condition)) +
        geom_point(alpha = .15) +
        geom_smooth(method = 'glm',
                    method.args = list(family = 'binomial')) +
        labs(x = 'Rational Benchmark Belief', y = 'Willingnes to Pay', color = 'Condition') +
        scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
                           values = c('blue1', 'cyan3', 'skyblue4')) +
        theme_minimal()

}
# Saving Plotlist ---------------------------------------------------------
  saveRDS(plot_list, 'plot_list.Rdat')
  m2_newer = ((p1*(m1_new ^ alpha))/p2)^(1/alpha)
