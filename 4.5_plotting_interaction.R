# Interaction Plots -------------------------------------------------------
# Note: The simulated rational baseline for p_switch = .2 and p_up_good = .75 is -.45.

general_specs <- list(theme_minimal(),
                 labs(color = 'Condition', x = 'Block'),
                 scale_x_discrete(labels=1:2),
                 scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
                                   values = c('blue1', 'cyan3', 'skyblue4')),
                 theme(text = element_text(size = text_size)))


# DE per condition per Block
DE_raw <- complete_table %>%
    dplyr::select(DE_12, DE_34, main_condition, participant_code) %>%
    gather(key = block, value = DE, -main_condition, -participant_code) %>%
    filter(!is.na(main_condition))

DE_gathered <- DE_raw %>%
    group_by(block, main_condition) %>%
    summarise(mean_DE = mean(DE),
              n = n(),
              sd_DE = sd(DE),
              se_DE = sd(DE)/sqrt(length(DE)),
              width_95 = se_DE * qt(.95, length(DE)),
              lower = mean_DE - width_95,
              upper = mean_DE + width_95)

ggplot(data = DE_gathered, aes(x = block, y = mean_DE, color = main_condition)) +
    geom_line(aes(group=main_condition), position = position_dodge(.05), size = 1) +
    geom_point(position = position_dodge(.05)) +
    geom_errorbar(aes(ymin=lower, ymax = upper), width = 0,
                  position = position_dodge(.05)) +
    geom_line(data = DE_raw, aes(x = block, y = DE, color = main_condition, group = participant_code),
              alpha = .2) +
    ylab('Disposition Effect') +
    general_specs

ggplotly()


# DE per condition per Block, corrected for baseline
DE_gathered_baselined <- DE_gathered %>%
    mutate(mean_DE_baselined = ifelse(main_condition == 'states_shown' & block == 'DE_34',
                                      mean_DE, mean_DE + .45))

p <- ggplot(data = DE_gathered_baselined,
            aes(x = block, y = mean_DE_baselined, color = main_condition)) +
    geom_line(aes(group=main_condition), position = position_dodge(.05), size = 1) +
    geom_point(position = position_dodge(.05)) +
    geom_errorbar(aes(ymin=mean_DE_baselined - width_95, ymax = mean_DE_baselined + width_95),
                  width = 0, position = position_dodge(.05)) +
    # geom_line(data = DE_raw, aes(x = block, y = DE, color = main_condition, group = participant_code),
    #           alpha = .2) +
    ylab('Disposition Effect (Relative to Baseline)') +
    general_specs

ggplotly(p)


if(FALSE){ # To prevent sourcing
  # DE per field per Block
  DE_gathered <- complete_table %>%
      dplyr::select(DE_0, DE_1, study_field_coarse, main_condition) %>%
      gather(key = block, value = DE, -study_field_coarse, -main_condition) %>%
      filter(!is.na(main_condition)) %>%
      group_by(block, study_field_coarse) %>%
      summarise(mean_DE = mean(DE, na.rm = TRUE),
                se_DE = sd(DE)/sqrt(length(DE)))

  ggplot(data = DE_gathered, aes(x = block, y=mean_DE, color = study_field_coarse)) +
      geom_line(aes(group=study_field_coarse), position = position_dodge(.05), size = 1) +
      geom_point(position = position_dodge(.05)) +
      geom_errorbar(aes(ymin=mean_DE - se_DE, ymax = mean_DE + se_DE), width = 0,
                    position = position_dodge(.05)) +
      theme_minimal() +
      labs(color = 'Field', x = 'Block', y = 'DE') +
      scale_x_discrete(labels=1:2)
      scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'))


  # Same for PGR and PLR

  reals_gathered <- complete_table %>%
      dplyr::select(PGR_0, PGR_1, PLR_0, PLR_1, main_condition) %>%
      gather(key = block, value = reals, -main_condition) %>%
      filter(!is.na(main_condition))

  reals_gathered$returns <- ifelse(str_detect(reals_gathered$block, 'PLR'), 'Loss', 'Gain')
  reals_gathered$block <- str_remove_all(reals_gathered$block, '\\D')

  reals_gathered <- reals_gathered %>%
      group_by(block, main_condition, returns) %>%
      summarise(mean_reals = mean(reals, na.rm = TRUE),
                se_reals = sd(reals)/sqrt(length(reals)),
                n_reals = length(reals))

  ggplot(data = reals_gathered, aes(x = block, y = mean_reals,
                                    group = interaction(main_condition, returns))) +
      geom_line(aes(color = main_condition, linetype = returns),
                position = position_dodge(.08), size = 1) +
      geom_point(aes(color = main_condition), position = position_dodge(.08)) +
      geom_errorbar(aes(color = main_condition, linetype = returns, ymin=mean_reals - se_reals,
                        ymax = mean_reals + se_reals), width = 0, position = position_dodge(.08)) +
      ylab('Proportion Realised') +
      labs(linetype = 'Return') +
      general_specs


  #   Separate
  ggplot(data = filter(reals_gathered, returns == 'Gain'),
         aes(x = block, y = mean_reals, group = main_condition)) +
      geom_line(aes(color = main_condition), position = position_dodge(.05), size = 1) +
      geom_point(aes(color = main_condition), position = position_dodge(.05)) +
      geom_errorbar(aes(color = main_condition, ymin=mean_reals - se_reals,
                        ymax = mean_reals + se_reals),
                    width = .1, position = position_dodge(.05)) +
      ylab('Proportion Gains Realised') +
      labs(linetype = 'Return') +
      general_specs

  ggplot(data = filter(reals_gathered, returns == 'Loss'),
         aes(x = block, y = mean_reals, group = main_condition)) +
      geom_line(aes(color = main_condition), position = position_dodge(.05), size = 1) +
      geom_point(aes(color = main_condition), position = position_dodge(.05)) +
      geom_errorbar(aes(color = main_condition, ymin=mean_reals - se_reals,
                        ymax = mean_reals + se_reals),
                    width = .1, position = position_dodge(.05)) +
      ylab('Proportion Losses Realised') +
      labs(linetype = 'Return') +
      general_specs

  # RT per condition per Block
  RT_gathered <- complete_table %>%
      dplyr::select(mean_RT_0, mean_RT_1, main_condition) %>%
      gather(key = block, value = RT, -main_condition) %>%
      filter(!is.na(main_condition)) %>%
      group_by(block, main_condition) %>%
      summarise(mean_RT = mean(RT, na.rm = TRUE),
                se_RT = sd(RT) / sqrt(length(RT)))

  ggplot(data = RT_gathered, aes(x = block, y = mean_RT,
      color = main_condition)) +
      geom_line(aes(group = main_condition), position = position_dodge(.05), size = 1) +
      geom_point(position = position_dodge(.05)) +
      geom_errorbar(aes(ymin = mean_RT - se_RT, ymax = mean_RT + se_RT),
        width = 0, position = position_dodge(.05)) +
      ylab('Decision Time') +
      general_specs


  # Strategy profiles per condition
  strats_raw <- complete_table %>%
      dplyr::select(starts_with('strategy_'), main_condition) %>%
      gather(key = strategy, value = value, -main_condition) %>%
      na.omit()

  strats_gathered <- strats_raw %>%
      group_by(strategy, main_condition) %>%
      summarise(mean_strategy = mean(value, na.rm = TRUE),
                se_strategy = sd(value) / sqrt(length(value)))


  ggplot(data = strats_gathered, aes(x = strategy, y = mean_strategy,
      color = main_condition)) +
      geom_line(aes(group = main_condition), position = position_dodge(.05), size = 1) +
      geom_point(position = position_dodge(.05)) +
      geom_errorbar(aes(ymin = mean_strategy - se_strategy,
                        ymax = mean_strategy + se_strategy), width = 0,
                    position = position_dodge(.05)) +
      theme_minimal() +
      labs(color = 'Condition') +
      ylab('Value') +
      scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
                     values = c('blue1', 'cyan3', 'skyblue4')) +
      geom_point(data = strats_raw, aes(x = strategy, y = value, color = main_condition),
                 position = position_jitter(.1, .05), alpha = .3)
}
