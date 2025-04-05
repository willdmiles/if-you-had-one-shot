library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

rm(list = ls())

## first set working directory
setwd('~/Dropbox/Intellectual Outliers/NBER-wp/')


## create a folder to store simualtion results
sim_results_folder <- 'REPLACE-WITH-YOUR-FOLDER-PATH'

## NOTE: 

## these simulations are slow. You may want to begin by running a smaller number of simulations i.e., not 400.
## OR, you can skip to below the simulation code, and run the code to produce the figures by loading the provided simulation results file



## BASLINE SIMULATION

for(simulation in 1:400){
  print(paste('<<<<<<<<<< SIMULATION', simulation, '>>>>>>>>>>')) ## Run the simulation for all cost shares i times
  
  pi_a = 0.4
  pi_b = 0.2
  high_c <- 0.05 
  low_c <- 0.025
  p_a <- 0.3
  p_b <- 0.3
  number_of_firms <- 50
  cost_share_sequence <- seq(0, 1, 0.1)
  
  full_sim_result <- data.frame()
  df_targets <- data.frame(target = c('target_a', 'target_b'), p = c(p_a, p_b), pi = c(pi_a, pi_b), entries = 0)
  
  
  for(high_cost_share in cost_share_sequence){
    print(paste('--------- HIGH COST SHARE =', high_cost_share, '---------'))
    firm_names <- paste0("firm_", 1:number_of_firms)
    df_T0 <- data.frame(firm = firm_names)
    
    low_count <- round((1 - high_cost_share) * number_of_firms)
    high_count <- number_of_firms - low_count 
    
    df_T0$cost <- c(rep(low_c, low_count), rep(high_c, high_count))
    
    
    RESULTS <- data.frame(period = 1:150,
                          firm = NA,
                          firm_cost = NA,
                          firm_type = NA,
                          max_payoff = NA,
                          target = NA)
    
    t = 1
    consecutive_no_entry <- 0
    
    while(consecutive_no_entry < 10){
      print(paste('beginning iteration', t))
      
      ## pick a firm
      random_row_index <- sample(1:nrow(df_T0), 1)
      trial_firm <- df_T0[random_row_index, ]$firm
      
      trial_firms_cost <- df_T0[random_row_index, ]$cost
      
      ## has this firm already ENTERED with a trial?
      firms_w_trials <- RESULTS %>% filter(!is.na(target)) 
      incumbent <- trial_firm %in% firms_w_trials$firm
      
      if(t == 1){
        
        RESULTS$firm[t] <- trial_firm
        RESULTS$firm_type[t] <- 'Entrant' 
        RESULTS$firm_cost[t] <- trial_firms_cost
        RESULTS$target[t] <- 'target_a'
        
      } else{
        
        payoff_df <- data.frame(target = character(), payoff = numeric(), stringsAsFactors = FALSE)
        
        for(i in 1:150){
          
          ## first need to draw for pi for each approach in n_targets
          
          sim_targets <- df_targets %>% 
            rowwise() %>% 
            mutate(expected_pi = rbinom(1, 1, prob = pi)) %>% 
            ungroup()
          
          ##
          sim_df <- left_join(RESULTS, sim_targets, by = 'target') %>%
            filter(period < t) %>% 
            filter(!is.na(target)) %>% 
            rowwise() %>% 
            mutate(expected_pr = rbinom(1, 1, prob = p), success = expected_pr * expected_pi) %>% 
            ungroup()
          
          ##  calculate number of successes
          total_success <- sum(sim_df$success, na.rm=TRUE)
          
          ## incumbents success
          firm_success <- max(0, sum(sim_df$success[sim_df$firm == trial_firm], na.rm=TRUE))
          
          
          if(!incumbent){ ## firm is new
            for(target in df_targets$target){
              firm_p <- rbinom(1, 1, sim_targets$p[sim_targets$target == target])
              firm_has_success <- firm_p * sim_targets$expected_pi[sim_targets$target == target]
              
              ## need to consider cost
              expected_payoff <- ifelse(firm_has_success == 0, 0, ifelse(total_success == 0, 1, 1/(total_success+1)))  - trial_firms_cost
              
              ## store
              payoff_df <- rbind(payoff_df, data.frame(target = target, payoff = expected_payoff, stringsAsFactors = FALSE))
              
            }
          } 
          else{ ## firm is incumbent
            for(target in df_targets$target){
              
              firm_p <- rbinom(1, 1, sim_targets$p[sim_targets$target == target])
              firm_has_success <- firm_p * sim_targets$expected_pi[sim_targets$target == target]
              
              ## need to consider cost
              ## firm_has_success is current firm's success in current period
              ## firm_success is current firm's success in previous trial
              ## total_success is all successful trials
              expected_payoff <- ifelse(firm_has_success == 0 & firm_success == 0, 0, ifelse(total_success == 0, 1, 1/(total_success+1)))  - trial_firms_cost
              ## store
              payoff_df <- rbind(payoff_df, data.frame(target = target, payoff = expected_payoff, stringsAsFactors = FALSE))
              
              
            }
          }
          
          
        }
        
        payoff_df$payoff <- as.numeric(payoff_df$payoff)
        av_payoff_df <- payoff_df %>% filter(!is.na(target)) %>% group_by(target) %>% summarize(av_payoff = mean(payoff)) %>% arrange(-av_payoff)
        
        max_payoff <- max(av_payoff_df$av_payoff)
        chosen_target <- ifelse(max_payoff > 0, av_payoff_df$target[1], NA)
        
        RESULTS$firm[t] <- trial_firm
        RESULTS$firm_type[t] <- ifelse(incumbent, 'Incumbent', 'Entrant')
        RESULTS$firm_cost[t] <- trial_firms_cost
        RESULTS$max_payoff[t] <- max_payoff
        RESULTS$target[t] <- chosen_target
        
        if(!is.na(chosen_target)){
          df_targets$entries[df_targets$target == chosen_target] <- df_targets$entries[df_targets$target == chosen_target] + 1
          consecutive_no_entry <- 0
        } else{consecutive_no_entry <- consecutive_no_entry + 1}
        
        ## IF THE FIRM NOW HAS 2 TRIAL ENTRIES, REMOVE THIS FIRM FROM THE LIST OF FIRMS
        if(nrow(RESULTS %>% filter(!is.na(target) & firm == trial_firm)) == 2){
          print('This firm has two entries')
          df_T0 <- df_T0 %>% filter(!firm %in% trial_firm)
        }
      }
      
      
      t <- t + 1
    }
    ## outcomes
    df_target_outcomes <- df_targets %>% 
      rowwise() %>% 
      mutate(realized_pi = rbinom(1, 1, pi)) %>% 
      select(target, realized_pi, p)
    
    RESULTS <- left_join(RESULTS, df_target_outcomes, by = 'target')
    
    RESULTS <- RESULTS %>% 
      rowwise() %>% 
      mutate(success = realized_pi * rbinom(1, 1, p))
    
    RESULTS$high_cost_share <- high_cost_share
    
    RESULTS <- RESULTS %>% filter(!is.na(firm))
    
    full_sim_result <- rbind(full_sim_result, RESULTS)
  }
  
  
  
  full_sim_result$sim_number <- simulation
  
  write.csv(full_sim_result, paste0(sim_results_folder,simulation, '.csv'))
  
}


##########################################################################################
##########################################################################################


## OPTIONAL -- load provided simulation data here -->>

ALL_SIM_RESULTS <- read.csv('Simulations/baseline-simulation-results.csv')


ALL_SIM_RESULTS <- ALL_SIM_RESULTS %>% 
  mutate(market = paste0(high_cost_share, '-', sim_number))


market_level_df <-
  ALL_SIM_RESULTS %>%
  filter(!is.na(target)) %>% 
  group_by(market, firm) %>% 
  mutate(n_exp = n()) %>% 
  group_by(market) %>% 
  summarize(n_a = sum(target == "target_a"),
            n_b = sum(target == "target_b"),
            n_entry = sum(n_a, n_b),
            share_a = n_a/n_entry,
            atleastone_success = ifelse(max(success) == 1, 1, 0),
            share_success = mean(success),
            n_firms = n_distinct(firm),
            n_multis = n_distinct(firm[n_exp == 2]),
            n_singles = n_distinct(firm[n_exp == 1]),
            multi_share = n_multis/n_firms)


##--------------------------------------------------------------------------------
#
# Figure C.1 Simulation Results
#
##--------------------------------------------------------------------------------

## (a)

market_level_df %>% 
  mutate(multi_share_bin = ntile(multi_share, 10)) %>%  # Divide av_scale into quantile-based bins
  group_by(multi_share_bin) %>%
  summarize(
    multi_share_binned = mean(multi_share, na.rm = TRUE),
    choice_binned = mean(1-share_a, na.rm = TRUE)
  ) %>%  
  ggplot(aes(x = multi_share_binned, y = choice_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, fill = 'orange4', alpha = 0.1, col = 'orange4') +
  theme_classic() +
  labs(x = 'Share of Multi-Experimenters', y = 'Diversity of Approaches') +
  theme(text = element_text(size = 8)) -> sims_choice

ggsave("Simulations/baseline-sims-diversity-exp-share.pdf", sims_choice, width = 2, height = 2)

## (b)

market_level_df %>% 
  mutate(multi_share_bin = ntile(multi_share, 10)) %>%  # Divide av_scale into quantile-based bins
  group_by(multi_share_bin) %>%
  summarize(
    multi_share_binned = mean(multi_share, na.rm = TRUE),
    success_share_binned = mean(share_success, na.rm = TRUE)
  ) %>%  
  ggplot(aes(x = multi_share_binned, y = success_share_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, fill = 'orange4', alpha = 0.1, col = 'orange4') +
  theme_classic() +
  labs(x = 'Share of Multi-Experimenters', y = 'Share of Success') +
  theme(text = element_text(size = 8)) -> sims_success_share

ggsave("Simulations/baseline-sims-success-share-exp-share.pdf", sims_success_share, width = 2, height = 2)


## (c)

market_level_df %>% 
  mutate(choice_bin = ntile(1-share_a, 10)) %>%  # Divide av_scale into quantile-based bins
  group_by(choice_bin) %>%
  summarize(
    choice_binned = mean(1-share_a, na.rm = TRUE),
    atleastone_binned = mean(atleastone_success, na.rm = TRUE)
  ) %>%  
  ggplot(aes(x = choice_binned, y = atleastone_binned)) +
  geom_point(shape = 4, size = 1) +
  geom_smooth(method = 'lm', size = 0.5, fill = 'orange4', alpha = 0.1, col = 'orange4') +
  theme_classic() +
  labs(x = 'Diversity of Approaches', y = 'Pr(At least one Success)') + 
  theme(text = element_text(size = 8)) -> sims_choice_to_atleastone_success


ggsave("Simulations/baseline-sims-atleastone-success-diversity.pdf", sims_choice_to_atleastone_success, width = 2, height = 2)

