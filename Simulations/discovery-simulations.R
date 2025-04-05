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



## function to generate new target name

get_next_target <- function(current_targets) {
  last_target <- tail(current_targets, 1)
  last_letter <- substr(last_target, nchar(last_target), nchar(last_target))
  next_letter <- letters[which(letters == last_letter) + 1]
  next_target <- paste0("target_", next_letter)
  return(next_target)
}


for(simulation in 1:400){
  print(paste('<<<<<<<<<< SIMULATION', simulation, '>>>>>>>>>>')) ## Run the simulation for all cost shares i times
  
  pi_new <- 0.1  # Lower pi for new approaches
  p_new <- 0.3
  
  pi_a = 0.4
  pi_b = 0.2
  high_c <- 0.05 
  low_c <- 0.025
  p_a <- 0.3
  p_b <- 0.3
  number_of_firms <- 50
  cost_share_sequence <- seq(0, 1, 0.2)
  
  max_periods <- 50
  internal_sim_count <- 50
  
  discovery_seq <- seq(0, 0.1, 0.05)
  
  full_sim_result <- data.frame()
  df_targets_base <- data.frame(target = c('target_a', 'target_b'), p = c(p_a, p_b), pi = c(pi_a, pi_b), entries = 0)
  
  
  for(high_cost_share in cost_share_sequence){
    print(paste('--------- HIGH COST SHARE =', high_cost_share, '---------'))
    
    for(discovery_pr in discovery_seq){
      print(paste('--------- DISCOVERY PROB =', discovery_pr, '---------'))
      firm_names <- paste0("firm_", 1:number_of_firms)
      df_T0 <- data.frame(firm = firm_names)
      
      low_count <- round((1 - high_cost_share) * number_of_firms)
      high_count <- number_of_firms - low_count 
      
      df_T0$cost <- c(rep(low_c, low_count), rep(high_c, high_count))
      
      
      RESULTS <- data.frame(period = 1:max_periods,
                            firm = NA,
                            firm_cost = NA,
                            firm_type = NA,
                            max_payoff = NA,
                            target = NA)
      
      df_targets <- df_targets_base ## make sure we only start with approach a and b
      t = 1
      consecutive_no_entry <- 0
      
      while(consecutive_no_entry < 1 & t < max_periods){
        print(paste('beginning iteration', t))
        
        
        ## DISCOVERY
        if(runif(1) < discovery_pr & t > 1){
          print('discovery!')
          new_target <- get_next_target(df_targets$target)
          df_targets <- rbind(df_targets, c(new_target, p_new, pi_new, 0))
          
          
          RESULTS$firm[t] <- 'Discovery'
          RESULTS$firm_type[t] <- 'Discovery' 
          RESULTS$firm_cost[t] <- 'Discovery'
          RESULTS$target[t] <- new_target
          
          df_targets$p <- as.numeric(df_targets$p)
          df_targets$pi <- as.numeric(df_targets$pi)
          
          t <- t +1 
          next
        }
        
        ##
        
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
          
          for(i in 1:internal_sim_count){
            
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
            df_targets$entries[df_targets$target == chosen_target] <- as.numeric(df_targets$entries[df_targets$target == chosen_target]) + 1
            consecutive_no_entry <- 0
          } else{
            if(is.na(chosen_target) & trial_firms_cost == min(df_T0$cost)){
              consecutive_no_entry <- consecutive_no_entry + 1}
          }
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
      RESULTS$discovery_pr <- discovery_pr
      
      RESULTS <- RESULTS %>% filter(!is.na(firm))
      
      full_sim_result <- rbind(full_sim_result, RESULTS)
    }
    
  }
  
  full_sim_result$sim_number <- simulation
  
  write.csv(full_sim_result, paste0('/Users/williammiles/Dropbox/Intellectual Outliers/Derived/simulations/nov-24/sim-discovery-v3/', simulation, '.csv'))
  print('saved')
  
}

##########################################################################################
##########################################################################################

## OPTIONAL -- load provided simulation data here -->>

DISCOVERY_SIMS <- read.csv('Simulations/discovery-simulation-results.csv')

DISCOVERY_SIMS <- DISCOVERY_SIMS %>% 
  mutate(market = paste0(discovery_pr, '-', high_cost_share, '-', sim_number))


##--------------------------------------------------------------------------------
#
# Figure C.2 Simulation Results with DISCOVERY
#
##--------------------------------------------------------------------------------

## (a)

DISCOVERY_SIMS %>% 
  filter(!is.na(target)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share, firm) %>% 
  mutate(n_exp = n(),
         n_exp = ifelse(firm == 'Discovery', NA, n_exp)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share) %>% 
  summarize(
    mean_share = mean(n_exp, na.rm=TRUE),
    entropy = -sum((table(target) / n()) * log2(table(target) / n()), na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = mean_share-1, y = entropy, col = as.factor(discovery_pr), shape = as.factor(discovery_pr), lty = as.factor(discovery_pr))) +
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.1, se = TRUE) +
  labs(x = "Share of Multi-Experimenters", y = "Shannon Entropy", shape = 'Pr(Discovery)', col = 'Pr(Discovery)', lty = 'Pr(Discovery)') +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_shape_manual(values = c(2, 4, 20))+
  scale_color_manual(values = c("darkblue", "purple", "lightblue")) +
  theme_pubclean() +
  theme(legend.position = 'bottom')+
  theme(text = element_text(size = 6)) -> sim_discovery_shannon

ggsave("Simulations/sim_discovery_shannon.pdf",
       sim_discovery_shannon,
       height = 2, 
       width = 2)

## (b)

DISCOVERY_SIMS %>% 
  filter(!is.na(target)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share, firm) %>% 
  mutate(n_exp = n(),
         n_exp = ifelse(firm == 'Discovery', NA, n_exp)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share) %>% 
  summarize(
    mean_share = mean(n_exp, na.rm=TRUE),
    entropy = -sum((table(target) / n()) * log2(table(target) / n()), na.rm = TRUE),
    atleastone = ifelse(max(success) == 1, 1, 0),
  ) %>% 
  filter(mean_share < 1.9) %>% ##  for illustration purposes
  ggplot(aes(x = mean_share-1, y = atleastone, col = as.factor(discovery_pr), shape = as.factor(discovery_pr), lty = as.factor(discovery_pr))) +
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.1, se = TRUE) +
  labs(x = "Share of Multi-Experimenters", y = "Pr(At least one success)", shape = 'Pr(Discovery)', col = 'Pr(Discovery)', lty = 'Pr(Discovery)') +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_shape_manual(values = c(2, 4, 20))+
  scale_color_manual(values = c("darkblue", "purple", "lightblue")) +
  theme_pubclean() +
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 6)) -> sim_discovery_alo_es

ggsave("Simulations/sim_discovery_atleastone_exp_share.pdf",
       sim_discovery_alo_es,
       height = 2, 
       width = 2)

## (c)

DISCOVERY_SIMS %>% 
  filter(!is.na(target)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share, firm) %>% 
  mutate(n_exp = n(),
         n_exp = ifelse(firm == 'Discovery', NA, n_exp)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share) %>% 
  summarize(
    mean_share = mean(n_exp, na.rm=TRUE),
    entropy = -sum((table(target) / n()) * log2(table(target) / n()), na.rm = TRUE),
    atleastone = ifelse(max(success) == 1, 1, 0),
  ) %>% 
  ggplot(aes(x = entropy, y = atleastone, col = as.factor(discovery_pr), shape = as.factor(discovery_pr), lty = as.factor(discovery_pr))) +
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.1, se = TRUE) +
  labs(x = "Shannon Entropy", y = "Pr(At least one success)", shape = 'Pr(Discovery)', col = 'Pr(Discovery)', lty = 'Pr(Discovery)') +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_shape_manual(values = c(2, 4, 20))+
  scale_color_manual(values = c("darkblue", "purple", "lightblue")) +
  theme_pubclean() +
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 6)) -> sim_discovery_alo_div

ggsave("Simulations/sim_discovery_atleastone_diversity.pdf",
       sim_discovery_alo_div,
       height = 2, 
       width = 2)

## (d)

DISCOVERY_SIMS %>% 
  filter(!is.na(target)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share, firm) %>% 
  mutate(n_exp = n(),
         n_exp = ifelse(firm == 'Discovery', NA, n_exp)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share) %>% 
  summarize(
    mean_share = mean(n_exp, na.rm=TRUE),
    entropy = -sum((table(target) / n()) * log2(table(target) / n()), na.rm = TRUE),
    success_share = mean(success),
  ) %>% 
  filter(mean_share <1.9) %>% 
  ggplot(aes(x = mean_share-1, y = success_share, col = as.factor(discovery_pr), shape = as.factor(discovery_pr), lty = as.factor(discovery_pr))) +
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.1, se = TRUE) +
  labs(x = "Share of Multi-Experimenters", y = "Success Share", shape = 'Pr(Discovery)', col = 'Pr(Discovery)', lty = 'Pr(Discovery)') +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_shape_manual(values = c(2, 4, 20))+
  scale_color_manual(values = c("darkblue", "purple", "lightblue")) +
  theme_pubclean() +
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 6)) -> sim_discovery_ss_es

ggsave("Simulations/sim_discovery_success_share_exp_share.pdf",
       sim_discovery_ss_es,
       height = 2, 
       width = 2)

## (e)

DISCOVERY_SIMS %>% 
  filter(!is.na(target)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share, firm) %>% 
  mutate(n_exp = n(),
         n_exp = ifelse(firm == 'Discovery', NA, n_exp)) %>% 
  group_by(sim_number, discovery_pr, high_cost_share) %>% 
  summarize(
    mean_share = mean(n_exp, na.rm=TRUE),
    entropy = -sum((table(target) / n()) * log2(table(target) / n()), na.rm = TRUE),
    success_share = mean(success),
  ) %>% 
  ggplot(aes(x = entropy, y = success_share, col = as.factor(discovery_pr), shape = as.factor(discovery_pr), lty = as.factor(discovery_pr))) +
  geom_smooth(method = 'loess', size = 0.5, alpha = 0.1, se = TRUE) +
  labs(x = "Shannon Entropy", y = "Success Share", shape = 'Pr(Discovery)', col = 'Pr(Discovery)', lty = 'Pr(Discovery)') +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, NA)) +
  scale_shape_manual(values = c(2, 4, 20))+
  scale_color_manual(values = c("darkblue", "purple", "lightblue")) +
  theme_pubclean() +
  theme(legend.position = 'bottom')+
  theme(text = element_text(size = 6)) -> sim_discovery_ss_div

ggsave("Simulations/sim_discovery_success_share_diversity.pdf.pdf",
       sim_discovery_ss_div,
       height = 2, 
       width = 2)

