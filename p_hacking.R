
library('tidyverse')
library('lme4')


#########################
# simulation parameters
#########################

# effect size (mean difference between groups / pooled standard deviation)
effect_size = 0
# initial sample size for each group
n = 20
# how many samples to add each time
n_step = 5
# max sample size for each group
n_max = 100
# p value cutoff
p_cutoff = .05


#########################
# run simulations
#########################

# define simulation
simulate_experiments = function() {
  
  # generate initial data points
  control_group = rnorm(n)
  experimental_group = rnorm(n, mean = effect_size)
  
  # run quick t-test
  p_current = t.test(control_group, experimental_group)$p.value
  
  # while p > p_cutoff and n < n_max, get more samples and re-test
  while (p_current > p_cutoff) {
    
    control_group = c(control_group, rnorm(n_step))
    experimental_group = c(experimental_group, rnorm(n_step, mean = effect_size))
    
    p_current = t.test(control_group, experimental_group)$p.value
    
    if (length(control_group) >= n_max) {
      break
    }
  }
  
  # single samples
  control_group_single = rnorm(length(control_group))
  experimental_group_single = rnorm(length(control_group), mean = effect_size)
  
  results = list()
  results$n_current = length(control_group)
  results$p_hack = p_current
  results$effect_hack = mean(experimental_group) - mean(control_group)
  results$p_single = t.test(control_group_single, experimental_group_single)$p.value
  results$effect_single = mean(experimental_group_single) - mean(control_group_single)
  
  return(results)
}

# run simulations
results_list = replicate(10000, simulate_experiments())

# convert list to dataframe
results = as_tibble(matrix(unlist(results_list), nrow = 10000, byrow = T))
colnames(results) = rownames(results_list)

# clean up
remove(results_list)


###########################
# data analysis interlude
###########################

if (1 != 1) {
  
  ####### non-parametric permutation test ###### 
  ####### useful when you can't assume the underlying distributions are normal ###### 
  
  scramble_it = function(experimental_group, control_group) {
    
    # scramble data
    data_scramble = sample(c(experimental_group, control_group))
    
    # pull out new groups
    experimental_scramble = data_scramble[1:length(experimental_group)]
    control_scramble = data_scramble[(length(experimental_group) + 1):length(data_scramble)]
    
    # calculate observed difference
    return(mean(experimental_scramble) - mean(control_scramble))
  }
  
  # generate initial data points
  control_group = rnorm(n)
  experimental_group = rnorm(n, mean = effect_size)
  
  num_scrambles = 10000
  
  # run permutation test
  empirical_distribution = as_tibble(replicate(num_scrambles, scramble_it(experimental_group, control_group)))
  colnames(empirical_distribution) = "observed_difference"
  
  # plot empirical distribution
  ggplot(empirical_distribution, aes(observed_difference, fill = "observed_difference")) + 
    geom_density() +
    geom_vline(aes(xintercept = mean(experimental_group) - mean(control_group)), 
               color = "#00C094", linetype = "dashed", size = 1) +
    theme(legend.position="none")
  
  # get p value
  sum(abs(empirical_distribution) > abs(mean(experimental_group) - mean(control_group))) / num_scrambles
  
  
  
  ###### logistic regression model (useful with binomial data) ###### 
  ###### mixed model (useful when you have hierarchically structured data) ###### 
  
  # two groups of 1000 samples
  # 1 trial per sample
  # experimental_group is 3% more likely to convert.
  control_group = rbinom(n = 1000, size = 1, prob = .1)
  experimental_group = rbinom(n = 1000, size = 1, prob = .13)
  
  # transform for modeling
  data_model = gather(tibble(control_group, experimental_group),
                      "condition", "conversion", factor_key = TRUE)
  
  # add day of week for random effect
  data_model$day = sample(seq(1, 7), nrow(data_model), replace = TRUE)
  
  # logistic regression mixed model with day of the week as random effect
  model = glmer(conversion ~ 1 + condition + (1 | day), family = "binomial", data = data_model)
  
  summary(model)
  
  # get p value for between conditions test
  summary(model)$coefficients[2, 4]
  
}



########################
# visualize
########################


##### false alarm inflation #####

theme_set(theme_gray(base_size = 20))
theme = theme_update(legend.position = "top")

# plot single samples
ggplot(results, aes(p_single, fill = "p_single")) + 
  geom_density(alpha = .5, adjust = .5) +
  geom_vline(aes(xintercept = .05), 
             color = "black", linetype = "dashed", size = 1) +
  theme(legend.position="none") +
  xlab("p value") +
  ggtitle(paste("single samples false alarm rate =", round(mean(results$p_single < .05), digits = 3)))

# plot hacked samples
ggplot(results, aes(p_hack, fill = "hack")) + 
  geom_density(alpha = .5, adjust = .5) +
  geom_vline(aes(xintercept = .05), 
             color = "black", linetype = "dashed", size = 1) +
  theme(legend.position="none") +
  xlab("p value") +
  ggtitle(paste("hack samples false alarm rate =", round(mean(results$p_hack < .05), digits = 3)))

# plot both
plot_all = gather(select(results, p_hack, p_single), "condition", "p_value")

ggplot(plot_all, aes(p_value, fill = condition, group = condition)) + 
  geom_density(alpha = .5, adjust = .5) +
  geom_vline(aes(xintercept = .05), color = "black", linetype = "dashed", size = 1) +
  xlab("p value") +
  scale_fill_discrete(name = "", labels = c("p-hacked  ", "single sample")) +
  ggtitle(paste0("false alarm rate increased ", 
                round((mean(results$p_hack < .05) - mean(results$p_single < .05)) / 
                        mean(results$p_single < .05), digits = 2) * 100, "%"))



##### effect size inflation #####

effect_size = .2

# run simulations
results_list = replicate(10000, simulate_experiments())

# convert list to dataframe
results = as_tibble(matrix(unlist(results_list), nrow = 10000, byrow = T))
colnames(results) = rownames(results_list)

# clean up
remove(results_list)

# plot both
plot_all = gather(select(results, effect_hack, effect_single), "condition", "effect")

ggplot(plot_all, aes(effect, fill = condition, group = condition)) + 
  geom_density(alpha = .4, adjust = .5) +
  geom_vline(aes(xintercept = effect_size), color = "black", linetype = "dashed", size = 1) +
  xlab("observed difference") +
  scale_fill_discrete(name = "", labels = c("p-hacked  ", "single sample")) +
  ggtitle(paste0("observed difference inflated by ", 
                 round((mean(results$effect_hack) - mean(results$effect_single)) / 
                         effect_size, digits = 2) * 100, "%"))

