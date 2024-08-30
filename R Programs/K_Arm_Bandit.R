
rm(list=ls())

########################
# Reinforcement Learning
########################

########################################################################################################
# Classic RL Problem
# K Arm Bandit 
# 
# https://towardsdatascience.com/solving-the-multi-armed-bandit-problem-b72de40db97c
# https://github.com/ankonzoid/LearningX/blob/master/classical_RL/multiarmed_bandit/multiarmed_bandit.py
# 
# You have a slot machine with k arms and each arm has its own probability distribution for success
# The goal is to sequentially pull the arms to maximize the total reward collected in the long run
########################################################################################################

# Define some parameters for the experiment
probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65) # Probability for each arm in bandit
actions = seq(1:length(probs)) # Number of arms on the bandit
eps = 0.10 # The switching from the maximum expected reward action probability

N_action = rep(0,length(probs)) # Number of times each action taken
Q_action = rep(0,length(probs)) # The expected reward for each action

episodes = 5000 # Number of trials to run
R_action = rep(0,episodes) # The reward for each trial
which_action = rep(0,episodes) # The action taken for each trial

# Play the game with RL
for(episode in 1:episodes){
  
  # Find which action to take (maximum vs switching probability)
  max_action = which(Q_action==max(Q_action))
  if(length(max_action)>1){take_action = sample(max_action,1)} else{take_action=max_action}
  if(runif(1)<eps){take_action = sample(actions,1)}
  which_action[episode] = take_action
  
  # Count number of times each action is taken
  N_action[take_action] = N_action[take_action] + 1
  
  # Store the reward and update the expected reward for each action
  R_action[episode] = rbinom(1,1,probs[take_action])
  Q_action[take_action] = Q_action[take_action] + (1/N_action[take_action])*(R_action[episode]-Q_action[take_action])
  
}

# Print summary information
table(which_action)
round(Q_action,3)
mean(R_action)

# Plot the cumulative reward average
R_avg = cumsum(R_action) / seq_along(R_action) 
plot(R_avg,type="l", xlab="Episode Number", ylab="Reward Average")

# Plot the cumulative count percentage for each arm
for(i in 1:length(probs)){
  
  action_counts = which_action
  action_counts[action_counts!=i]=0
  action_counts[action_counts==i]=1
  count_perc = cumsum(action_counts) / seq_along(action_counts)
  
  if(i==1){plot(count_perc,type="l",ylim=c(0,1),xlab="Episode Number", ylab="Count Percentage",col=i)} else{lines(count_perc,col=i)}
  
}

# Add a legend to the plot
k_arms = NULL
for(i in 1:length(probs)){
  k_arms = c(k_arms, paste("Arm ",i,"=",probs[i]))
}

legend(3000, 0.8, legend=k_arms, col=1:10, lty=1, )

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

################################################
# Conduct experiments to view long term behavior
################################################

probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65)
episodes = 500
experiments = 10000

action_mat = matrix(rep(0,episodes*length(probs)),episodes,length(probs))
dim(action_mat)
R_experiment_results = rep(0,episodes)

for(experiment in 1:experiments){
  
  # Define some parameters for the experiment
  probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65)
  actions = seq(1:length(probs))
  eps = 0.10
  
  N_action = rep(0,length(probs))
  Q_action = rep(0,length(probs))
  
  episodes = 500
  R_action = rep(0,episodes)
  which_action = rep(0,episodes)
  
  # Play the game with RL
  for(episode in 1:episodes){
    
    max_action = which(Q_action==max(Q_action))
    if(length(max_action)>1){take_action = sample(max_action,1)} else{take_action=max_action}
    if(runif(1)<eps){take_action = sample(actions,1)}
    which_action[episode] = take_action
    
    N_action[take_action] = N_action[take_action] + 1
    
    R_action[episode] = rbinom(1,1,probs[take_action])
    Q_action[take_action] = Q_action[take_action] + (1/N_action[take_action])*(R_action[episode]-Q_action[take_action])
    
    #
    action_mat[episode,which_action[episode]] = action_mat[episode,which_action[episode]] + 1
    
  } # Episode End
  
  R_experiment_results = R_experiment_results + R_action
  
  if((experiment%%100)==0){print(experiment)}
  
} # Experiment End



# Plot the Reward average for each episode averaged over the experiments
R_experiment_results
plot(R_experiment_results/experiments,type="l", xlab="Episode Number", ylab="Reward Average",col="blue")

# Plot the Count Percentage for each arm on the bandit
action_mat_perc = action_mat/experiments
for(i in 1:length(probs)){
  
  if(i==1){plot(action_mat_perc[,i],type="l",ylim=c(0,1),xlab="Episode Number", ylab="Count Percentage",col=i)} else{lines(action_mat_perc[,i],col=i)}
  
}

# Add a legend to the plot
k_arms = NULL
for(i in 1:length(probs)){
  k_arms = c(k_arms, paste("Arm ",i,"=",probs[i]))
}

legend(0, 1, legend=k_arms, col=1:10, lty=1, )

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

rm(list=ls())

########################
# Reinforcement Learning
########################

########################################################################################################
# Classic RL Problem
# K Arm Bandit 
# 
# https://towardsdatascience.com/solving-the-multi-armed-bandit-problem-b72de40db97c
# https://github.com/ankonzoid/LearningX/blob/master/classical_RL/multiarmed_bandit/multiarmed_bandit.py
# 
# You have a slot machine with k arms and each arm has its own probability distribution for success
# The goal is to sequentially pull the arms to maximize the total reward collected in the long run
########################################################################################################

##################################
# Define the k_arm_bandit function
##################################

k_arm_bandit = function(probs, eps, episodes){
  
  # Define some values for the experiment
  actions = seq(1:length(probs))
  
  N_action = rep(0,length(probs))
  Q_action = rep(0,length(probs))
  
  R_action = rep(0,episodes)
  which_action = rep(0,episodes)
  
  action_mat = matrix(rep(0,episodes*length(probs)),episodes,length(probs))
  
  # Play the game with RL
  for(episode in 1:episodes){
    
    max_action = which(Q_action==max(Q_action))
    if(length(max_action)>1){take_action = sample(max_action,1)} else{take_action=max_action}
    if(runif(1)<eps){take_action = sample(actions,1)}
    which_action[episode] = take_action
    
    N_action[take_action] = N_action[take_action] + 1
    
    R_action[episode] = rbinom(1,1,probs[take_action])
    Q_action[take_action] = Q_action[take_action] + (1/N_action[take_action])*(R_action[episode]-Q_action[take_action])
    
    #
    action_mat[episode,which_action[episode]] = action_mat[episode,which_action[episode]] + 1
    
  } # Episode End
  
  return(list("R_action"=R_action, "action_mat"=action_mat,
              "Q_action"=Q_action, "which_action"=which_action))
  
} # Function End

###############################
# Run the k_arm_bandit function
###############################

# Define k_arm_bandit function parameters
probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65)
eps = 0.10
episodes = 500

# Run k_arm_bandit function
ten_arm_bandit = k_arm_bandit(probs, eps, episodes)

##############################################
# Look at the results for a single experiment
##############################################

# Print summary information
table(ten_arm_bandit$which_action)
round(ten_arm_bandit$Q_action,3)
mean(ten_arm_bandit$R_action)

# Plot the cumulative reward average
R_avg = cumsum(ten_arm_bandit$R_action) / seq_along(ten_arm_bandit$R_action) 
plot(R_avg,type="l", xlab="Episode Number", ylab="Reward Average")

# Plot the cumulative count percentage for each arm
for(i in 1:length(probs)){
  
  action_counts = ten_arm_bandit$which_action
  action_counts[action_counts!=i]=0
  action_counts[action_counts==i]=1
  count_perc = cumsum(action_counts) / seq_along(action_counts)
  
  if(i==1){plot(count_perc,type="l",xlim=c(-400,500),ylim=c(0,1),xlab="Episode Number", ylab="Count Percentage",col=i)} else{lines(count_perc,col=i)}
  
}

# Add a legend to the plot
k_arms = NULL
for(i in 1:length(probs)){
  k_arms = c(k_arms, paste("Arm ",i,"=",probs[i]))
}

legend(-410, 0.9, legend=k_arms, col=1:10, lty=1)

##################################################################
# Define experiment to view long term behavior of the k_arm_bandit
##################################################################

# Define function parameters
probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65)
eps = 0.10
episodes = 500
experiments = 1000

# Define result matrices
R_experiment_results = rep(0,episodes)
action_mat_experiment_results = matrix(rep(0,episodes*length(probs)),episodes,length(probs))

# Run experiment
for(experiment in 1:experiments){
  
  ten_arm_bandit = k_arm_bandit(probs, eps, episodes)
  
  R_experiment_results = R_experiment_results + ten_arm_bandit$R_action
  action_mat_experiment_results = action_mat_experiment_results + ten_arm_bandit$action_mat
  
  if((experiment%%100)==0){print(experiment)}
  
} # Experiment End

########################################
# Look at the results for the experiment
########################################

# Plot the Reward average for each episode averaged over the experiments
R_experiment_results
plot(R_experiment_results/experiments,type="l", xlab="Episode Number", ylab="Reward Average",col="blue")

# Plot the Count Percentage for each arm on the bandit
action_mat_perc = action_mat_experiment_results/experiments
for(i in 1:length(probs)){
  
  if(i==1){plot(action_mat_perc[,i],type="l",ylim=c(0,0.8),xlab="Episode Number", ylab="Count Percentage",col=i)} else{lines(action_mat_perc[,i],col=i)}
  
}

# Add a legend to the plot
k_arms = NULL
for(i in 1:length(probs)){
  k_arms = c(k_arms, paste("Arm ",i,"=",probs[i]))
}

legend(0, 0.8, legend=k_arms, col=1:10, lty=1)

######################################################
# Vary epsilon (the switching parameter) in simulation
######################################################

# Define function parameters
probs = c(0.10, 0.50, 0.60, 0.80, 0.10, 0.25, 0.60, 0.45, 0.75, 0.65)
epsilon = c(0.01,0.10,0.20)
episodes = 500
experiments = 1000

# Define result matrices
R_eps_results = NULL

for(eps in epsilon){
  
  R_experiment_results = rep(0,episodes)
  action_mat_experiment_results = matrix(rep(0,episodes*length(probs)),episodes,length(probs))
  
  # Run experiment
  for(experiment in 1:experiments){
    
    ten_arm_bandit = k_arm_bandit(probs, eps, episodes)
    
    R_experiment_results = R_experiment_results + ten_arm_bandit$R_action
    action_mat_experiment_results = action_mat_experiment_results + ten_arm_bandit$action_mat
    
    if((experiment%%100)==0){cat("Epsilon",eps,"Experiment",experiment,"\n")}
    
  } # Experiment End
  
  R_eps_results = cbind(R_eps_results,R_experiment_results)
  
} # Epsilon End

#################
# Look at results
#################

R_eps_results_avg = R_eps_results/experiments

for(i in 1:length(epsilon)){
  if(i==1){plot(R_eps_results_avg[,i],type="l",col=i,ylim=c(0.40,0.80),xlab="Episode Number", ylab="Reward Average")}else{lines(R_eps_results_avg[,i],col=i)}
}

# Add a legend to the plot
k_arms = NULL
for(i in 1:length(epsilon)){
  k_arms = c(k_arms, paste("Epsilon ",epsilon[i]))
}

legend(300, 0.6, legend=k_arms, col=1:10, lty=1)














