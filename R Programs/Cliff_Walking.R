
###################################################################
# Cliff Walking
# The agent explores a grid with a cliff that restarts the location
# The agent learns how to reach the goal as quickly as possible
###################################################################

###############
# House Keeping
###############

# Clean up
rm(list=ls())

# Set seed
set.seed(802)

# Grid size
grid_rows = 4
grid_cols = 12

# Action set 1=Up 2=Down 3=Left 4=Right
actions = 1:4

# Reward at Each Step
reward = -1

# Step Size
alpha = 0.5

# Exploration Rate
epsilon = 0.1

# Locations
start = c(4, 1)
goal = c(4, 12)

# Q learning table
q_table_sarsa = array(0, dim = c(grid_rows,grid_cols,length(actions)))
q_table_ql = array(0, dim = c(grid_rows,grid_cols,length(actions)))

# Set number of episodes to run
episodes = 500

######################################
# Function to move through environment
######################################

step = function(state,action){
  
  rownum = state[1]
  colnum = state[2]
  reward = -1
  
  # 1=Up
  if(action == 1){
    
    rownum = max(rownum -1, 1)
    
    # 2=Down
  } else if(action == 2){
    
    rownum = min(rownum +1, grid_rows)
    
    # 3=Left
  } else if(action == 3){
    
    colnum = max(colnum -1, 1)
    
    # 4=Right
  } else{
    
    colnum = min(colnum +1, grid_cols)
    
  }
  
  # Restart Position if Agent Moves Off Cliff
  if( (rownum == grid_rows) & (colnum %in% 2:(grid_cols-1)) ){
    
    rownum = start[1]
    colnum = start[2]
    reward = -100
    
  }
  
  return(list("state"=c(rownum,colnum), "reward"=reward))
  
} # End Step Function

#########################################################################
# Function to select the greedy action from the current state and q_table
#########################################################################

greedy = function(state, q_table, epsilon){
  
  # Randomly select action with probability epsilon
  if(runif(1)<epsilon){
    
    g_action=sample(1:4,1)
    
  } else{
    
    # Select the best action for the given state with probability 1-epsilon
    q_vals = q_table[state[1], state[2], ]
    g_action = which(q_vals==max(q_vals))
    if(length(g_action)>1){g_action=sample(g_action,1)}
    
  }
  
  return(g_action)
  
} # End Greedy Function

###############################
# Run SARSA in Windy Grid World
###############################

# Store the steps to reach the goal for each episode
steps_vec_sarsa = NULL
reward_vec_sarsa = NULL

# SARSA
for(i in 1:episodes){

  # Count steps and sum total reward per episode
  steps_count = 0
  reward_total = 0
  
  # Always start in the same position
  state = c(4, 1)
  
  # Choose action based on epsilon-greedy
  action = greedy(state, q_table_sarsa, epsilon)
  
  
  # Loop until the goal is reached
  while(!setequal(state,goal)){
    
    # Find next state and reward given current state and action
    new_s_r = step(state, action)
    new_state = new_s_r$state
    reward = new_s_r$reward
    
    # Choose action based on epsilon-greedy
    new_action = greedy(new_state, q_table_sarsa, epsilon)
    
    # Update the q-table with SARSA
    q_table_sarsa[state[1],state[2],action] =  q_table_sarsa[state[1],state[2],action] + 
      alpha * ( reward + q_table_sarsa[new_state[1],new_state[2],new_action] - 
                  q_table_sarsa[state[1],state[2],action] )
    
    # Set new state to current state and new action to current action
    state = new_state
    action = new_action

    # Update steps count and reward total
    steps_count = steps_count + 1
    reward_total = reward_total + reward
    
  }
  
  # Save steps and total reward per episode
  steps_vec_sarsa = c(steps_vec_sarsa, steps_count)
  reward_vec_sarsa = c(reward_vec_sarsa, reward_total)
  
} # End Loop

####################################
# Run Q-Learning in Windy Grid World
####################################

# Store the steps to reach the goal for each episode
steps_vec_ql = NULL
reward_vec_ql = NULL

# Q-Learning
for(i in 1:episodes){
  
  # Count steps and sum total reward per episode
  steps_count = 0
  reward_total = 0
  
  # Always start in the same position
  state = c(4, 1)
  
  # Loop until the goal is reached
  while(!setequal(state,goal)){
    
    # Choose action based on epsilon-greedy
    action = greedy(state, q_table_ql, epsilon)
    
    # Find next state and reward given current state and action
    new_s_r = step(state, action)
    new_state = new_s_r$state
    reward = new_s_r$reward
    
    # Update the q-table with Q-Learning
    q_table_ql[state[1],state[2],action] =  q_table_ql[state[1],state[2],action] + 
      alpha * ( reward + max(q_table_ql[new_state[1],new_state[2],]) - 
                  q_table_ql[state[1],state[2],action] )
    
    # Set new state to current state
    state = new_state
    
    # Update steps count and reward total
    steps_count = steps_count + 1
    reward_total = reward_total + reward
    
  }
  
  # Save steps and total reward per episode
  steps_vec_ql = c(steps_vec_ql, steps_count)
  reward_vec_ql = c(reward_vec_ql, reward_total)
  
} # End Loop

###################################
# View SARSA and Q-Learning Results
###################################

# Summary Info SARSA
min_steps_sarsa = grid_cols + 1
optimal_path_count_sarsa = sum(steps_vec_sarsa==min_steps_sarsa)
paste0("The agent followed the optimal path ", 100*optimal_path_count_sarsa/episodes, "% of the time with SARSA.")

# Summary Info Q-Learning
min_steps_ql = grid_cols + 1
optimal_path_count_ql = sum(steps_vec_ql==min_steps_ql)
paste0("The agent followed the optimal path ", 100*optimal_path_count_ql/episodes, "% of the time with Q-Learning.")

# Graphic of steps by episode
plot(steps_vec_ql, type="l", ylim=c(10,50), main="Total Steps Per Episode", xlab="Episodes", ylab="Steps to Reach Goal", col="blue", lwd=1)
lines(steps_vec_sarsa, col="red")
legend(300, 50, legend=c("Q-Learning", "SARSA"),col=c("blue","red"), lwd=c(3,3), cex=0.8)

# Graphic of the cumulative time steps across episodes
plot(cumsum(steps_vec_ql), 1:episodes, type="l", main="Cumulative Steps", ylab="Episodes", xlab="Time Steps", col="blue", lwd=3)
lines(cumsum(steps_vec_sarsa), 1:episodes, type="l", col="red", lwd=3)
legend(6000, 100, legend=c("Q-Learning", "SARSA"),col=c("blue","red"), lwd=c(3,3), cex=0.8)

# Summary Information
paste0("The average reward for SARSA is ", mean(reward_vec_sarsa))
paste0("The average reward for Q-learning is ", mean(reward_vec_ql))

# Create Function to Find the Moving Average of a Vector
mov_ave = function(vec,obs){
  
  MA = NULL
  for(i in 1:(length(vec)-(obs-1))){
    MA = c(MA,mean(vec[i:(i+(obs-1))]))
  }
  
  return(MA)
  
}

# Graphic of the Total Reward Moving Average
MA_window = 100
plot(mov_ave(reward_vec_ql, MA_window), type="l", ylim=c(-60,-10), col="blue", main="Total Reward Per Episode", xlab="Episode", ylab="Moving Average")
lines(mov_ave(reward_vec_sarsa, MA_window), col="red")
legend(250, -10, legend=c("Q-Learning", "SARSA"),col=c("blue","red"), lwd=c(3,3), cex=0.8)

# Find the optimal direction for each state (U,D,L,R) with Q-learning
optimal_policy_ql = matrix(0,nrow=grid_rows,ncol=grid_cols)
for(i in 1:grid_rows){
  for(j in 1:grid_cols){
    direction = which.max(q_table_ql[i,j,])
    if(direction==1){optimal_policy_ql[i,j]="U"}
    if(direction==2){optimal_policy_ql[i,j]="D"}
    if(direction==3){optimal_policy_ql[i,j]="L"}
    if(direction==4){optimal_policy_ql[i,j]="R"}
  }
}

for(i in 2:(grid_cols-1)){
  optimal_policy_ql[grid_rows,i]="C"
}

optimal_policy_ql[4,12]="G"
optimal_policy_ql

# Find the optimal direction for each state (U,D,L,R) with SARSA
optimal_policy_sarsa = matrix(0,nrow=grid_rows,ncol=grid_cols)
for(i in 1:grid_rows){
  for(j in 1:grid_cols){
    direction = which.max(q_table_sarsa[i,j,])
    if(direction==1){optimal_policy_sarsa[i,j]="U"}
    if(direction==2){optimal_policy_sarsa[i,j]="D"}
    if(direction==3){optimal_policy_sarsa[i,j]="L"}
    if(direction==4){optimal_policy_sarsa[i,j]="R"}
  }
}

for(i in 2:(grid_cols-1)){
  optimal_policy_sarsa[grid_rows,i]="C"
}

optimal_policy_sarsa[4,12]="G"
optimal_policy_sarsa

##########################################################################################
# Change epsilon to 0.001 and rerun the program
# When epsilon approaches 0 SARSA will converge to the optimal fast policy along the cliff
##########################################################################################





