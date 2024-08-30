
###############################################################
# Windy Grid World
# There is a cross wind for the columns of the grid
# The agent learns how to reach the goal as quickly as possible
###############################################################

###############
# House Keeping
###############

# Clean up
rm(list=ls())

# Set seed
set.seed(802)

# Grid size
grid_rows = 7
grid_cols = 10

# Wind Strength 
wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)

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
goal = c(4, 8)

# Q learning table
q_table_sarsa = array(0, dim = c(grid_rows,grid_cols,length(actions)))
q_table_ql = array(0, dim = c(grid_rows,grid_cols,length(actions)))

# Set number of episodes to run
episodes = 250

######################################
# Function to move through environment
######################################

step = function(state,action){
  
  rownum = state[1]
  colnum = state[2]
  
  # 1=Up
  if(action == 1){

    rownum = max(rownum -1 -wind[colnum], 1)
    
    # 2=Down
  } else if(action == 2){

    rownum = max(min(rownum +1 -wind[colnum],grid_rows), 1)
    
    # 3=Left
  } else if(action == 3){

    rownum = max(rownum -wind[colnum], 1)
    colnum = max(colnum-1, 1)
    
    # 4=Right
  } else{

    rownum = max(rownum -wind[colnum], 1)
    colnum = min(colnum+1, grid_cols)
    
  }
  
  return(c(rownum,colnum))
  
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

# SARSA
for(i in 1:episodes){
  
  # Count steps per episode
  steps_count = 0
  
  # Always start in the same position
  state = c(4, 1)
  
  # Choose action based on epsilon-greedy
  action = greedy(state, q_table_sarsa, epsilon)
  
  # Loop until the goal is reached
  while(!setequal(state,goal)){
    
    # Find next state given current state and action
    new_state = step(state, action)
    
    # Choose action based on epsilon-greedy
    new_action = greedy(new_state, q_table_sarsa, epsilon)
    
    # Update the q-table with SARSA
    q_table_sarsa[state[1],state[2],action] =  q_table_sarsa[state[1],state[2],action] + 
      alpha * ( reward + q_table_sarsa[new_state[1],new_state[2],new_action] - 
                  q_table_sarsa[state[1],state[2],action] )
    
    # Set new state to current state and new action to current action
    state = new_state
    action = new_action
    
    # Update steps count
    steps_count = steps_count + 1
    
  }
  
  # Save steps per episode
  steps_vec_sarsa = c(steps_vec_sarsa, steps_count)
  
} # End Loop

####################################
# Run Q-Learning in Windy Grid World
####################################

# Store the steps to reach the goal for each episode
steps_vec_ql = NULL

# Q-Learning
for(i in 1:episodes){
  
  # Count steps per episode
  steps_count = 0
  
  # Always start in the same position
  state = c(4, 1)
  
  # Loop until the goal is reached
  while(!setequal(state,goal)){
    
    # Choose action based on epsilon-greedy
    action = greedy(state, q_table_ql, epsilon)
    
    # Find next state given current state and action
    new_state = step(state, action)
    
    # Update the q-table with Q-Learning
    q_table_ql[state[1],state[2],action] =  q_table_ql[state[1],state[2],action] + 
               alpha * ( reward + max(q_table_ql[new_state[1],new_state[2],]) - 
                         q_table_ql[state[1],state[2],action] )
    
    # Set new state to current state
    state = new_state
    
    # Update steps count
    steps_count = steps_count + 1
    
  }
  
  # Save steps per episode
  steps_vec_ql = c(steps_vec_ql, steps_count)
  
} # End Loop

###################################
# View SARSA and Q-Learning Results
###################################

# Summary Info SARSA
min_steps_sarsa = min(steps_vec_sarsa)
optimal_path_count_sarsa = sum(steps_vec_sarsa==min_steps_sarsa)
paste0("The agent followed the optimal path ", 100*optimal_path_count_sarsa/episodes, "% of the time with SARSA.")

# Summary Info Q-Learning
min_steps_ql = min(steps_vec_ql)
optimal_path_count_ql = sum(steps_vec_ql==min_steps_ql)
paste0("The agent followed the optimal path ", 100*optimal_path_count_ql/episodes, "% of the time with Q-Learning.")

# Graphic of steps by episode
plot(steps_vec_ql, type="l", main="Q-Learning Total Steps Per Episode", xlab="Episodes", ylab="Steps to Reach Goal", col="blue", lwd=1)
segments(x0=which(steps_vec_ql==min_steps_ql)[1], x1=which(steps_vec_ql==min_steps_ql)[1], y0=0, y1=50, col='red', lwd=3)
text(140,80, "First Optimal Path", col="red")

# Graphic of the cumulative time steps across episodes
plot(cumsum(steps_vec_ql), 1:episodes, type="l", main="Cumulative Steps", ylab="Episodes", xlab="Time Steps", col="blue", lwd=3)
lines(cumsum(steps_vec_sarsa), 1:episodes, type="l", col="red", lwd=3)
legend(5000, 40, legend=c("Q-Learning", "SARSA"),
       col=c("blue","red"), lwd=c(3,3), cex=0.8)
paste("The first time the agent followed the optimal path was on episode",which(steps_vec_ql==min_steps_ql)[1],"for Q-Learning and episode",which(steps_vec_sarsa==min_steps_sarsa)[1],"for SARSA.")

# Find the optimal direction for each state (U,D,L,R)
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
optimal_policy_ql[4,8]="G"
optimal_policy_ql = rbind(optimal_policy_ql, wind)
optimal_policy_ql




