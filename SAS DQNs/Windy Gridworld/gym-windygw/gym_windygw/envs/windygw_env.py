
import gym
from gym import error, spaces, utils
from io import StringIO
import sys
import numpy as np

class WindygwEnv(gym.Env):
    
    metadata = {'render.modes': ['human']}
    
    def __init__(self):
        
        # Grid size
        self.grid_rows = 7
        self.grid_cols = 10

        # Locations
        self.start = [3, 0]
        self.goal = [3, 7]
        
        # Wind Strength 
        self.wind = [0, 0, 0, 1, 1, 1, 2, 2, 1, 0]
        
        # Grid matrix
        self.grid = np.full((7, 10), "-")
        self.grid[self.start[0],self.start[1]] = "S"
        self.grid[self.goal[0],self.goal[1]] = "G"

        print(self.grid)

        # Action set 0=Up 1=Down 2=Left 3=Right
        self.actions = range(4)
        self.action_letters = ["Up","Down","Left","Right"]

        # Reward at Each Step
        self.reward = -1
        
        # Action space
        self.action_space =  spaces.Discrete(len(self.actions))
        
        # Observation space
        self.observation_space = spaces.Box(low=0,high=10,shape=[2])
    
    # Initialize state location
    def reset(self):
        
        self.state = self.start
        return self.state
    
    # Function to move through environment
    def step(self, action):
        
        self.lasta = action
        rownum, colnum = self.state
    
        # 0=Up
        if action==0:
            rownum = max( rownum -1 -self.wind[colnum], 0 )
    
        # 1=Down
        elif action==1:
            rownum = max( min( rownum +1 -self.wind[colnum], self.grid_rows-1 ), 0 )
    
        # 2=Left
        elif action==2:
            rownum = max( rownum -self.wind[colnum], 0 )
            colnum = max( colnum -1, 0 )
    
        # 3=Right
        else:
            rownum = max( rownum -self.wind[colnum], 0 )
            colnum = min( colnum +1, self.grid_cols-1 )
        
        # Update internal state location
        self.state = [rownum, colnum]
        
        # Indicator for episode end
        if self.state == self.goal:
            done = True
        else:
            done = False
            
        # Return new state, reward received, episode end indicator, additional information
        return self.state, self.reward, done, {}
       
    # Print the state of the agent in the environment
    def render(self, mode="human"):

        None

        # Initialize streaming file
        # outfile = StringIO() if mode == "ansi" else sys.stdout

        # convert grid to list
        # agent_loc = self.grid.copy()
        # agent_loc[self.state[0],self.state[1]] = "A"
        # agent_loc = agent_loc.tolist()

        # Specify colors
        # agent_loc[self.start[0]][self.start[1]] = utils.colorize(agent_loc[self.start[0]][self.start[1]], color="blue", highlight=False)
        # agent_loc[self.goal[0]][self.goal[1]] = utils.colorize(agent_loc[self.goal[0]][self.goal[1]], color="red", highlight=False)
        # agent_loc[self.state[0]][self.state[1]] = utils.colorize(agent_loc[self.state[0]][self.state[1]], color="green", highlight=True)

        # Print action and environment
        # outfile.write("\n"+str(self.action_letters[self.lasta])+"\n")
        # outfile.write("\n".join("".join(line) for line in agent_loc) + "\n")

