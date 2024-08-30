import numpy as np
import gym
from gym import error, spaces, utils
from gym.utils import seeding

ACTIONS = ['UP', 'DOWN', 'LEFT', 'RIGHT']
SIDE_LEN = 5


class BerryPatchEnv(gym.Env):
    metadata = {'render.modes': ['human']}
    def __init__(self, side_len=4, has_snakes=True):
        # Set information about the environment
        self.action_space = spaces.Discrete(4)
        self.obs_shape = [16]
        self.observation_space = spaces.Box(low=0, high=1, shape=self.obs_shape, dtype=np.float32)
        
        self.berry_found = False
        self.berry_reward = 50
        self.has_snake = has_snakes
        self.height = self.width = side_len
        self.n_locs = 16
        # init the state->reward function
        self.berrypatch = np.full((4, 4), -1.)

        # Time to add berries

        self.berrypatch[3, 3] = self.berry_reward
        self.berry_loc = np.array([3, 3])

        # Time to add snakes
        if has_snakes:
            self.berrypatch[0, 1] = -10
            self.berrypatch[2, 0] = -10
            self.berrypatch[2, 1] = -10
            self.berrypatch[2, 2] = -10
            self.snake_locs = np.array([[0, 1]])
            self.snake_locs = np.append(self.snake_locs, [[2, 0]], 0)
            self.snake_locs = np.append(self.snake_locs, [[2, 1]], 0)
            self.snake_locs = np.append(self.snake_locs, [[2, 2]], 0)

        # init agent location
        self.cur_loc = np.array([0, 0])

        print(self.berrypatch)

    def reset(self):
        '''reinit the agent loc '''
        self.cur_loc = np.array([0,0])
        #Add berries back
        self.berry_found = False
        self.berrypatch[3, 3] = self.berry_reward
        print("New episode")
        return self.get_agent_loc()


    def is_goal_complete(self):
        '''decide if to terminate this trial'''
        if self.berry_found:
            print("HARVESTED")
        return self.berry_found

    def get_agent_loc(self):
        '''get the current location / state of the agent'''
        grid = np.zeros((self.height, self.width))
        grid[tuple(self.cur_loc)] = 1
        return grid.flatten()

    def get_reward(self, input_loc):
        '''compute the reward given the current state'''
        return self.berrypatch[tuple(input_loc)]

    def step(self, action_):
        """take an action, update the current location of the agent, and
        return a reward
        Parameters
        ----------
        action : int
            [0,1,2,3] -> ['UP', 'DOWN', 'LEFT', 'RIGHT']
        Returns
        -------
        a number
            the reward at time t
        """
        print(ACTIONS[action_])
        action = ACTIONS[action_]
        if action == 'UP':
            if self.cur_loc[0] != 0:
                self.cur_loc += [-1, 0]
        elif action == 'DOWN':
            if self.cur_loc[0] != self.height - 1:
                self.cur_loc += [1, 0]
        elif action == 'LEFT':
            if self.cur_loc[1] != 0:
                self.cur_loc += [0, -1]
        elif action == 'RIGHT':
            if self.cur_loc[1] != self.width - 1:
                self.cur_loc += [0, 1]
        else:
            raise ValueError(f'unrecognizable action')
        # compute return info
        r_t = self.get_reward(self.cur_loc)

        # if you've found the berry, then update berry_found
        if (self.cur_loc == self.berry_loc).all():
            self.berry_found = True
            print("complete")

        return self.get_agent_loc(), r_t, self.is_goal_complete(), {}

    def render(self, mode='human', close=False):
            return

    def seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    def close(self):
        return