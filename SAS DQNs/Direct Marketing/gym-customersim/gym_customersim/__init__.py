from gym.envs.registration import register

register(
    id='CustomerSim-v0',
    entry_point='gym_customersim.envs:CustomerSimEnv',
	max_episode_steps=18,
)