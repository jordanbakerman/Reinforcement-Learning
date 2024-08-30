from gym.envs.registration import register

register(
    id='windygw-v0',
    entry_point='gym_windygw.envs:WindygwEnv',
	max_episode_steps=200,
)