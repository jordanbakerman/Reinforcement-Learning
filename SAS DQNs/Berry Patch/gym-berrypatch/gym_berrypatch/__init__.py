from gym.envs.registration import register

register(
    id='berrypatch-v0',
    entry_point='gym_berrypatch.envs:BerryPatchEnv',
	max_episode_steps=200,
)