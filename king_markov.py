import random
import matplotlib.pyplot as plt


num_weeks = 1000
positions = [0] * num_weeks
current = 10

for i in range(num_weeks):
    # Record current position
    positions[i] = current
    # Flip coin to generate proposal
    proposal = current + random.choice([-1, 0, 1])
    # Now make sure he loops around the archipelago
    if proposal < 1:
        proposal = 10
    if proposal > 10:
        proposal = 1
    # Move
    prob_move = proposal / current
    if random.random() < prob_move:
        current = proposal

plt.scatter(range(num_weeks), positions, marker='.', alpha=.2)
plt.show()
