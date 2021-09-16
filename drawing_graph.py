import matplotlib.pyplot as plt
import networkx as nx


G = nx.MultiGraph([(1, 2),
                   (2, 3),
                   (3, 2),
                   (2, 1)])
pos = {1: (.75, 1), 2: (1, 1), 3: (1.25, 1)}
names = {1: 'X', 2: 'Z', 3: 'Y'}
nx.draw_networkx_nodes(G, pos, node_color='r', node_size=200, alpha=1)
ax = plt.gca()
arrow_styles = ['<|-', '-|>']
for e in G.edges:
    ax.annotate('',
                xy=pos[e[0]],
                xytext=pos[e[1]],
                arrowprops=dict(arrowstyle=arrow_styles[e[2]], color="red",
                                shrinkA=10, shrinkB=10,
                                mutation_scale=35,
                                connectionstyle="arc3,rad=rrr".replace('rrr', str(2 * (e[2] - 0.5))
                                                                       ),
                                )
                )
for n in G.nodes:
    ax.annotate(names[n], xy=pos[n], color='blue', fontsize=50)
plt.tight_layout()
plt.axis('off')
plt.show()

