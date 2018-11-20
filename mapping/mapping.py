#!/usr/bin/env python3

import networkx as nx
import matplotlib.pyplot as plt
from networkx.drawing.nx_agraph import write_dot, graphviz_layout
from sklearn import cluster

Nnodes = 16

def generate_map(Nnodes):
    G = nx.Graph()

    Ncores = 72
    N = Nnodes*Ncores

    for i in range(0, N):
        d = 1
        while i + d < N:
            if d <= i:
                d = d * 2
                continue
            if i + d >= Nnodes:
                G.add_edge(i, i + d)
            d = d * 2

    all_nodes = list()
    for component in nx.connected_component_subgraphs(G):
        all_nodes.extend(component.nodes)

    print('Writing', Nnodes)
    with open('MPICH_RANK_ORDER.{}'.format(Nnodes), 'w') as f:
        f.write(','.join([str(i) for i in all_nodes]))

    return G

for i in [16, 32, 64, 128, 256, 512]:
    generate_map(i)

G = generate_map(16)
pos = graphviz_layout(G, prog='dot')
nx.draw(G, pos, with_labels=True)

# print(nx.is_tree(G))
plt.show()
