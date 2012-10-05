import csv
import networkx as nx
import re
import string
import time
import numpy as np
import pandas
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import os

os.chdir('/mnt/fwire_80/twitter_election2012')

def is_rt(msg):
    rt_origin = re.match('RT\s.*?\s', msg)
    if rt_origin:
        out = re.sub('RT @', '', rt_origin.group(0))
        out = out.strip()
    else:
        out = None
    return out

def extract_edgelist(group_id, from_user, msg_list):
    edgelist_dict = {}
    for group, msg_author, msg in zip(group_id, from_user, msg_list):
        rt_origin = is_rt(msg)
        if rt_origin:
            pair = (rt_origin, msg_author)
            if group in edgelist_dict:
                edgelist_dict[group].append(pair)
            else:
                edgelist_dict[group] = pair
        else:
            continue
    return edgelist_dict

def dgraph_from_edgelist(edgelist):
    graph_dict = {}
    for k in edgelist:
        dgraph = nx.DiGraph()
        dgraph.add_edges_from(edgelist[k])

def plot_dgraph_dict(rt_graphs, edge_threshold, nx_layout='neato'):
    for rtg in rt_graphs:
        if len(rt_graphs[rtg].edges) >= edge_threshold:
            rtg_layout = nx.graphviz_layout(rt_graphs[rtg], prog=nx_layout)
            file_name = './figures/rt_graphs/' + rtg + 'graphviz_layout.pdf'
            nx.draw_graphviz(rt_graphs[rtg], prog=nx_layout, font_size=4, node_size=5, alpha=0.7)
            plt.savefig(file_name)
## TODO: would be interesting to look at overlap between candidates in their RT graphs. 
        

## Test it
rt_data = pandas.read_csv('./data/cand_fuser_msg.csv')


rt_edgelist = extract_edgelist(rt_data['unique_cand_id'],
                               rt_data['from_user'],
                               rt_data['text']
                               )


## First layout each edgelist separately
rt_graphs = dgraph_from_edgelist(rt_edgelist)
graph_output = plot_dgraph_dict(rt_graphs)



## Then combine and extract
