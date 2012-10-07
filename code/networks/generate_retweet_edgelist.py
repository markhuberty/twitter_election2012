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
    punct_list = '[' + string.punctuation + ']'
    if rt_origin:
        out = re.sub('RT @', '', rt_origin.group(0))
        out = out.strip()
        out = re.sub(punct_list, '',  out)
        #out = out.encode('ascii', 'replace')
    else:
        out = None
    return out

def extract_edgelist(group_id, from_user, msg_list):
    edgelist_dict = {}
    for group, msg_author, msg in zip(group_id, from_user, msg_list):
        rt_origin = is_rt(msg)
        if rt_origin:
            try:
                rt_origin = rt_origin.encode('ascii', 'replace')
                msg_author = msg_author.encode('ascii', 'replace')
            except:
                continue
            pair = (rt_origin, msg_author) #msg_author.encode('ascii', 'replace'))
            if group in edgelist_dict:
                edgelist_dict[group].append(pair)
            else:
                edgelist_dict[group] = [pair]
        else:
            continue
    return edgelist_dict

def dgraph_from_edgelist(edgelist, edge_threshold):
    graph_dict = {}
    for k in edgelist:
        if len(edgelist[k]) >= edge_threshold:
            dgraph = nx.DiGraph()
            dgraph.add_edges_from(edgelist[k])
            graph_dict[k] = dgraph
    return graph_dict

def plot_largest_connected_component(rt_graphs, nx_layout='neato', file_type='png', plot=True):
    rtg_cc_length = {}
    for rtg in rt_graphs:
        rtg_mst = nx.connected_component_subgraphs(rt_graphs[rtg].to_undirected())
        rtg_cc_length[rtg] = len(rtg_mst[0])
        if plot:
            rtg_layout = nx.graphviz_layout(rtg_mst[0], prog=nx_layout)
            file_name = './figures/rt_graphs/' + rtg + '_graphviz_layout.' + file_type
            nx.draw_networkx_nodes(rtg_mst[0], pos=rtg_layout, node_size=5, alpha=0.7)
            nx.draw_networkx_edges(rtg_mst[0], pos=rtg_layout)
            plt.savefig(file_name)
            plt.close()
    return rtg_cc_length

def return_most_connected_individual(rt_graphs):
    rtg_connected = {}
    for rtg in rt_graphs:
        idc = nx.betweenness_centrality(rt_graphs[rtg])
        idc_sorted = sorted(idc.iteritems(), key=operator.itemgetter(1))
        rtg_connected[rtg] = idc_sorted[-1]
    return rtg_connected
## TODO: would be interesting to look at overlap between candidates in their RT graphs. 
        

## Test it
rt_data = pandas.read_csv('./data/cand_fuser_msg.csv')


rt_edgelist = extract_edgelist(rt_data['unique_cand_id'],
                               rt_data['from_user'],
                               rt_data['text']
                               )


## First layout each edgelist separately
rt_graphs = dgraph_from_edgelist(rt_edgelist, edge_threshold = 0)
cc_graph_length = plot_largest_connected_component(rt_graphs, plot=False)

import operator
sorted_cc = sorted(cc_graph_length.iteritems(), key=operator.itemgetter(1))
sorted_betweenness = return_most_connected_individual(rt_graphs)



## Then combine and extract
