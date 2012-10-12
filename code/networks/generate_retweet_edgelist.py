import csv
import re
import string
import numpy as np
import pandas as pds
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import os
import networkx as nx
import operator

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
#rt_data = pandas.read_csv('./data/cand_fuser_msg.csv')
rt_data = pds.read_csv('/mnt/fwire_80/twitter_election2012/data/doc_term_mat/house_data.csv')#tweets
user_pship = pds.read_csv('/mnt/fwire_80/twitter_election2012/data/user_partisanship.csv')

rt_edgelist = extract_edgelist(['master'] * rt_data.shape[0],#rt_data['unique_cand_id'],
                               rt_data['from_user'],
                               rt_data['text']
                               )


## First layout each edgelist separately
# rt_graphs = dgraph_from_edgelist(rt_edgelist, edge_threshold = 0)
# cc_graph_length = plot_largest_connected_component(rt_graphs, plot=True, nx_layout='sfdp')

# import operator
# sorted_cc = sorted(cc_graph_length.iteritems(), key=operator.itemgetter(1))
# sorted_betweenness = return_most_connected_individual(rt_graphs)

## Accumulate the nodes and edges across districts
## NOTE: much faster to accumulate nodes in dict
global_nodedict = {}
global_edgedict = {}
for rtg in rt_edgelist:
    for edge in rt_edgelist[rtg]:
        for node in edge:
            if node not in global_nodedict:
                global_nodedict[node] = 1
        if edge not in global_edgedict:
            global_edgedict[edge] = 1
        else:
            global_edgedict[edge] += 1

## Set node color by user partisanship
global_nodelist = []
for node in global_nodedict.keys():
    if node in user_pship['user'].values:
        ncolor = user_pship['pscore'][user_pship['user'].values ==  node]
        ncolor = ncolor.values[0]
    else:
        ncolor = 0
    global_nodelist.append((node, {'color': ncolor})) 

## Generate the (f,t,w) edge tuples
global_ebunch = []
for edge in global_edgedict:
    etuple = (edge[0], edge[1], global_edgedict[edge])
    global_ebunch.append(etuple)

## Generate the graph and recover its largest connected component
global_rt_graph = nx.DiGraph()
global_rt_graph.add_nodes_from(global_nodelist)
global_rt_graph.add_weighted_edges_from(global_ebunch)

largest_cc = nx.connected_component_subgraphs(global_rt_graph.to_undirected())
largest_cc_mst = nx.minimum_spanning_tree(largest_cc[0])

## Generate the betweenness centrality and sort it
lcc_centrality = nx.degree_centrality(largest_cc[0])
sorted_centrality = sorted(lcc_centrality.iteritems(),
                           key=operator.itemgetter(1),
                           reverse=True
                           )

labels_to_plot = [c[0] for i,c in enumerate(sorted_centrality) if i <= 30]

plot_labels = [n if n in labels_to_plot else ''
               for n in largest_cc_mst.nodes()]
plot_labels = dict(zip(largest_cc_mst.nodes(), plot_labels))
## Recover the color and size attributes for nodes
## in the mst
node_color = nx.get_node_attributes(largest_cc_mst, 'color')
colorvec = [node_color[n] for n in node_color]
nodesize = [2 if np.abs(c) > 0 else 0.2 for c in colorvec]

## Lay out and plot the graph
graph_layout = nx.graphviz_layout(largest_cc_mst, prog='sfdp')

nx.draw_networkx_edges(largest_cc_mst,
                       graph_layout,
                       alpha=0.2,
                       width=0.2
                       )
nx.draw_networkx_nodes(largest_cc_mst,
                       graph_layout,
                       nodelist=[n for n in node_color
                                 if np.abs(node_color[n]) == 0],
                       node_color=[c for c in colorvec
                                   if np.abs(c) == 0],
                       vmin=-1,
                       vmax=1,
                       cmap=matplotlib.cm.get_cmap('RdBu_r'),
                       alpha=0.5,
                       node_size=[s for s, c in zip(nodesize, colorvec)
                                  if np.abs(c) == 0],
                       linewidths=0.05
                       )
nx.draw_networkx_nodes(largest_cc_mst,
                       graph_layout,
                       nodelist=[n for n in node_color if np.abs(node_color[n]) > 0],
                       node_color=[c for c in colorvec
                                   if np.abs(c) > 0],
                       vmin=-1,
                       vmax=1,
                       cmap=matplotlib.cm.get_cmap('RdBu_r'),
                       alpha=1,
                       node_size=[s for s, c in zip(nodesize, colorvec)
                                  if np.abs(c) > 0],
                       linewidths=0.05
                       )
nx.draw_networkx_labels(largest_cc_mst,
                        graph_layout,
                        labels=plot_labels,
                        font_color='green',
                        font_size=6
                        )
plt.savefig('./figures/largest_cc_partisan_plot.pdf')
plt.close()
