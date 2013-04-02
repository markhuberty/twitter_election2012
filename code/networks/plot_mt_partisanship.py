import csv
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import operator
import os
import pandas as pd
import re
import string

re_rt = re.compile(r'RT\s.*?\s')
re_mt = re.compile(r'@(\w+)')
re_punct = re.compile(r'[' + string.punctuation + ']')

def is_mt(msg):
    """
    Checks a string for non-rt mentions. Return any mentions found.
    """
    msg = re_rt.sub(' ', msg)
    mentions = re_mt.findall(msg)
    return mentions


def is_rt(msg):
    """
    Checks a string for evidence that it's a retweet. If so, it returns
    the username of the tweet originator.
    """
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
    """
    Extracts a dict of retweet edges (from original author to retweeter)
    from a list of tweets and a corresponding list of authors.

    groupid should provide an index that aggregates tweets into logical groups.
    If no aggregation is desired, then just pass a list of length from_user with
    a single identifier (e.g., ['placeholder'] * len(from_user)).
    """
    edgelist_dict = {}
    for group, msg_author, msg in zip(group_id, from_user, msg_list):
        mentions = is_mt(msg)
        if mentions:
            try:
                mentions = [m.encode('ascii', 'replace') for m in mentions]
                msg_author = msg_author.encode('ascii', 'replace')
            except:
                continue
            pairs = [(msg_author, m) for m in mentions if m != msg_author]
            for pair in pairs:
                if group in edgelist_dict:
                    if pair in edgelist_dict[group]:
                        edgelist_dict[group][pair] += 1
                    else:
                        edgelist_dict[group][pair] = 1
                else:
                    edgelist_dict[group] = {pair: 1}
        else:
            continue
    return edgelist_dict

def compute_shortest_paths(g, node_list_a, node_list_b, n_paths=1000):
    """
    compute the dijkstra shortest path between n_paths random pairs of
    nodes in a graph, for paths that actually exist.
    """
    idx_a = range(len(node_list_a))
    idx_b = range(len(node_list_b))
    np.random.shuffle(idx_a)
    np.random.shuffle(idx_b)

    counter = 0
    path_lengths = []
    
    for idx in idx_a:
        for jdx in idx_b:
            if node_list_a[idx] != node_list_b[jdx]:
                try:
                    pl = nx.dijkstra_path_length(g,
                                                 node_list_a[idx],
                                                 node_list_b[jdx]
                                                 )
                except nx.NetworkXNoPath:
                    continue
                path_lengths.append(pl)
                counter += 1
                if counter % 100 == 0:
                    print counter
            if counter > n_paths:
                break
        if counter > n_paths:
            break
    return path_lengths

def compute_path_prob(g, node_list_a, node_list_b, n_paths = 10000):
    """
    Given two lists of nodes in a graph g, check to see how many paths exist
    among all paths that _could_ exist. Samples n_paths. 
    """
    np.random.shuffle(node_list_a)
    np.random.shuffle(node_list_b)
    
    actual_path_ct = 0
    total_paths_checked = 0
    for n in node_list_a:
        for m in node_list_b:
            if n != m:
                total_paths_checked += 1
                if total_paths_checked % 10000 == 0:
                    print total_paths_checked
                if nx.has_path(g, n, m):
                    actual_path_ct += 1
                
                if total_paths_checked > n_paths:
                    break
        if total_paths_checked > n_paths:
            break
    path_prob = actual_path_ct / float(n_paths)
    return path_prob

def compute_edge_homophily(graph, labels):
    """
    Given a graph and a set of node labels, determine
    the share of edges that exist between nodes with the same
    label.

    This assumes that labels is a pandas Series of node labels,
    indexed with the node names themselves. 
    """
    edge_count = 0
    homophily = 0
    for e in graph.edges():
        if e[0] in labels.index and e[1] in labels.index:
            edge_count += 1
            e1_label = labels[e[0]]
            e2_label = labels[e[1]]
            if e1_label == e2_label:
                homophily += 1
    homophily_share = float(homophily) / edge_count
    return homophily_share, homophily, edge_count
## END FUNCTION DEF ## 




## Begin analysis
## This script does three things:
## 1. It builds a graph of retweets from the entire set of
##    Twitter data in the 2012 election.
## 2. It plots the largest connected component of that graph,
##    with node colors determined by partisanship as computed
##    in compute_user_partisanship.py.
## 3. It calculates metrics for partisan homophily based on
##    the retweet graphs. 
##

## Load up the tweets and the partisanship data
tweets = pd.read_csv('../../data/master_cron_file_2013_sub.csv')
tweets['text'].fillna(value='', inplace=True)

## Check how many tweets each user released
tweets_grouped = tweets.groupby('from_user')
user_count = tweets_grouped.size()
users_to_use = user_count[user_count > np.mean(user_count)].index

mnb_pship = pd.read_csv('../../data/mnb_user_partisanship.csv')
gnb_pship = pd.read_csv('../../data/gnb_user_partisanship.csv')


## Strip out some extraneous stuff in the tweet data:
extraneous_names = ['Yankees',
                    'ObliviousNFLRef',
                    'stephenasmith'
                    ]
bool_tweets = [False if n in extraneous_names else True
               for n in tweets.from_user
               ]
bool_mnb_pship = [False if n in extraneous_names else True
                  for n in mnb_pship.user
                  ]
bool_gnb_pship = [False if n in extraneous_names else True
                  for n in gnb_pship.user
                  ]

tweets = tweets[bool_tweets]
mnb_pship = mnb_pship[bool_mnb_pship]
gnb_pship = gnb_pship[bool_gnb_pship]

test = pd.merge(tweets, gnb_pship,
                how='inner',
                left_on='from_user',
                right_on='user'
                )

test['district'] = [s[0:4] for s in test.unique_cand_id]
test_grouped = test.groupby('district')
mean_district_partisanship = test_grouped.pship.agg(np.mean)

mean_district_partisanship[1:].to_csv('../../data/district_gnb_partisan_score.csv')

## 1. BUILD GRAPH
## Build the entire retweet edgelist
mt_edgelist = extract_edgelist(['master'] * tweets.shape[0],
                               tweets['from_user'],
                               tweets['text']
                               )

## Construct separate edge and node sets based on the retweet
## edgelist
global_nodedict = {}
global_edgedict = {}
for rtg in mt_edgelist:
    for edge in mt_edgelist[rtg]:
        if edge[0] in users_to_use and edge[1] in users_to_use:
            for node in edge:
                if node not in global_nodedict:
                    global_nodedict[node] = 1
            if edge not in global_edgedict:
                global_edgedict[edge] = mt_edgelist[rtg][edge]
            else:
                global_edgedict[edge] += 1

## Build up the nodes and encode the color as
## the partisan label
global_nodelist = []
gnb_pship.set_index('user', inplace=True)
for node in global_nodedict.keys():
    try:
        ncolor = gnb_pship.ix[node]['pship']
    except KeyError:
        ncolor = 0
    global_nodelist.append((node, {'color': ncolor}))

## Generate the (from, to, weight) edge tuples
global_ebunch = []
for edge in global_edgedict:
    etuple = (edge[0], edge[1], float(1) / global_edgedict[edge])
    global_ebunch.append(etuple)

## Generate the graph from the nodelist and edge bunch
global_mt_graph = nx.DiGraph()
global_mt_graph.add_nodes_from(global_nodelist)
global_mt_graph.add_weighted_edges_from(global_ebunch)

## 1. END BUILD GRAPH
###################################

###################################
## 2. ANALYZE PARTISAN HOMOPHILY

## Sample N node pairs for each of three types:
## Lib:Lib, Con:Con, Lib:Con
## And compute the shortest paths between the
## node pairs

gnb_pship.reset_index(inplace=True)
gnb_pship.set_index('user', inplace=True)
lib_nodes = [n for n in global_mt_graph.nodes() if n in gnb_pship.index and
             gnb_pship.ix[n]['pship_label'] in ['lib', 'very_lib']
             ]
con_nodes = [n for n in global_mt_graph.nodes() if n in gnb_pship.index and
             gnb_pship.ix[n]['pship_label'] in ['con', 'very_con']
             ]
lib_nodes = pd.Series(lib_nodes)
con_nodes = pd.Series(con_nodes)
        
## Compute the shortest paths between
## the node pairs
lib_paths = compute_shortest_paths(global_mt_graph, lib_nodes, lib_nodes)
con_paths = compute_shortest_paths(global_mt_graph, con_nodes, con_nodes)
lib_con_paths = compute_shortest_paths(global_mt_graph,
                                       lib_nodes,
                                       con_nodes
                                       )

## Zip them up and write them out.
lib_path_list = zip(['lib'] * len(lib_paths), lib_paths)
con_path_list = zip(['con'] * len(con_paths), con_paths)
lib_con_path_list = zip(['lib_con'] * len(lib_con_paths),
                        lib_con_paths
                        )
lib_path_list.extend(con_path_list)
lib_path_list.extend(lib_con_path_list)
with open('../../data/gnb_mt_path_lengths.csv', 'wt') as f:
    writer = csv.writer(f)
    writer.writerow(['class', 'length'])
    for p in lib_path_list:
        writer.writerow(p)

# Compute the probabilities that paths
# exist between node pairs from the same or
# different partisans
lib_path_prob = compute_path_prob(global_mt_graph,
                                  lib_nodes,
                                  lib_nodes,
                                  n_paths=1000000
                                  )

con_path_prob = compute_path_prob(global_mt_graph,
                                  con_nodes,
                                  con_nodes,
                                  n_paths=1000000
                                  )

lib_con_path_prob = compute_path_prob(global_mt_graph,
                                      lib_nodes,
                                      con_nodes,
                                      n_paths=1000000
                                      )

# Finally, compute the share of retweets that occur
# between users of the same partisan class.
share, homophile_edges, total_edges = compute_edge_homophily(global_mt_graph,
                                                             gnb_pship.pship_label
                                                             )

## 2. END HOMOPHILY ANALYSIS
###############################################

###############################################
## 3. Lay out and plot a graph of the largest
##    connected component of the retweet graph.

largest_cc = nx.connected_component_subgraphs(global_mt_graph.to_undirected())
largest_cc_mst = nx.minimum_spanning_tree(largest_cc[0])


# Only label nodes with a high degree centrality.
# Generate the degree centrality and sort it
lcc_centrality = nx.degree_centrality(largest_cc[0])
sorted_centrality = sorted(lcc_centrality.iteritems(),
                           key=operator.itemgetter(1),
                           reverse=True
                           )

# Set node (user) labels for a small subset of nodes based
# on centrality
plot_labels = [c[0] if i <= 30 else '' for i,c in enumerate(sorted_centrality)]
plot_label_dict = dict(zip([c[0] for c in sorted_centrality], plot_labels))


## Lay out and plot the graph
mst_layout = nx.graphviz_layout(largest_cc_mst, prog='sfdp')
cc_layout = nx.graphviz_layout(largest_cc[0], prog='sfdp')

## Plot the MST
## Colors equivalent to blue, blue/purple, purple/red, red-red
mnb_colormap = {0:'#000066', 1:'#660066', 2:'#990033', 3:'#FF0000'}
gnb_colormap = {-1:'#000066', 0: '#006600', 1:'#FF0000'}
node_color = nx.get_node_attributes(largest_cc_mst, 'color')
colorvec = [gnb_colormap[node_color[n]] for n in node_color]
nodesize = [np.ceil(np.log10(user_count[n])) for n in largest_cc_mst.nodes()]

nx.draw_networkx_edges(largest_cc_mst,
                       mst_layout,
                       alpha=0.2,
                       width=0.2
                       )
nx.draw_networkx_nodes(largest_cc_mst,
                       mst_layout,
                       nodelist=[n for n in node_color],
                       node_color=colorvec,
                       alpha=0.5,
                       node_size=nodesize,
                       linewidths=0.05
                       )
nx.draw_networkx_labels(largest_cc_mst,
                        mst_layout,
                        labels=plot_label_dict,
                        font_color='green',
                        font_size=6
                        )
plt.axis('off')
plt.savefig('../../figures/user_mt_largest_cc_mst_gnb.pdf',
            bbox_inches='tight'
            )
plt.close()

## Then render as a png w/o labels
nx.draw_networkx_edges(largest_cc_mst,
                       mst_layout,
                       alpha=0.2,
                       width=0.2
                       )
nx.draw_networkx_nodes(largest_cc_mst,
                       mst_layout,
                       nodelist=[n for n in node_color],
                       node_color=colorvec,
                       alpha=0.5,
                       node_size=nodesize,
                       linewidths=0.05
                       )
plt.axis('off')
plt.savefig('../../figures/user_mt_largest_cc_mst_gnb.png',
            bbox_inches='tight'
            )
plt.close()

## Colors equivalent to blue-blue/purple-purple/red-red
mnb_colormap = {0:'#000066', 1:'#660066', 2:'#990033', 3:'#FF0000'}
gnb_colormap = {-1:'#000066', 0: '#006600', 1:'#FF0000'}
node_color = nx.get_node_attributes(largest_cc[0], 'color')
colorvec = [gnb_colormap[node_color[n]] for n in node_color]
nodesize = [np.ceil(np.log10(user_count[n])) for n in largest_cc[0].nodes()]

nx.draw_networkx_edges(largest_cc_mst,
                       cc_layout,
                       alpha=0.2,
                       width=0.2
                       )
nx.draw_networkx_nodes(largest_cc_mst,
                       cc_layout,
                       nodelist=[n for n in node_color],
                       node_color=colorvec,
                       alpha=0.5,
                       node_size=nodesize,
                       linewidths=0.05
                       )
plt.axis('off')
plt.savefig('../../figures/user_mt_largest_cc_gnb.pdf',
            bbox_inches='tight'
            )
plt.close()

## 3. END
#############################
