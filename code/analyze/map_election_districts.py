import pandas as pd
import numpy as np
import os
os.chdir('/home/markhuberty/projects/twitter_election2012')

results_2008 = pd.read_csv('./results/house_results_2008.csv')
results_2008['vote_pct'] = results_2008.vote_pct * 100
results_2010 = pd.read_csv('./results/house_results_2010_fec_2.csv')
results_2012 = pd.read_csv('./results/house_results_2012.csv')
results_2012['party'] = [p[0] for p in results_2012.party_abbrev]
results_2012['cand_first_name_trunc'] = [n.split(' ')[0] for n in
                                         results_2012.name]

results_2008 = results_2008[results_2008.vote_pct > 0.0]
results_2010 = results_2010[results_2010.vote_pct > 0.0]

results_2008 = results_2008[results_2008.dist != 'S']
results_2010 = results_2010[results_2010.dist != 'S']

party_abbrev = [p[0] for p in results_2010.party]
results_2010['party'] = party_abbrev


def top_two(df, col, n):
    df.sort(col, inplace=True)
    out = df[-n:]
    return out

def subset_to_top_votes(g, col, n=2):

    list_out = []
    for name, group in g:
        list_out.append(top_two(group, col, n))

    return pd.concat(list_out)

df_2008 = subset_to_top_votes(results_2008[results_2008.party.isin(['D', 'R'])].groupby(['state', 'dist']), 'vote_pct')
df_2010 = subset_to_top_votes(results_2010[results_2010.party.isin(['D', 'R'])].groupby(['state', 'dist']), 'vote_pct')
df_2012 = subset_to_top_votes(results_2012[results_2012.party.isin(['D', 'R'])].groupby('state_dist'), 'vote_pct')

# df_2008 = results_2008[results_2008.party.isin(['D', 'R'])]
# df_2010 = results_2010[results_2010.party.isin(['D', 'R'])]
# df_2012 = results_2012[results_2012.party.isin(['D', 'R'])] 

df_2008.set_index(['state', 'dist'], inplace=True)
vote_totals = df_2008.groupby(df_2008.index).vote_pct.sum() 
df_2008['vote_pct_norm'] = df_2008.vote_pct / vote_totals * 100
df_2008.reset_index(inplace=True)

df_2010.set_index(['state', 'dist'], inplace=True)
vote_totals = df_2010.groupby(df_2010.index).vote_pct.sum() 
df_2010['vote_pct_norm'] = df_2010.vote_pct / vote_totals * 100
df_2010.reset_index(inplace=True)

df_2012.set_index('state_dist', inplace=True)
vote_totals = df_2012.groupby(df_2012.index).vote_pct.sum()
df_2012['vote_pct_norm'] = df_2012.vote_pct / vote_totals * 100


df_2008_winner = subset_to_top_votes(df_2008.groupby(['state', 'dist']), 'vote_pct_norm', 1)
df_2010_winner = subset_to_top_votes(df_2010.groupby(['state', 'dist']), 'vote_pct_norm', 1)
df_2012.reset_index(inplace=True)

# This is the easy one, b/c the district numbering hasn't changed. 
map_2008_2010 = pd.merge(df_2008_winner,
                         df_2010,
                         left_on=['state', 'dist', 'party', 'cand_name_last'],
                         right_on=['state', 'dist', 'party', 'cand_last_name'],
                         how='inner'
                         )


map_2008_2010['r_vote_2008'] = [v if p=='R' else 100-v
                                for p, v in zip(map_2008_2010.party,
                                                map_2008_2010.vote_pct_norm_x)
                                ]

map_2008_2010['r_vote_2010'] = [v if p=='R' else 100-v
                                for p, v in zip(map_2008_2010.party,
                                                map_2008_2010.vote_pct_norm_y)
                                ]

map_2008_2010 = map_2008_2010[['state', 'dist', 'r_vote_2008', 'r_vote_2010']]
map_2008_2010 = map_2008_2010.drop_duplicates()

map_2008_2010['state_dist'] = [s + d for s, d in zip(map_2008_2010.state,
                                                     map_2008_2010.dist)]

df_2010_winner['cand_first_name_trunc'] = [n.split(' ')[0] for n in
                                           df_2010_winner.cand_first_name]

map_2010_2012 = pd.merge(df_2010_winner,
                         df_2012,
                         left_on=['state', 'party', 'cand_first_name_trunc', 'cand_last_name'],
                         right_on=['state', 'party', 'cand_first_name_trunc', 'last_name'],
                         how='inner'
                         )

map_2010_2012['r_vote_2010'] = [v if p=='R' else 100 - v for p, v in
                                zip(map_2010_2012.party, map_2010_2012.vote_pct_norm_x)]
map_2010_2012['r_vote_2012'] = [v if p=='R' else 100 - v for p, v in
                                zip(map_2010_2012.party, map_2010_2012.vote_pct_norm_y)]

map_2010_2012 = map_2010_2012[['state', 'dist', 'state_dist', 'r_vote_2010', 'r_vote_2012']]
map_2010_2012 = map_2010_2012.drop_duplicates()

map_2008_2010['state_dist'] = [s + '0' + str(d) if len(d) < 2 else s + str(d) for
                            s, d in zip(map_2008_2010.state, map_2008_2010.dist)]
map_2010_2012['state_dist_prior'] = [s + '0' + str(d) if len(d) < 2 else s + str(d) for
                                     s, d in zip(map_2010_2012.state, map_2010_2012.dist)]

map_2008_2010.to_csv('./results/map_2008_2010_results.csv', index=False)
map_2010_2012.to_csv('./results/map_2010_2012_results.csv', index=False)
