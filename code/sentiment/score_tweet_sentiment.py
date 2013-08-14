import csv
import numpy as np
import re
import string
import pandas as pd



def score_tweet(tweet, sentiment_dict):
    tweet_words = tweet.lower().split(' ')
    tweet_score = 0
    for word in tweet_words:
        if word in sentiment_dict:
            tweet_score += sentiment_dict[word]
    return tweet_score

## Generates pos/neg counts per O'Connor et al 2009
def oconnor_score(tweet, sentiment_dict):
    tweet_words = tweet.lower().split(' ')
    pos_count = 0
    neg_count = 0
    for word in tweet_words:
        if word in sentiment_dict:
            if sentiment_dict[word] < 0:
                neg_count += 1
            else:
                pos_count += 1

    return pos_count, neg_count


def delete_punctuation(str_in):
    out = ''.join(c for c in str_in if c not in to_exclude)
    return out


## Would be better to read this in as a dict with word:score, solves problems later on
word_score_dict = {}
with(open('../../data/opinionfinder_subj_dict.csv',
          'rU'
          )
     ) as opinionfinder_conn:
    reader = csv.DictReader(opinionfinder_conn, dialect="excel")
    for row in reader:
        if row['pos1']=='adj' and \
            row['priorpolarity'] != 'both' and \
            row['stemmed1']=='n' and \
            row['word1'] not in ['frank']:
                polarity = row['priorpolarity']
                if polarity == 'positive':
                    score = 1
                elif polarity == 'negative':
                    score = -1
                else:
                    score = 0
                word_score_dict[row['word1']] = score

## Add emoticons
emoticons = {':-)':1, ':)':1,
             ':o)':1, ':]':1,
             ':3':1,
             ':c)':1,
             ':>':1, '=]':1,
             '8)':1, '=)':1,
             ':}':1, ':^)':1,
             ':)':1,
             '>:[':-1, ':-(':-1, ':(':-1, ':-c':-1, ':c':-1, ':-<':-1, ':<':-1, ':-[':-1, ':[':-1, ':{':-1
             }

sentiment_dict = dict(word_score_dict, **emoticons)

to_exclude = string.punctuation


## 2012
input_conn = open('/home/markhuberty/projects/twitter_election2012/data/master_cron_file_2013_sub.csv',
                  'rt'
                  )
reader = csv.DictReader(input_conn)
tweets = [row for row in reader]
input_conn.close()


tweet_text = [delete_punctuation(t['text']) if t['text'] else '' for t in tweets]
tweet_sentiment = [oconnor_score(t , sentiment_dict) if isinstance(t, str) else 0
                   for t in tweet_text]

df_sentiment = pd.DataFrame(tweets)
df_sentiment['pos_sentiment'] = [t[0] for t in tweet_sentiment]
df_sentiment['neg_sentiment'] = [t[1] for t in tweet_sentiment]

# Fix some mangled district data
df_sentiment.unique_cand_id[df_sentiment.unique_cand_id=='TX30_D_Bernice Johnson'] = 'TX30_D_Johnson'
df_sentiment.unique_cand_id[df_sentiment.unique_cand_id=='TX18_D_Jackson Lee'] = 'TX18_D_Lee'
df_sentiment.unique_cand_id[df_sentiment.unique_cand_id=='FL07__KDndall'] = 'FL07_D_Kendall'

## Aggregate pos / neg sentment by candidate
sentiment_agg = df_sentiment.groupby('unique_cand_id').agg({'pos_sentiment': np.sum,
                                                      'neg_sentiment': np.sum}
                                                     )

sentiment_agg['party'] = [i[5:6] for i in sentiment_agg.index]
sentiment_agg['dist'] = [i[:4] for i in sentiment_agg.index]
sentiment_agg.reset_index(inplace=True)

# Score sentiment as pos / (pos + neg). Note this departs from
# Oconnor et al b/c of sparsity in the pos/neg ratings
sentiment_agg['sentiment_score'] = [float(p) / (p + n) if p > 0 or n > 0 else 0 for p, n in
                                    zip(sentiment_agg.pos_sentiment,
                                        sentiment_agg.neg_sentiment)]

# Restrict to districts where 2 candidates are present
contested_districts = sentiment_agg.dist.value_counts()
contested_districts = contested_districts[contested_districts == 2 ].index

sentiment_agg = sentiment_agg[sentiment_agg.dist.isin(contested_districts)]

#sentiment_agg.set_index('dist', inplace=True)
sentiment_agg_r = sentiment_agg[sentiment_agg.party=='R']
sentiment_agg_d = sentiment_agg[sentiment_agg.party=='D']
sentiment_agg_d = sentiment_agg_d[sentiment_agg_d.dist != 'NY05'] ## 2 democrats...

sentiment_agg_r.set_index('dist', inplace=True)
sentiment_agg_d.set_index('dist', inplace=True)

sentiment_agg_r['sentiment_ratio'] = (sentiment_agg_r.sentiment_score) / (sentiment_agg_r.sentiment_score + \
                                                                          sentiment_agg_d.sentiment_score)

sentiment_agg_r.to_csv('../../data/r_sentiment_bydistrict_2012.csv')



## 2010
input_conn = open('/home/markhuberty/projects/twitter_election2012/data/master.cron.file.2010.csv',
                  'rt'
                  )
reader = csv.DictReader(input_conn)
tweets = [row for row in reader]
input_conn.close()


tweet_text = [delete_punctuation(t['text']) if t['text'] else '' for t in tweets]
tweet_sentiment = [oconnor_score(t , sentiment_dict) if isinstance(t, str) else 0
                   for t in tweet_text]

df_sentiment = pd.DataFrame(tweets)
df_sentiment['pos_sentiment'] = [t[0] for t in tweet_sentiment]
df_sentiment['neg_sentiment'] = [t[1] for t in tweet_sentiment]

## Aggregate pos / neg sentment by candidate
sentiment_agg = df_sentiment.groupby('unique_cand_id').agg({'pos_sentiment': np.sum,
                                                      'neg_sentiment': np.sum}
                                                     )

sentiment_agg['party'] = [i[5:6] for i in sentiment_agg.index]
sentiment_agg['dist'] = [i[:4] for i in sentiment_agg.index]
sentiment_agg.reset_index(inplace=True)

# Score sentiment as pos / (pos + neg). Note this departs from
# Oconnor et al b/c of sparsity in the pos/neg ratings
sentiment_agg['sentiment_score'] = [float(p) / (p + n) if p > 0 or n > 0 else 0 for p, n in
                                    zip(sentiment_agg.pos_sentiment,
                                        sentiment_agg.neg_sentiment)]

# Restrict to districts where 2 candidates are present
contested_districts = sentiment_agg.dist.value_counts()
contested_districts = contested_districts[contested_districts == 2 ].index

sentiment_agg = sentiment_agg[sentiment_agg.dist.isin(contested_districts)]

#sentiment_agg.set_index('dist', inplace=True)
sentiment_agg_r = sentiment_agg[sentiment_agg.party=='R']
sentiment_agg_d = sentiment_agg[sentiment_agg.party=='D']
sentiment_agg_d = sentiment_agg_d[sentiment_agg_d.dist != 'NY16'] ## 2 democrats...
sentiment_agg_r = sentiment_agg_r[sentiment_agg_r.dist != 'LA03'] ## 3 republicans...

sentiment_agg_r.set_index('dist', inplace=True)
sentiment_agg_d.set_index('dist', inplace=True)

sentiment_agg_r['sentiment_ratio'] = (sentiment_agg_r.sentiment_score) / (sentiment_agg_r.sentiment_score + \
                                                                          sentiment_agg_d.sentiment_score)

sentiment_agg_r.to_csv('../../data/r_sentiment_bydistrict_2010.csv')
