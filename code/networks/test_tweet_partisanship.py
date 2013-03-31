import csv
import re
import string
import sklearn
from sklearn import feature_extraction as fe
from sklearn import cross_validation as cv
from sklearn import naive_bayes as nb
import numpy as np
import pandas as pd
import operator
import copy
import nltk

## Based on http://www.epjdatascience.com/content/1/1/6
def return_hashtag_index(tweets, seed_tag):
    """
    Returns the occurrance indices of a seed hashtag and all other hashtags
    in a list of tweets

    tweets: a character vector of tweets
    seed_tag: a master hashtag

    Output is of the form seed_indices, dict_of_other_indices
    """
    seed_tag_idx = []
    other_tag_idx_dict = {}
    for i,t in enumerate(tweets):
        if seed_tag in t:
            seed_tag_idx.append(i)
        other_tags = [word for word in t.split()
                      if word.startswith('#') and len(word) > 1]
        if other_tags:
            for tag in other_tags:
                if tag != seed_tag:
                    if tag in other_tag_idx_dict:
                        other_tag_idx_dict[tag].append(i)
                    else:
                        other_tag_idx_dict[tag] = [i]
    return seed_tag_idx, other_tag_idx_dict

def return_jaccard_similar_tags(tweets, seed_tag, threshold=0.01):
    """
    Given a seed tag, returns similar tags from a list of tweets based
    on the jaccard score of tag co-occurance

    tweets: a character vector of twitter messages
    seed_tag: a hashtag for which we will find similar tags
    threshold: the minimum jaccard score for similarity
    """
    seed_idx, other_idx = return_hashtag_index(tweets, seed_tag)
    seed_idx_set = set(seed_idx)
    other_tag_jaccard = {}
    for tag in other_idx:
        numerator = set(other_idx[tag]) & seed_idx_set
        denom = other_idx[tag] + seed_idx
        denom = len(set(denom))
        jaccard = float(len(numerator)) / denom
        other_tag_jaccard[tag] = jaccard
    sorted_jaccard = sorted(other_tag_jaccard.iteritems(),
                            key=operator.itemgetter(1),
                            reverse=True
                            )
    out = [s for s in sorted_jaccard if s[1] >= threshold]
    return out

def compute_partisan_sentiment(entity,
                               partisan_labels,
                               sentiment_labels,
                               msg_threshold=5,
                               denom_exclude_labels = ['neu'],
                               agg_fun='mean'
                               ):
    """
    Returns a partisan sentiment score (lib/con/neu * score) for each unique
    entity. Scores are determined by the agg_fun (e.g., sum, median, mean, etc)
    
    """
    this_df = pd.DataFrame({'entity': entity, 'plabel': partisan_labels, 'slabel': sentiment_labels})
    #this_df.set_index(['entity', 'plabel'], inplace=True)

    grouped = this_df.groupby(['entity', 'plabel'])
    grouped_size = grouped.size()
    to_retain = grouped_size.index[grouped_size > msg_threshold]

    # grouped_score = grouped.aggregate(agg_fun)

    # df_out = grouped_score.reset_index()
    # return df_out
    return grouped

def compute_partisanship(entity, partisan_labels, sentiment_labels,
                         label_scores={'con':1, 'lib':-1, 'neu':0},
                         msg_threshold=5,
                         denom_exclude_labels = ['neu']
                         ):
    """
    Returns a per-user (or entity) partisanship score for tweets with
    partisan_labels. Scores are on the interval -1, 1

    entity: vector of entities for objects assigned labels
    partisan_labels: character vector of labels
    label_scores: dict mapping from each value in partisan_labels to a score
    msg_threshold: minimum number of times a given value must appear in entity for the score to
    be returned
    """
    u_score = {}
    u_piecewise_score = {}
    score_template = dict(zip(label_scores.keys(),
                              [0] * len(label_scores.keys())
                              )
                          )
    for u, l in zip(entity, partisan_labels):
        if u not in u_piecewise_score:
            u_piecewise_score[u] = copy.deepcopy(score_template)
        u_piecewise_score[u][l] += 1
    for u in u_piecewise_score:
        scores = u_piecewise_score[u]
        # print u, scores
        numerator_score = 0
        denominator_score = 0
        for k in label_scores:
            # print label_scores[k], scores[k]
            numerator_score += label_scores[k] * scores[k]
            if k not in denom_exclude_labels:
                denominator_score += scores[k]
            # print numerator_score, denominator_score
        if(denominator_score >= (msg_threshold)):
            u_score[u] = numerator_score / float(denominator_score)
    return u_score # u_piecewise_score

def score_tweet(tweet, sentiment_dict):
    tweet_words = tweet.split(' ')
    tweet_score = 0
    for word in tweet_words:
        if word in sentiment_dict:
            tweet_score += sentiment_dict[word]
    return tweet_score

def score_user(tweets, users, tag_dict):
    out = {}
    for t, u in zip(tweets, users):
        if u not in out:
            out[u] = {'n_total':0}
        for h in tag_dict:
            if h in t.lower():
                out[u]['n_total'] += 1
                if tag_dict[h] in out[u]:
                    out[u][tag_dict[h]] += 1
                else:
                    out[u][tag_dict[h]] = 1
    return out
        

## Read in the text data
tweets = pd.read_csv('../../data/master_cron_file_2013_sub.csv')
tweets['text'].fillna(value='', inplace=True)

noise_terms = ["mlb",
               "kicker",
               "orleans",
               "yankee",
               "nfl",
               "yankees",
               "baseball",
               "football",
               "orioles",
               "touchdown",
               "sports",
               "coach",
               "Yankees",
               "ObliviousNFLRef"
               ]

noise_bool = []
for t in tweets['text'].values:
    bool_vec = [True if n in t.lower() else False for n in noise_terms]
    if any(bool_vec):
        noise_bool.append(False)
    else:
        noise_bool.append(True)

tweets = tweets[noise_bool]
tweets['dist'] = [d[0:4] for d in tweets.unique_cand_id]
tweets['text'].fillna('', inplace=True)

## Find liberal and conservative tags
lib_tags = return_jaccard_similar_tags(tweets['text'],
                                       'p2',
                                       threshold=0.01
                                       )
con_tags = return_jaccard_similar_tags(tweets['text'],
                                       'tcot',
                                       threshold=0.01
                                       )
lib_tags = [(l[0].lower(), l[1]) for l in lib_tags]
con_tags = [(c[0].lower(), c[1]) for c in con_tags]
lib_tags = dict(lib_tags)
con_tags = dict(con_tags)

## Eliminate any overlaps; note the hand-coding required
## to deal with some obvious false positives
lib_tag_subset = lib_tags.copy()
con_tag_subset = con_tags.copy()
confusion_tags = ['#gop', '#tcot', '#gop2012', '#ocra']
for l in lib_tags:
    if l in con_tags:
        if lib_tags[l] > con_tags[l] and l not in confusion_tags:
            del con_tag_subset[l]
        else: 
            del lib_tag_subset[l]
    else:
        if l in confusion_tags:
            con_tag_subset[l] = 1
            del lib_tag_subset[l]

## Score users
hashtag_dict = {k:'lib' for k in lib_tag_subset}
hashtag_dict = dict(hashtag_dict, **{k:'con' for k in con_tag_subset})

user_alignment = score_user(tweets['text'].values, tweets['from_user'].values, hashtag_dict)

all_users, con_users, lib_users = {}, {}, {}

for u, v in user_alignment.iteritems():
    if isinstance(u, str):
        if 'con' in v and 'lib' in v:
            if v['con'] > v['lib']:
                con_users[u] = v['con'] / float(v['n_total'])
                all_users[u] = v['con'] / float(v['n_total'])
            elif v['con'] < v['lib']:
                lib_users[u] = v['lib'] / float(v['n_total'])
                all_users[u] = -1 * v['lib'] / float(v['n_total'])
            elif 'lib' in v:
                lib_users[u] = -1 * v['lib'] / float(v['n_total'])
            elif 'con' in v:
                con_users[u] = v['con'] / float(v['n_total'])

## dump this for kicks
with open ('../../data/user_alignment.csv', 'wt') as f:
    writer = csv.writer(f)
    writer.writerow(['username', 'score'])
    for u, s in all_users.iteritems():
        writer.writerow((u,s))


## Munge them into regex
lib_re = '|'.join([t.lower() for t in lib_tag_subset]) + '|#p2'
con_re = '|'.join([t.lower() for t in con_tag_subset]) + '|#tcot'
neu_re = 'poll|election|news'

re_lib = re.compile(lib_re)
re_con = re.compile(con_re)
re_neu = re.compile(neu_re)

## Find the lib / con / neutral tweets
lib_tweets = [True if re_lib.search(t.lower()) else False for t in tweets['text']]
con_tweets = [True if re_con.search(t.lower()) else False for t in tweets['text']]
neu_tweets = [True if re_neu.search(t.lower()) else False for t in tweets['text']]

l = [t for i,t in enumerate(tweets['text']) if lib_tweets[i]]

## Take as "training" a restrictive set where 'liberal' is only
## 'liberal' if only 'liberal' tags are found, and similarly for conservative
## Remove the hashtags so we don't just rediscover the labels
training_tweets = []
training_labels = []
re_hashtag = re.compile("#\w+")
re_url = re.compile('http\://[\w\./]+')

for i, t in enumerate(tweets['text']):
    if lib_tweets[i] and not con_tweets[i]:
        t_out = re_url.sub('LINK', re_lib.sub(' ', t.lower()))
        training_labels.append('lib')
        training_tweets.append(t_out)
    elif con_tweets[i] and not lib_tweets[i]:
        t_out = re_url.sub('LINK', re_con.sub(' ', t.lower()))
        training_labels.append('con')
        training_tweets.append(t_out)
    elif not con_tweets[i] and not lib_tweets[i] and neu_tweets[i]:
        t_out = re_url.sub('LINK', re_neu.sub(' ', t.lower()))
        training_labels.append('neu')
        training_tweets.append(t_out)

neu_ct = 0
lib_ct = 0
con_ct = 0
for l in training_labels:
    if l == 'neu':
        neu_ct += 1
    if l == 'con':
        con_ct += 1
    if l == 'lib':
        lib_ct += 1
print lib_ct, neu_ct, con_ct

training_tweets = [unicode(t.lower(), errors='ignore') for t in training_tweets]


## Generate the romanized / lowercase / no punctuation text
tweet_count_vectorizer = fe.text.CountVectorizer(ngram_range=(1,1))
vectorizer = tweet_count_vectorizer.fit(training_tweets)
vectorized_counts = vectorizer.transform(training_tweets)

## Tfidf transform
tweet_tfidf_transform = fe.text.TfidfTransformer(use_idf=True)
transformer = tweet_tfidf_transform.fit(vectorized_counts)
tfidf_counts = transformer.transform(vectorized_counts)

## Subset into training and evaluation data
train_data, test_data, train_labels, test_labels, train_tweets, test_tweets = cv.train_test_split(
    tfidf_counts, training_labels, training_tweets, test_size=0.1
    )

## Then generate the multinomial naive bayes classifier
gnb = nb.MultinomialNB()
tweet_classifier = gnb.fit(train_data, train_labels)
pred_train = tweet_classifier.predict(train_data)
pred_test = tweet_classifier.predict(test_data)

## Now label everything and write it out
all_tweets = [unicode(t.lower(), errors='ignore') for t in tweets['text']]
all_tweets = [re_url.sub('LINK', t) for t in all_tweets]
all_vectorized_counts = vectorizer.transform(all_tweets)
all_tfidf_counts = transformer.transform(all_vectorized_counts)
all_labels = tweet_classifier.predict(all_tfidf_counts)

#all_counts = transformer.transform(vectorizer.transform(all_tweets))
#all_labels = tweet_classifier.predict(all_counts)

neu_ct = 0
lib_ct = 0
con_ct = 0
for l in all_labels:
    if l == 'neu':
        neu_ct += 1
    if l == 'con':
        con_ct += 1
    if l == 'lib':
        lib_ct += 1
print lib_ct, neu_ct, con_ct

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
                this_key = r'%s' % row['word1']
                word_score_dict[this_key] = score

# Add emoticons
# Taken from the Wikipedia happy/sad emoticons list
emoticons = {r':-)':1,
             r':)':1,
             r':o)':1,
             r':]':1,
             r':3':1,
             r':c)':1,
             r':>':1,
             r'=]':1,
             r'8)':1,
             r'=)':1,
             r':}':1,
             r':^)':1,
             r':)':1,
             r'>:[':-1,
             r':-(':-1,
             r':(':-1,
             r':-c':-1,
             r':c':-1,
             r':-<':-1,
             r':<':-1,
             r':-[':-1,
             r':[':-1,
             r':{':-1
             }

sentiment_dict = dict(word_score_dict, **emoticons)

## Then score each tweet for sentiment
all_sentiment = [score_tweet(t, sentiment_dict) for t in all_tweets]

tweets['sentiment'] = all_sentiment
tweets['partisanship'] = all_labels
## for partisanship: group by labels, sum scores, so get lib/con/neu sentiment scores
## then will have the 2x2 implicitly.

## Then estimate partisanship scores by user, candidate, and district
user_partisanship = compute_partisan_sentiment(tweets['from_user'],
                                               all_labels,
                                               all_sentiment,
                                               agg_fun='np.sum'
                                               )

pship_scores = user_partisanship.aggregate(sum)
pship_scores.reset_index(inplace=True)



## And finally get entities
tweet_entities = []
for t in tweets['text'].values:
    chunks = []
    t = re.sub('Rep\.*', '', t)
    t = re.sub('Republican', '', t)
    t = re.sub('Democrat', '', t)
    for chunk in nltk.ne_chunk(nltk.pos_tag(nltk.word_tokenize(t))):
        if hasattr(chunk, 'node'):
            if chunk.node == 'PERSON' or chunk.node == 'NE':
                temp = (chunk.node, ' '.join(c[0] for c in chunk.leaves()))
                chunks.append(temp)
    tweet_entities.append((t, chunks))
        
user_partisanship = compute_partisanship(tweets['from_user'], all_labels, msg_threshold=10)
candidate_partisanship = compute_partisanship(tweets['unique_cand_id'], all_labels, msg_threshold=10)

dist_label = [cid.split('_')[0] for cid in tweets['unique_cand_id']]
dist_partisanship = compute_partisanship(dist_label, all_labels, msg_threshold=10)

candidates = pd.read_csv('/Users/markhuberty/Documents/Research/Papers/twitter_election2012/data/candidates.final.2012.csv')

def get_entity_parties(tweet_text, names, parties):
    t_parties = []
    for name, party in zip(names, parties):
        if name in t:
            t_parties.append(party)
    return t_parties

test = []
for t in tweets.text:
    temp = get_entity_parties(t, candidates.name, candidates.party)
    test.append(temp)

## Sort for inspection
sorted_user_pship = sorted(user_partisanship.iteritems(),
                           key=operator.itemgetter(1),
                           reverse=True
                           )
sorted_cand_pship = sorted(candidate_partisanship.iteritems(),
                           key=operator.itemgetter(1),
                           reverse=True
                           )
sorted_dist_pship = sorted(dist_partisanship.iteritems(),
                           key=operator.itemgetter(1),
                           reverse=True
                           )

## And write out
pd.DataFrame(sorted_user_pship, columns=['user', 'pscore']).to_csv('../../data/user_partisanship.csv')
pd.DataFrame(sorted_cand_pship, columns=['cand', 'pscore']).to_csv('../../data/cand_partisanship.csv')
pd.DataFrame(sorted_dist_pship, columns=['dist', 'pscore']).to_csv('../../data/dist_partisanship.csv')


