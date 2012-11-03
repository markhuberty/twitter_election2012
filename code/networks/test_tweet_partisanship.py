import csv
import re
import string
import sklearn
from sklearn import feature_extraction as fe
from sklearn import cross_validation as cv
from sklearn import naive_bayes as nb
import numpy as np
import pandas as pds
import operator
import copy

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

def compute_partisanship(entity, partisan_labels, label_scores={'con':1, 'lib':-1, 'neu':0},
                         msg_threshold=5, denom_exclude_labels = ['neu']):
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

## Read in the text data
tweets = pds.read_csv('../../data/doc_term_mat/house_data.csv')

## Find liberal and conservative tags
lib_tags = return_jaccard_similar_tags(tweets['text'],
                                       'p2',
                                       threshold=0.01
                                       )
con_tags = return_jaccard_similar_tags(tweets['text'],
                                       'tcot',
                                       threshold=0.01
                                       )
lib_tags = dict(lib_tags)
con_tags = dict(con_tags)

## Eliminate any overlaps; note the hand-coding required
## to deal with some obvious false positives
lib_tag_subset = lib_tags.copy()
con_tag_subset = con_tags.copy()
confusion_tags = ['#GOP', '#tcot', '#gop2012', '#ocra']
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
training_tweets = []
training_labels = []
for i, t in enumerate(tweets['text']):
    if lib_tweets[i] and not con_tweets[i]:
        t_out = re_lib.sub(' ', t.lower())
        training_labels.append('lib')
        training_tweets.append(t_out)
    elif con_tweets[i] and not lib_tweets[i]:
        t_out = re_lib.sub(' ', t.lower())
        training_labels.append('con')
        training_tweets.append(t_out)
    elif not con_tweets[i] and not lib_tweets[i] and neu_tweets[i]:
        t_out = re_neu.sub(' ', t.lower())
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

training_tweets = [unicode(t, errors='ignore') for t in training_tweets]


## Generate the romanized / lowercase / no punctuation text
tweet_count_vectorizer = fe.text.CountVectorizer(ngram_range=(2,2))
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
all_counts = transformer.transform(vectorizer.transform(all_tweets))
all_labels = tweet_classifier.predict(all_counts)

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


## Then estimate partisanship scores by user, candidate, and district
user_partisanship = compute_partisanship(tweets['from_user'], all_labels, msg_threshold=10)
candidate_partisanship = compute_partisanship(tweets['unique_cand_id'], all_labels, msg_threshold=10)

dist_label = [cid.split('_')[0] for cid in tweets['unique_cand_id']]
dist_partisanship = compute_partisanship(dist_label, all_labels, msg_threshold=10)

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
pds.DataFrame(sorted_user_pship, columns=['user', 'pscore']).to_csv('../../data/user_partisanship.csv')
pds.DataFrame(sorted_cand_pship, columns=['cand', 'pscore']).to_csv('../../data/cand_partisanship.csv')
pds.DataFrame(sorted_dist_pship, columns=['dist', 'pscore']).to_csv('../../data/dist_partisanship.csv')


