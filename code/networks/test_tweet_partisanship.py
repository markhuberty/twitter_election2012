import csv
import re
import string
import nltk
import sklearn
from sklearn import feature_extraction as fe
from sklearn import cross_validation as cv
from sklearn import naive_bayes as nb
from sklearn import svm as sksvm
import numpy as np


input_conn = open('/Users/markhuberty/Documents/Research/Papers/twitter_election2012/data/tweets_users.csv',
                  'rt'
                  )
reader = csv.DictReader(input_conn)
tweets = [row for row in reader]
input_conn.close()

## Based on http://www.epjdatascience.com/content/1/1/6
def return_partisan_hashtags(tweets, seed_tag):
    seed_tag_idx = []
    other_tag_idx_dict = {}
    for i,t in enumerate(tweets):
        if seed_tag in t:
            seed_tag_idx.append(i)
        other_tags = [word for word in t.split() if word.startswith('#')]
        if other_tags:
            for tag in other_tags:
                if tag != seed_tag:
                    if tag in other_tag_idx_dict:
                        other_tag_idx_dict[tag].append(i)
                    else:
                        other_tag_idx_dict[tag] = [i]
    return seed_tag_idx, other_tag_idx_dict

def return_partisan_jaccard(tweets, seed_tag, threshold=0.01):
    seed_idx, other_idx = return_partisan_hashtags(tweets, seed_tag)
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

lib_tags = return_partisan_jaccard(training_tweets, 'p2')
con_tags = return_partisan_jaccard(training_tweets, 'tcot')

lib_re = '|'.join([t[0].lower() for t in lib_tags]) + '|#p2'
con_re = '|'.join([t[0].lower() for t in con_tags]) + '|#tcot'
neu_re = 'poll|election|news'

re_lib = re.compile(lib_re)
re_con = re.compile(con_re)
re_neu = re.compile(neu_re)

lib_tweets = [True if re_lib.search(t['text'].lower()) else False for t in tweets]
con_tweets = [True if re_con.search(t['text'].lower()) else False for t in tweets]
neu_tweets = [True if re_neu.search(t['text'].lower()) else False for t in tweets]


lib_ct = 0
con_ct = 0
either_ct = 0
both_ct = 0
neither_ct = 0
neu_ct = 0
for l, c, n in zip(lib_tweets, con_tweets, neu_tweets):
    if l and c:
        both_ct += 1
    elif l and not c:
        lib_ct += 1
        either_ct += 1
    elif c and not l:
        con_ct += 1
        either_ct += 1
    elif not l and not c:
        neither_ct += 1
        if n:
            neu_ct += 1


training_tweets = []
training_labels = []
for i, t in enumerate(tweets):
    if lib_tweets[i] and not con_tweets[i]:
        t_out = re_lib.sub(' ', t['text'].lower())
        training_labels.append('lib')
        training_tweets.append(t_out)
    if con_tweets[i] and not lib_tweets[i]:
        t_out = re_lib.sub(' ', t['text'].lower())
        training_labels.append('con')
        training_tweets.append(t_out)
    if not con_tweets[i] and not lib_tweets[i] and neu_tweets[i]:
        t_out = re_neu.sub(' ', t['text'].lower())
        training_labels.append('neu')
        training_tweets.append(t_out)

training_tweets = [unicode(t, errors='ignore') for t in training_tweets]


## Generate the romanized / lowercase / no punctuation text
tweet_count_vectorizer = fe.text.CountVectorizer(ngram_range=(2,2))
vectorizer = tweet_count_vectorizer.fit(training_tweets)
vectorized_counts = vectorizer.transform(training_tweets)

## Then execute a tfidf transform
tweet_tfidf_transform = fe.text.TfidfTransformer(use_idf=True)
transformer = tweet_tfidf_transform.fit(vectorized_counts)
tfidf_counts = transformer.transform(vectorized_counts)

## Finally subset
train_data, test_data, train_labels, test_labels, train_tweets, test_tweets = cv.train_test_split(
    tfidf_counts, training_labels, training_tweets, test_size=0.1
    )

## Then generate the multinomial naive bayes classifier
gnb = nb.MultinomialNB()

## This doesn't do very well at all. Hashtags as labels
## might suck; too little data in the tweets.
tweet_classifier = gnb.fit(train_data, train_labels)
pred_train = tweet_classifier.predict(train_data)
pred_test = tweet_classifier.predict(test_data)

## Now label everything and write it out

all_tweets = [unicode(t['text'].lower(), errors='ignore') for t in tweets]
all_counts = transformer.transform(vectorizer.transform(all_tweets))
all_labels = tweet_classifier.predict(all_counts)

neu_ct, con_ct, lib_ct = 0,0,0
for l in all_labels:
    if l == 'lib':
        lib_ct += 1
    if l == 'con':
        con_ct += 1
    if l == 'neu':
        neu_ct += 1
print lib_ct, con_ct, neu_ct

all_neu = [t for t,l in zip(all_tweets, all_labels) if l=='neu']
all_con = [t for t,l in zip(all_tweets, all_labels) if l=='con']
all_lib = [t for t,l in zip(all_tweets, all_labels) if l=='lib']


## Then walk across the user set and accumulate a partisanship score
def compute_partisanship(users, partisan_labels, msg_threshold=5):
    u_score = {}
    u_piecewise_score = {}
    for u, l in zip(users, partisan_labels):
        if u not in u_piecewise_score:
            u_piecewise_score[u] = {'con':0, 'lib':0, 'neu':0}
        u_piecewise_score[u][l] += 1
    for u in u_piecewise_score:
        scores = u_piecewise_score[u]
        # print scores
        total_score = 1 * scores['con'] + -1 * scores['lib']
        score_denom = scores['con'] + scores['lib'] + 1
        if(score_denom >= (msg_threshold - 1)):
            u_score[u] = total_score / float(score_denom)
    return u_score # u_piecewise_score

users = [t['from_user'] for t in tweets]

user_partisanship = compute_partisanship(users, all_labels)

import operator
sorted_partisanship = sorted(user_partisanship.iteritems(),
                             key=operator.itemgetter(1),
                             reverse=True
                             )

output_conn = open('../../data/user_partisanship.csv', 'wt')
writer = csv.writer(output_conn)
writer.writerow(['user', 'pscore'])
for user in user_partisanship:
    out = [user, user_partisanship[user]]
    writer.writerow(out)
output_conn.close()


