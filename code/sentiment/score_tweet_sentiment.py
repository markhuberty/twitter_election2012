import csv
import re
import string


def score_tweet(tweet, sentiment_dict):
    tweet_words = tweet.split(' ')
    tweet_score = 0
    for word in tweet_words:
        if word in sentiment_dict:
            tweet_score += sentiment_dict[word]
    return tweet_score


input_conn = open('/Users/markhuberty/Documents/Research/Papers/twitter_election2012/data/all_tweets.csv',
                  'rt'
                  )
reader = csv.DictReader(input_conn)
tweets = [row for row in reader]
input_conn.close()

## Would be better to read this in as a dict with word:score, solves problems later on
word_score_dict = {}
with(open('/Users/markhuberty/Documents/Research/Papers/twitter_election2012/data/opinionfinder_wordlist.csv',
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

tweet_text = [t['text'].lower() for t in tweets]
tweet_sentiment = [score_tweet(t , sentiment_dict) for t in tweet_text]

sentiments = zip(tweet_text, tweet_sentiment)
