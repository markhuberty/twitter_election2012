from nltk import bigrams
from nltk.corpus import stopwords
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.stem.porter import PorterStemmer
import csv
import re
import time
import string
import numpy as np
from scipy.stats.mstats import mquantiles

def extract_bigram_dict(msgs, words, sw=stopwords.words('english')):
    punct_reg = '[' + string.punctuation + ']'
    formatted_words = [' ' + w + ' ' for w in words]
    bigram_dict = {}
    bigram_freqs = []
    for m in msgs:
        m = re.sub(punct_reg, '', m)
        m_tokens = m.lower().split()
        m_tokens = [t for t in m_tokens if t not in sw]
        m_bigrams = bigrams(m_tokens)
        m_bigrams = [sorted(mb) for mb in m_bigrams]
        m_bigrams_out = [' '.join(mb) for mb in m_bigrams
                         if mb[0] in words or mb[1] in formatted_words]
        for bigram in m_bigrams_out:
            if bigram in bigram_dict:
                bigram_dict[bigram] += 1
            else:
                bigram_dict[bigram] = 1
    return(bigram_dict)

for k in test.keys():
    if test[k] < term_freq_cutoff:
        del(test[k])

def get_bigrams(msg, punct, words, sw):
    m = re.sub(punct, '', msg)
    m_tokens = m.lower().split()
    m_tokens = [t for t in m_tokens if t not in sw]
    m_bigrams = bigrams(m_tokens)
    # m_bigrams = [sorted(mb) for mb in m_bigrams]
    # m_bigrams_out = [' '.join(mb) for mb in m_bigrams
    #                  if mb[0] in words or mb[1] in words]
    # return m_bigrams_out
    return m_bigrams

def get_sentiment_scores(terms, scores):
    term_score_dict = {}
    for t in terms:
        if t[0] in scores:
            term_score_dict[t[1]] = scores[t[0]]
        # elif t[1] in scores:
        #     term_score_dict[t[0]] = scores[t[1]]
        else:
            continue
    return term_score_dict

def count_candidate_sentiment(cand_ids, msgs, wordscore_dict, sw=stopwords.words('english')):
    punct_reg = '[' + string.punctuation + ']'
    formatted_words = [' ' + w + ' ' for w in wordscore_dict.keys()]
    cand_scores = {}
    total_terms = {}
    for c, m in zip(cand_ids, msgs):
        msg_bigrams = get_bigrams(m, punct_reg, formatted_words, sw)
        msg_scores = get_sentiment_scores(msg_bigrams, wordscore_dict)
        for term in msg_scores:
            if c in cand_scores:
                if term in cand_scores[c]:
                    cand_scores[c][term] += msg_scores[term]
                else:
                    cand_scores[c][term] = msg_scores[term]
            else:
                cand_scores[c] = {}
                cand_scores[c][term] = msg_scores[term]
            if c in total_terms:
                total_terms[c] += 1
            else:
                total_terms[c] = 1
    return cand_scores, total_terms


## Filter. First stem terms, then count by stem and keep only
## words w/ stems above a threshold
def filter_stems(cand_word_counts, threshold_quantile=0.9):
    stmr = PorterStemmer()
    stem_counts = {}
    word_stem_map = {}
    for cand in cand_word_counts:
        for term in cand_word_counts[cand]:
            word_stem = stmr.stem(term)
            if word_stem in word_stem_map and
            term not in word_stem_map[word_stem]:
                word_stem_map[word_stem].append(term)
            else:
                word_stem_map[word_stem] = [term]
            if word_stem in stem_counts:
                stem_counts[word_stem] += 1
            else:
                stem_counts[word_stem] = 1
    all_counts = [stem_counts[stem] for stem in stem_counts]
    threshold = mquantiles(all_counts, prob=threshold_quantile)
    filtered_stems = [stem for stem in stem_counts if stem_counts[stem] >= threshold]
    terms_to_keep = {}
    for stem in filtered_stems:
        terms_to_keep[stem] = word_stem_map[stem]
    return terms_to_keep, stem_counts


## Now, with the filtered stems, subset and sum the candidate terms
def count_stem_sentiment(cand_word_counts, stem_dict):
    cand_stem_counts = {}
    for cand in cand_word_counts:
        cand_stem_counts[cand] = {}
        for stem in stem_dict:
            for term in stem_dict[stem]:
                if term in cand_word_counts[cand]:
                    if stem in cand_stem_counts[cand]:
                        cand_stem_counts[cand][stem] += cand_word_counts[cand][term]
                    else:
                        cand_stem_counts[cand][stem] = cand_word_counts[cand][term]
    return cand_stem_counts



## Test output

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

cand_id_vec = [t['unique_cand_id'] for t in tweets]
msg_vec = [t['text'] for t in tweets]

candidate_sentiment_scores, total_sentiment_terms = \
    count_candidate_sentiment(cand_id_vec, msg_vec, word_score_dict)

filtered_terms, term_counts = \
    filter_stems(candidate_sentiment_scores, threshold_quantile=0.975)

test_stem = count_stem_sentiment(candidate_sentiment_scores, filtered_terms)


## Write out the result
with open('/Users/markhuberty/Documents/Research/Papers/twitter_election2012/data/candidate_word_sentiments.csv',
          'wt'
          ) as output_conn: 
    writer = csv.writer(output_conn)
    writer.writerow(['unique_cand_id', 'state_dist', 'party', 'last_name', 'stem', 'score', 'score_norm'])
    for cand in test_stem:
        state_dist, party, last_name = cand.split('_')
        for stem in test_stem[cand]:
            score_norm = test_stem[cand][stem] / float(total_sentiment_terms[cand]) * 100
            row = [cand, state_dist, party, last_name, stem, str(test_stem[cand][stem]), str(score_norm)]
            writer.writerow(row)
                
