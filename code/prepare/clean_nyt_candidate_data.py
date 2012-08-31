import urllib
import re
import csv
import hashlib
import json

## Define some functions

def fix_party_names(input_party, party_name_dict):
    for k in party_name_dict:
        regex = '|'.join(party_name_dict[k])
        input_party = re.sub(regex, k, input_party)
    return(input_party)

party_name_dict = {'D':['Dem[\.]*', 'DEM[\.]*', 'Democrat'],
                   'R':['Rep[\.]*', 'REP[\.]*', 'Republican',
                        'GOP'],
                   'I':['NPD']
                   }

## End function def

## Get the NYT candidate data

nyt_handle = urllib.urlopen('http://elections.nytimes.com/2012/ratings/house')
nyt_raw = [line for line in nyt_handle]
nyt_handle.close()

## Get only the line with the data in it
## and clean it up
nyt_data = [d for d in nyt_raw if re.search('^\s+?var\sratings', d)]
nyt_data = re.sub("^\s+?var\sratings\s=\s", "", nyt_data[0])
nyt_data = re.sub(";$", "", nyt_data)

nyt_parsed = json.loads(nyt_data)

denorm_list = []
for idx, item in enumerate(nyt_parsed):
    print idx
    if item['seat_number'] < 10:
        state_dist = item['state_id'] + '0' + str(item['seat_number'])
    else:
        state_dist = item['state_id'] + str(item['seat_number'])
    if item['candidates']:
        for c in item['candidates']:
            temp = {'state_dist': state_dist,
                    'state_id':item['state_id'],
                    'district': item['seat_number'],
                    'office_id': item['office_id'],
                    'state': item['state'],
                    'primary_date': item['primary_date'],
                    'name': c['name'],
                    'party': fix_party_names(c['party'], party_name_dict),
                    'incumbent':c['incumbent']
                    }
            split_names = temp['name'].split(" ")
            first_name = split_names[0]
            if len(split_names) >= 2:
                last_name = ' '.join(split_names[1:])
            else:
                last_name = 'none'
            temp['last_name'] = last_name
            temp['first_name'] = first_name
            temp['unique_cand_id'] = '_'.join([temp['state_dist'],
                                               temp['party'],
                                               temp['last_name']
                                               ]
                                              )
            denorm_list.append(temp)
    else:
        temp = {'state_dist': state_dist,
                'state_id':item['state_id'],
                'district': item['seat_number'],
                'office_id': item['office_id'],
                'state': item['state'],
                'primary_date': item['primary_date'],
                'name': 'none',
                'party': 'none',
                'incumbent':'none',
                'first_name':'none',
                'last_name':'none',
                'unique_cand_id':'none'
                }
        denorm_list.append(temp)

fieldnames = ['state_dist',
              'state_id',
              'district',
              'office_id',
              'state',
              'primary_date',
              'name',
              'party',
              'incumbent',
              'first_name',
              'last_name',
              'unique_cand_id'
              ]


with(open('../../data/candidates.csv', 'wt')) as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writer.writerow(writer.fieldnames)
    for d in denorm_list:
        writer.writerow(d)
