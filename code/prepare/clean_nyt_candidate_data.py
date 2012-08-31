import re
import csv
import urllib
import json

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
                    'seat_number': item['seat_number'],
                    'office_id': item['office_id'],
                    'state': item['state'],
                    'primary_date': item['primary_date'],
                    'name': c['name'],
                    'party': c['party'],
                    'incumbent':c['incumbent']
                    }
            denorm_list.append(temp)
    else:
        temp = {'state_dist': state_dist,
                'state_id':item['state_id'],
                'seat_number': item['seat_number'],
                'office_id': item['office_id'],
                'state': item['state'],
                'primary_date': item['primary_date'],
                'name': 'none',
                'party': 'none',
                'incumbent':'none'
                }
        denorm_list.append(temp)




## Split and format the names
split_names = [item['name'].split(" ") for item in denorm_list]

for idx, s in enumerate(split_names):
    first_name = s[0]
    if len(s) == 2:
        last_name = s[1]
    elif len(s) > 2:
        if re.search(s[1], '[A-Z\.]{2}'):
            last_name = s[2]
        else:
            last_name = s[1] + ' ' + s[2]
    else:
        last_name = 'none'
    denorm_list[idx]['first_name'] = first_name
    denorm_list[idx]['last_name'] = last_name

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
              'last_name'
              ]


with(open('../../data/candidates.csv', 'wt')) as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writer.writerow(writer.fieldnames)
    for d in denorm_list:
        writer.writerow(d)
