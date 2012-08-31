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
    if item['candidates']:
        for c in item['candidates']:
            temp = [item['state_id'],
                    item['seat_number'],
                    item['office_id'],
                    item['state'],
                    item['primary_date'],
                    c['name'],
                    c['party'],
                    c['incumbent'],
                    ]
            denorm_list.append(temp)
    else:
        temp = [item['state_id'],
                item['seat_number'],
                item['office_id'],
                item['state'],
                item['primary_date'],
                'none',
                'none',
                'none'
                ]
        denorm_list.append(temp)




## Split and format the names
split_names = [n.split(" ") for item['name'] in denorm_list]

for idx, s in enumerate(split_names):
    first_name = s[0]
    if len(s) == 2:
        last_name = s[1]
    else:
        if re.search(s[1], '[A-Z\.]{2}'):
            last_name = s[2]
        else:
            last_name = s[1] + ' ' + s[2]
    denorm_list[i]['first_name'] = first_name
    denorm_list[i]['last_name'] = last_name

fieldnames = ['state_id',
              'seat_number',
              'office_id',
              'state',
              'primary_date',
              'name',
              'party',
              'incumbent',
              'first_name',
              'last_name'
              ]


with(open('/Users/markhuberty/Documents/Research/Papers/'
          'twitter_election2012/data/candidates.csv', 'wt')) as f:
    writer = csv.writer(f)
    writer.writerow(fieldnames)
    for d in denorm_list:
        writer.writerow(d)
