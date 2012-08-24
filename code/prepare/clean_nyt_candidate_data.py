import re
import csv
import urllib

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
                    c['name'],
                    c['party'],
                    c['incumbent']
                    ]
            denorm_list.append(temp)
    else:
        temp = [item['state_id'],
                item['seat_number'],
                item['office_id'],
                item['state'],
                'none',
                'none',
                'none'
                ]
        denorm_list.append(temp)



fieldnames = ['state_id',
              'seat_number',
              'office_id',
              'state',
              'name',
              'party',
              'incumbent'
              ]

with(open('/Users/markhuberty/Documents/Research/Papers/'
          'twitter_election2012/data/candidates.csv', 'wt')) as f:
    writer = csv.writer(f)
    writer.writerow(fieldnames)
    for d in denorm_list:
        writer.writerow(d)
