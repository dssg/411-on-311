import json
from itertools import groupby
import csv

normalize = True
f = open('../data/comm-area-call-volume.csv', 'w')

db_names = [  'abandoned-vehicles',
              'vacant-abandoned-buildings',
              'graffiti-removal',
              'potholes',
              'sanitation-code-complaints',
              'garbage-carts',
              'rodent-baiting',
              'street-lights-one-out',
              'street-lights-all-out',
              'alley-lights-out',  
              'tree-trims',
              'tree-debris'
              ]

pop_f = open('../data/chicago-community-areas.csv', 'r')
pop_f.readline()
areas_info = {}
l = pop_f.readline().split(',')
areas_info['names'] = l[1:]
l = pop_f.readline().split(',')
areas_info['population'] = [int(e) for e in l[1:]]
pop = areas_info['population']
pop_f.close()
print pop

for db_name in db_names:
    data_file = '311-' + db_name + '.json'
    jf = open('../data/' + data_file, 'r')

    req_data = json.load(jf)
    jf.close()

    columns = [e['fieldName'] for e in req_data['meta']['view']['columns']]
    ca_col = columns.index('community_area')
    date_col = 10 if db_name == 'vacant-abandoned-buildings' else 8
    comm_areas = sorted([int(e[ca_col]) for e in req_data['data'][:-1] \
        if int(e[date_col][0:4]) > 2010 and e[ca_col] not in ['0', None]])

    counts = [(k, len(list(g))) for (k, g) in groupby(comm_areas)]
  
    if normalize:
        counts = [(counts[i][0], float(counts[i][1]*10000)/float(pop[i])) \
            for i in xrange(len(counts))]
  
    if db_name == 'abandoned-vehicles':
        # Print headers
        f.write('type, ')
        f.write(', '.join([str(e[0]) for e in counts]))
        f.write('\n')
    f.write(db_name + ', ')
    f.write(', '.join([str(e[1]) for e in counts]))
    f.write('\n')

f.close()
              
