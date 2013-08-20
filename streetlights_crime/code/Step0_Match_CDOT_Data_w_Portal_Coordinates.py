import csv
import json
import sys

# Read the outage type that needs to be considered
if sys.argv[1] == 'alley':
    portal_file_name = '311-alley-lights-out.json'
    out_file_name = 'alley_lights_out.csv'
    field_name = 'Alley Light Out'
elif sys.argv[1] == 'street-one':
    portal_file_name = '311-street-lights-one-out.json'
    out_file_name = 'street-lights-one-out.csv'
    field_name = 'Street Light - 1/Out'
elif sys.argv[1] == 'street-all':
    portal_file_name = '311-street-lights-all-out.json'
    out_file_name = 'street-lights-all-out.csv'
    field_name = 'Street Lights - All/Out'


# Open the CDOT lights out csv file
cdot_f = open('../data/lights_out_cdot.csv', 'r')
cdot_r = csv.reader(cdot_f, delimiter=',', quotechar='"')

# Open data portal alley lights out csv file
portal_alley_f = open('../data/' + portal_file_name, 'r')
portal_alley_j = json.load(portal_alley_f)
portal_srns = [e[11] for e in portal_alley_j['data']]

# Open output file
out_f = open('../data/' + out_file_name, 'w')

i = 0
count_alley_lights = 0
count_alley_lights_matches = 0
headers = cdot_r.next()
new_headers = headers[0]
for h in headers[1:]:
    new_headers += ',' + h
# Add x_coord, y_coord
new_headers += ',x_coord,y_coord,zip_code,ward,police_district,community_area'
out_f.write(new_headers + '\n')

for r in cdot_r:
    if r[0] == field_name:
        count_alley_lights += 1
        # Retrieve portal record
        portal_record = [e for e in portal_alley_j['data'] if e[11] == r[1]][0]
        if portal_record == None:
            print 'SRN ' + r[1] + ' not found!'
        elif portal_record[15] == None or portal_record[16] == None:
            print 'X, Y coordinates missing for SRN ' + r[1]
        else:
            count_alley_lights_matches += 1
            new_record = r[0]
            # Put dates in quotes
            r[2] = '"' + r[2] + '"'
            r[3] = '"' + r[3] + '"'
            for v in r[1:]:
                new_record += ',' + v
            # Append x, y coordinates to record and print on file
            #new_record += ',' + portal_record[15] + ',' + portal_record[16]
            new_record += ',{0},{1},{2},{3},{4},{5}'.format(portal_record[15],\
                portal_record[16], portal_record[14], portal_record[17],\
                portal_record[18], portal_record[19])
            out_f.write(new_record + '\n')
    #if i == 100:
    #    break
    if i%1000 == 0:
        print i
    i += 1


out_f.close()


