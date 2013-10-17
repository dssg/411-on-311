""" This script downloads the 311 requests log from the Chicago data portal """

import json
import requests
import sys

__author__ = "Alessandro Panella (apanel2@uic.edu)"

db_codes = {
    'tree-debris': 'mab8-y9h3',
    'tree-trims': 'uxic-zsuj',
    'graffiti-removal': 'hec5-y4x5',
    'rodent-baiting': '97t6-zrhs',
    'garbage-carts': '9ksk-na4q',
    'abandoned-vehicles': '3c9v-pnva',
    'vacant-abandoned-buildings': '7nii-7srd',
    'sanitation-code-complaints': 'me59-5fac',
    'potholes': '7as2-ds3y',
    'alley-lights-out': 't28b-ys7j',
    'street-lights-one-out': '3aav-uy2v',
    'street-lights-all-out': 'zuxi-7xem'
}
db_codes = {
    'vacant-abandoned-buildings': '7nii-7srd',
}

for (i, k) in enumerate(db_codes.keys()):
    if sys.argv[1] == None:
        data_folder = '../data'
    else:
        data_folder = sys.argv[1]
    print 'Fetching 311 DB: ' + k + ' (' + db_codes[k] + ')'
    # Form query
    query = 'http://data.cityofchicago.org/views/' + db_codes[k] + '/rows.json'
    # Submit request
    r = requests.get(query).json()
    # Save json db on file
    out_file_name = data_folder + '/' + k + '.json'
    out_file = open(out_file_name, 'w')
    json.dump(r, out_file)
    out_file.close()

