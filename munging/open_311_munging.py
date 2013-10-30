# /usr/bin/

""" Set of functions for mungin open portal 311 data """

import json
import csv
import Polygon
import numpy as np
import scipy.spatial
import shapefile
import pickle as pkl

__author__ = "Alessandro Panella (apanel2@uic.edu)"

class FindInside:
    """ Find points inside shapes.
    This class is based on a script by Max Schronn. """
    
    def __init__(self, shapes):
        """ Initialize data structures, given the set of shapes """
        
        # Convert the list of shapes to a dictionary of Polygons
        self.polygons = {}
        for i, s in enumerate(shapes):
            self.polygons[i] = Polygon.cPolygon.Polygon(s)
       
        # To make things faster, create a cKDTree of shape centers
        self.centers = np.asarray([p.center() for p in self.polygons.itervalues()])
        # Reverse map for looking up polygons from centers
        self.reverse = {i: k for (i,k) in enumerate(self.polygons.iterkeys())}
        # Create the tree
        self.fastlookup = scipy.spatial.cKDTree(self.centers)

    def find(self, point, k=10):
        """ Return the index of the shape containing the point,
        otherwise return -1 """
        
        # Lookup in the tree the k nearest neighbors (centers) of the point
        k = 10
        idxs = self.fastlookup.query(point, k)[1]
        keys = [self.reverse[i] for i in idxs]
        for k in keys:
            if self.polygons[k].isInside(point[0], point[1]):
                return k
        return -1

def add_tract_info(request_type='graffiti-removal', data_folder='../data',\
    write_geojson=False):
    """ Find the tract request of type request_type are generated from, and
    write a geojson file containing the information. """

    # Open the census tract shapefile
    sf = shapefile.Reader(data_folder +\
        "/census_tracts_2010/CensusTractsTIGER2010")
    tract_shapes = [s.points for s in sf.iterShapes()]
    tract_ids = [r[3] for r in sf.iterRecords()]
    
    # Create the FindInside object
    finder = FindInside(tract_shapes)

    # Open the JSON file for the specified type of request
    json_f = open(data_folder + '/' + request_type + '.json')
    requests = json.load(json_f)
    json_f.close()

    # Add the field "tract" to the json metadata (if not present already)
    columns = [e['fieldName'] for e in requests['meta']['view']['columns']]
    if "tract" in columns:
        print "Census tract information already present"
        return

    tract_column = {
        'dataTypeName': 'number',
        'fieldName': 'tract',
        'name': 'TRACT',
        'renderTypeName': 'number',
        'position': requests['meta']['view']['columns'][-1]['position'] + 1
    }
    requests['meta']['view']['columns'].append(tract_column)

    # Check where the x and y coordinates are stored in the records
    y_col = columns.index('y_coordinate')
    x_col = columns.index('x_coordinate')
    # Scan the records and add the tract information
    for (i, record) in enumerate(requests['data']):
        record.append(-1)
        if None not in [record[x_col], record[y_col]]:
            tract_index = finder.find((float(record[x_col]), float(record[y_col])))
            if tract_index != -1:
                tract_id = tract_ids[tract_index]
                record[-1] = tract_id
        if i%10000 == 0:
            print i

    json_out_f = open(data_folder + '/' + request_type + '-tracts.json', 'w')
    json.dump(requests, json_out_f)
    json_out_f.close()

    # Write geojson, if requested
    if(write_geojson):
        # Check where the desired information is stored in the json db
        srn_col = columns.index('service_request_number')
        ward_col = columns.index('ward')
        date_col = columns.index('creation_date')

        # Open the output geojson file
        out = open('../data/' + request_type + '.geojson', 'w')
        out.write('{ "type": "FeatureCollection",\n')
        out.write('  "features": [\n')
        
        # Scan the records and find the tract the request was generated from, and
        # simultanelously write a geojson file
        for (i, record) in enumerate(requests['data']):
            if None not in [record[x_col], record[y_col], record[srn_col],\
                record[date_col], record[ward_col]]:
                # Retrieve the census tract ID
                tract_index = finder.find((float(record[x_col]), float(record[y_col])))
                if tract_index != -1:
                    tract_id = tract_ids[tract_index]
                    x = record[x_col]
                    y = record[y_col]
                    s = '    {{ "type": "Feature",\n\
          "geometry": {{"type": "Point", "coordinates": [{0}, {1}]}},\n\
          "properties": {{"SRN": "{2}", "Date": "{3}", "Ward": {4},\
     "Tract": "{5}" }}\n'\
                    .format(x, y, str(record[srn_col]), str(record[date_col]),\
                        str(record[ward_col]), tract_id)

                    out.write(s)

                    if i < len(requests['data'][:-1])-1:
                    #if i < 150-1:
                      out.write('      },\n')
                    else:
                      out.write('      }\n')
                      break
        out.write('    ]\n  }')
        out.close()

def aggr_tract_data(data_folder = '../data'):
    
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
    
    # Open census file containing population of each tract (and other info)
    census_f = open(data_folder +'/acs_2007_11_tract_variables.csv', 'r')
    census_r = csv.DictReader(census_f)
    census_d = []
    for r in census_r:
        census_d.append(r);
    census_f.close()

    header = ["id", "population"] + db_names
    out_f = open(data_folder + '/requests_by_tract.csv', 'w');
    out_w = csv.writer(out_f);
    out_w.writerow(header);
    
    aggr_vol = {}

    # For each request type, aggregate the number of requests at the tract level
    for i, db in enumerate(db_names):
        # Open the requests json file
        json_f = open(data_folder + '/' + db + '-tracts.json', 'r')
        req = json.load(json_f)
        json_f.close()
        count = 0
        for r in req['data']:
            if r[-1] in aggr_vol.keys():
                aggr_vol[r[-1]][i] += 1
            else:
                aggr_vol[r[-1]] = [0] * len(db_names)
                aggr_vol[r[-1]][i] = 1
            count += 1
            if(count % 10000 == 0):
                print count
        # Now write on the csv file
    for tract in aggr_vol.iterkeys():
        # Find the tract pop in tract info file
        pop = 0
        for record in census_d:
            if record['GEO.id2'] == tract:
                pop = int(record['Total_Pop'])
                break
        row = [tract, pop] + aggr_vol[tract]
        out_w.writerow(row)

    out_f.close();

def shp_to_geojson(shapename):
    """ This code is based on
        http://geospatialpython.com/2013/07/shapefile-to-geojson.html """
    # read the shapefile
    reader = shapefile.Reader(shapename)
    fields = reader.fields[1:]
    field_names = [field[0] for field in fields]
    buffer = []
    for sr in reader.shapeRecords():
        atr = dict(zip(field_names, sr.record))
        geom = sr.shape.__geo_interface__
        buffer.append(dict(type="Feature", \
            geometry=geom, properties=atr)) 
   
    # write the GeoJSON file
    geojson = open(shapename + '.geojson', "w")
    geojson.write(json.dumps({"type": "FeatureCollection",\
    "features": buffer}, indent=2) + "\n")
    geojson.close()

def add_311_to_tracts(tracts_file, data_folder='../data'):
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
    
    # Open the shapefile
    geojson_in = open(tracts_file, 'r')
    tracts_geojson = json.load(geojson_in)
    geojson_in.close()

    # Open the request aggragates by tract
    req_file = open(data_folder + '/requests_by_tract.csv')
    req_reader = csv.DictReader(req_file)
    reqs = {} # dictionary of dictionaries
    for r in req_reader:
        reqs[r.pop('id')] = r
    req_file.close()
    clustering_data = np.zeros((len(tracts_geojson['features']), 12))


    # For each record in the geojson file, add the 
    for i, r in enumerate(tracts_geojson['features']):
        tract_id = r['properties']['GEOID10']
        if tract_id in reqs.keys() and float(reqs[tract_id]['population']) > 0:
            for j, req_type in enumerate(db_names):
                r['properties'][req_type] = float(reqs[tract_id][req_type]) /\
                    float(reqs[tract_id]['population'])
                clustering_data[i, j] = float(reqs[tract_id][req_type]) /\
                    float(reqs[tract_id]['population'])
        else:
            for req_type in db_names:
                r['properties'][req_type] = 0.0;

    geojson_out = open(data_folder + '/requests_by_tract.geojson', 'w')
    geojson_out.write(json.dumps(tracts_geojson));
    geojson_out.close()

    pkl_out = open(data_folder + '/clustering_data.pkl', 'w')
    pkl.dump(clustering_data, pkl_out)
    pkl_out.close()

def do_all_munging(data_folder='../data'):
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
   
    for service_name in db_names:
        print('\n\n' + service_name)
        add_tract_info(request_type=service_name, data_folder=data_folder)

    aggr_tract_data(data_folder=data_folder)

    add_311_to_tracts('../data/census_tracts_2010/CensusTractsTIGER2010.geojson',\
        data_folder=data_folder)

