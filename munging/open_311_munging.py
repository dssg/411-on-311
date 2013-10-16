# /usr/bin/

""" Set of functions for mungin open portal 311 data """

import json
import csv
import Polygon
import numpy as np
import scipy.spatial
import shapefile

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

def add_tract_info(request_type='graffiti-removal', data_folder='../data/',\
    write_geojson=False):
    """ Find the tract request of type request_type are generated from, and
    write a geojson file containing the information. """

    # Open the census tract shapefile
    sf = shapefile.Reader(data_folder + "census_tracts/Census_Tracts")
    tract_shapes = [s.points for s in sf.iterShapes()]
    tract_ids = [r[2] for r in sf.iterRecords()]
    
    # Create the FindInside object
    finder = FindInside(tract_shapes)

    # Open the JSON file for the specified type of request
    json_f = open(data_folder + '311-' + request_type + '.json')
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

    json_out_f = open(data_folder + '311-' + request_type + '-tracts.json', 'w')
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

