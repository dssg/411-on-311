# /usr/bin/

""" Set of functions for mungin open portal 311 data """

import json
import csv
import Polygon
import numpy as np
import scipy.spatial

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
        return 0

