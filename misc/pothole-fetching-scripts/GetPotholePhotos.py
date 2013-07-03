#!/usr/bin/env

import three
import urllib
import io

i=1
chicago= three.city('chicago')
for pageNum in range (1,500):
    potholeRecords = chicago.requests(service_code='4fd3b656e750846c53000004',
    extensions='true',between=['1-12-2009','6-13-2013'], page_size=50, page=pageNum)
    potholeLinks = [req['media_url'] for req  in potholeRecords if 'media_url' in req]
    for pothole in potholeLinks:
        print pothole
    	localPathName="/Users/sarahevans/Documents/DSSG/dssg-Indices-project/potholePhotos/pothole"+str(i)+ ".jpg"
    	i+=1
        open(localPathName, 'w').close()
    	urllib.urlretrieve(pothole,localPathName)

#print len(potholePictures)
#for picture in potholePictures:
  
##print chicago.services()
##http://data.cityofchicago.org/resource/7as2-ds3y.json


