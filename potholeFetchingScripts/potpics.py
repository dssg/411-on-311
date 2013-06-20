import urllib2
import urllib
import json

#parameters
for ii in range(100):
    response=urllib2.urlopen(r"http://311api.cityofchicago.org/open311/v2/requests.json?service_code=4fd3b656e750846c53000004&page_size=500&page="+str(ii))
    potholes=json.loads(response.read())

    for pothole in potholes:
        try:     
            link = pothole['media_url']
            print link
            filename = link.split('/')[-1]
            urllib.urlretrieve(link, filename)                
        except KeyError: 
            pass
    
    


