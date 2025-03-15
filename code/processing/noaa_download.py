### file description ###########################################################
##                                                                            ##
## This file downloads National Oceans and Atmospheric Administration's Storm ##
##      data via FTP.                                                         ##
##                                                                            ##
##      Data included:                                                        ##
##          NOAA Storm Events 2004-2024                                       ##
##              https://www.ncdc.noaa.gov/stormevents/ftp.jsp                 ##
##              ./processing_scripts/noaa.r                                   ##
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/data/original_data/noaa                                    ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load libraries

from bs4 import BeautifulSoup           ## web parser
from pprint import pprint               ## pretty print web output
from urllib.parse import urlparse       ## url parser
import requests                         ## http access
import re                               ## regular expressions
import os                               ## access system files

#   ____________________________________________________________________________
#   preliminaries                                                           ####

parent_dir = "T:/"                                ## change this to the path where you want to save the files
directory = "noaa_backup"                                                  ## change this to whatever you want the folder to be called
path = os.path.join(parent_dir, directory)
url = "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"    ## link to the NOAA download page
pattern = "_d(\d{4})_"                                                   ## regular expression to check for the date
start = 1950
end = 2024

#   ____________________________________________________________________________
#   define functions                                                        ####

##  ............................................................................
##  filter links function   

## this function will get all the links on the page then filters them for the years defined above
def get_noaa_links():
    ## use beautiful soup to get all the links 
    r = requests.get(url) 
    soup = BeautifulSoup(r.content, "html.parser")
    all_links = soup.findAll('a')
    noaa_links = [url + link['href'] for link in all_links if link['href'].endswith('.csv.gz')]
    filtered_links = []
    ## filter the links for the years listed above
    for link in noaa_links:
        match = re.search(pattern, link)
        if match:
            year = int(match.group(1))
            if start <= year <= end:
                filtered_links.append(link)
    ## return only links for 2004-2024 
    return filtered_links

# create list of links
noaa_links = get_noaa_links()


##  ............................................................................
##  download links                                                          ####

## this function then takes the noaa_links and saves the associated files
def download_files():
    if os.path.exists(path):
        print("path exists, downloading files")
    else:
        os.mkdir(path)
    ## parse the links and use them to name the files
    for link in noaa_links:
        file_name = link.split("/")[-1]
        file_path = os.path.join(parent_dir, directory, file_name)
        
        ## download the data
        print("Downloading file:%s"%file_name)
        r = requests.get(link, stream = True)
        
        ## open the link and save the content
        with open(file_path, 'wb') as f:
            for chunk in r.iter_content(chunk_size = 1024*1024):
                if chunk:
                    f.write(chunk)
                    
        print("%s downloaded!\n"%file_name)
        
    print("All events downloaded!")
    return

download_files()



