"""
Crawler of security conference paper
Author:		Hyunji Hong (hyunji_hong@korea.ac.kr)
Create: 	November 10, 2021.
"""

from selenium import webdriver  
import re




"""GLOBALS"""
chrome_driver_path = '/usr/local/bin/chromedriver'      # Path of the chrome-driver
driver = webdriver.Chrome(chrome_driver_path)  

# [regex pattern] regarding my interest
keyword_patterns = re.compile(r'^.*?(open source|open-source|commit|patch| OSS|bug|project|reuse|report).*$',re.I)

def filtering(title_set):
    # collect papers regarding open source security, patches, vulnerability database
    result = []
    for title in title_set:
        if keyword_patterns.findall(title):
            result.append(title)

    return result

def ISSTA():
    # Crawling ISSTA conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://conf.researchr.org/track/issta-'+y+'/issta-'+y+'-technical-papers#event-overview'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('table.table.table-condensed [data-event-modal]')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'ISSTA'+y])
    
    return result

def ESEC_FSE():
    # Crawling FSE conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://'+y+'.esec-fse.org/track/fse-'+y+'-papers?#event-overview'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('table.table.table-condensed [data-event-modal]')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'ESEC-FSE'+y])
    return result

def ICSE():
# Crawling ICSE conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://conf.researchr.org/track/icse-'+y+'/icse-'+y+'-papers?#event-overview'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('table.table.table-condensed [data-event-modal]')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res.replace('\nTechnical',''),'ICSE'+y])
    return result

def MSR():
# Crawling MSR conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://'+y+'.msrconf.org/track/msr-'+y+'-papers?#event-overview'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('table.table.table-condensed [data-event-modal]')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'MSR'+y])
    return result

def main():
# For each function returns the data format
    result = []
    result.extend(ISSTA())
    result.extend(ICSE())
    result.extend(ESEC_FSE())
    result.extend(MSR())
    for res, conf in result:
        print(res+'('+conf+')')



""" EXECUTE """
if __name__ == "__main__":
    main()