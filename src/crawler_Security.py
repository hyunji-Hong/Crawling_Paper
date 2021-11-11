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

def USENIX():
# Crawling USENIX conference
    result = []
    year = ["19", "20", "21", "22"]
    season = ["summer", "fall", "spring", "winter"]
    
    for y in year:
        for s in season:
            conference_url  = 'https://www.usenix.org/conference/usenixsecurity'+y+'/'+s+'-accepted-papers'
            driver.get(conference_url)
            elems = driver.find_elements_by_css_selector('h2.node-title [href]')
            # links = [elem.get_attribute('href') for elem in elems]
            titles = [elem.text for elem in elems]
            for res in filtering(titles):
                result.append([res,'USENIX'+y+'-'+s])
    return result

def CCS():
# Crawling CCS conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://www.sigsac.org/ccs/CCS'+y+'/accepted-papers.html'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('div.row.papers-item .col-sm-3.col-md-6 b')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'CCS'+y])
    return result

def SnP():
# Crawling S&P conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://www.ieee-security.org/TC/SP'+y+'/program-papers.html'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('div.list-group-item b')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'S&P'+y])
    return result

def NDSS():
# Crawling NDSS conference
    result = []
    year = ['2019','2020','2021']
    for y in year:
        conference_url = 'https://www.ndss-symposium.org/ndss'+y+'/accepted-papers/'
        driver.get(conference_url)
        elems = driver.find_elements_by_css_selector('p strong')
        titles = [elem.text for elem in elems]
        for res in filtering(titles):
            result.append([res,'NDSS'+y])
    return result

def main():
# For each function returns the data format
    result = []
    result.extend(USENIX())
    result.extend(CCS())
    result.extend(SnP())
    result.extend(NDSS())
    for res, conf in result:
        print(res+'('+conf+')')



""" EXECUTE """
if __name__ == "__main__":
    main()