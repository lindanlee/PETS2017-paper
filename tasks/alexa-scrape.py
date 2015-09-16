#!/usr/bin/env python

import urllib2

from bs4 import BeautifulSoup

for page in range(0, 4):
    url = "http://www.alexa.com/topsites/countries;%d/US" % page
    f = urllib2.urlopen(url)
    try:
        soup = BeautifulSoup(f)
        for site_listing in soup.find_all("li", class_="site-listing"):
            count = site_listing.find("div", class_="count").get_text().strip()
            link = site_listing.find("a").get("href")
            site = link.split("/")[-1]
            print ",".join([count, site])
    finally:
        f.close()
