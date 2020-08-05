import requests
from bs4 import BeautifulSoup
import re as re
import numpy as np
#import cssutils

class WebFonts:

    def __init__(self, url, platform="desktop"):
        self.url = url
        self.platform = platform
        self.fonts = []
        self.USER_AGENT_HEADERS = None
        self.response = None
        self.soup = None
        self.stylesheet = []

        if self.platform == "desktop":
            self.USER_AGENT_HEADERS = {"user-agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"}
        if self.platform == "mobile":
            self.USER_AGENT_HEADERS = {"user-agent": "Mozilla/5.0 (Linux; Android 7.0; SM-G930V Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Mobile Safari/537.36"}

    def get_fonts(self):
        self.get_response()
        self.google_font()
        self.stylesheet_font()
        self.output_fonts()

    def get_response(self):
        # GET HTML response
        self.response = requests.get(self.url, headers=self.USER_AGENT_HEADERS)
        self.soup = BeautifulSoup(self.response.text, 'html.parser')

    def google_font(self):
        #Search response using Beautiful Soup
        result = self.soup.find_all('link', type="text/css", recursive=True)  # not specifying the color attribute
        #Convert result to a list of strings
        result = [str(r) for r in result]
        for r in result:
            google_fonts_tag = re.findall("fonts.googleapis.com/css\\?family=[a-z|A-Z|^\\+]{1,}", r)
            for f in google_fonts_tag:
                google_fonts_list = re.sub("fonts.googleapis.com/css\\?family=", repl="", string=f)
                google_fonts = re.split("\\|", google_fonts_list)
                #google_fonts = re.subn("\\+", " ", google_fonts_list)
                [self.fonts.append(ff) for ff in google_fonts]
                for ff in range(0, len(self.fonts)):
                    self.fonts[ff] = re.sub("\\+", " ", self.fonts[ff])

    def stylesheet_font(self):
         #Search response using Beautiful Soup
         link_tags = self.soup.findAll("link", rel="stylesheet")
         for each_tag in link_tags:
             link = each_tag["href"]
             if re.findall("https?://.*", link):
                 self.stylesheet.append(link)
             else:
                 self.stylesheet.append(self.url + link)
         for s in self.stylesheet:
             response_style = requests.get(s, headers=self.USER_AGENT_HEADERS)
             font_tags = re.findall("font-family:\s{1,}?'.*'", response_style.text)
             for f in font_tags:
                font_extracted = re.sub("'","", re.findall("'.*'", f).pop())
                if "fontawesome" in font_extracted:
                    continue
                else:
                    self.fonts.append(font_extracted)

    def output_fonts(self):
        if self.fonts == []:
            self.fonts.append(np.nan)
        else:
            self.fonts = list(set(self.fonts))