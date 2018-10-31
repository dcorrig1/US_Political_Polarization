# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class DlatlItem(scrapy.Item):
    # define the fields for your item here like:
    County = scrapy.Field()
    CandidateList = scrapy.Field()    
    Year = scrapy.Field()
    CandidatePartyCodeList = scrapy.Field()
    CandidateVoteList = scrapy.Field()
    CandidatePctList = scrapy.Field()
    State = scrapy.Field()
    
