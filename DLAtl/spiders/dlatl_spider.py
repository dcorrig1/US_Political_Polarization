from scrapy import Spider
from DLAtl.items import DlatlItem
import re

class DLAtlSpider(Spider):
    name = 'DLAtl_Spider'
    allowed_urls = ["https://uselectionatlas.org/"]
    prestart_urls = []
    for i in range(1, 57):
        for j in range(2016, 1956, -4):
            prestart_urls.append("https://uselectionatlas.org/RESULTS/datagraph.php?year=" + str(j) + "&fips=" + str(i) + "&f=1&off=0&elect=0")
    start_urltest = ["https://uselectionatlas.org/RESULTS/datagraph.php?year=2016&fips=1&f=1&off=0&elect=0"]
    start_urls = prestart_urls

    def parse(self, response):
        rows = response.xpath('//div[@class="info"]/table')
        for i in range(len(rows)):
            County = rows[i].xpath('./tr/td/b/text()').extract_first()
            CandidateList = rows[i].xpath('./tr[1]/td[@class="cnd"]/text()').extract()
            CandidateLaterNames = rows[i].xpath('./tr[position()>1]/td[1]/text()').extract()
            CandidateList.extend(CandidateLaterNames)        
            YearStateString = response.xpath('/html/body/div/b/text()').extract_first()
            CandidatePartyCode = rows[i].xpath('./tr/td/div/@class').extract()
            CandidateVoteList = rows[i].xpath('tr/td[@class="dat"]/text()').extract()
            CandidatePctList = rows[i].xpath('tr/td[@class="per"]/text()').extract()
            YearStateString_splitlist = YearStateString.split()
            Year = YearStateString_splitlist[0]
            Statewithdash = re.findall("-.*$", YearStateString)[0]
            State = Statewithdash[2:]
            


            item = DlatlItem()
            item['CandidateList'] = CandidateList
            item['County'] = County
            item['CandidatePartyCodeList'] = CandidatePartyCode
            item['Year'] = Year
            item['State'] = State
            item['CandidateVoteList'] = CandidateVoteList
            item['CandidatePctList'] = CandidatePctList

            yield item



