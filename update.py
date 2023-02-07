#!/usr/bin/python3

import xtb
import time
import csv
import datetime
import sys

logger = xtb.logger;

class Session():
    def __init__(self):
        self.client = xtb.APIClient();

        uid=xtb.API_UID
        logger.info("UserID: " + str(uid)) 
        password = xtb.passwd()

        loginResponse = self.client.execute(xtb.loginCommand(userId=uid, password=password, appName="myapp"))
        logger.info(str(loginResponse)) 

        if(loginResponse['status'] == False):
            raise(Exception('Login failed. Error code: {0}'.format(loginResponse['errorCode'])))

        self.ssid = loginResponse['streamSessionId']

    def call(self, cmd, args, transform):
        resp = self.client.commandExecute(cmd, arguments=args)
        if(resp['status'] == False):
            print(resp)
            raise(Exception("error while calling {0}".format(cmd)))

        return transform(resp['returnData']);

    def close(self):
        self.client.disconnect()

def subdict(keys,d):
    ans = dict()
    for k in (keys):
        ans[k] = d[k]
    return ans

def symtab(data):
    return list(map(lambda x: subdict(['symbol', 'currency', 'description'],x), data))

def store(file, d):
    with open(file, 'w') as fh:
        dw = csv.DictWriter(fh, d[0].keys())
        dw.writeheader()
        dw.writerows(d)

s = Session();
print("Update symbols table")
syms = s.call('getAllSymbols', {}, symtab)
store("xtb.csv", syms)
#syms = []
#with open("xtb.csv", "r") as fh:
#    dr = csv.DictReader(fh)
#    syms=list(dr)

ctime = s.call('getServerTime', {}, lambda x: x['time'])

for i in range(0, len(syms)):
    fname = "xtb/{0}.csv".format(syms[i]['symbol'])
    try:
        data = [];
        last = dict();
        with open(fname, "r") as fh:
            for x in csv.DictReader(fh):
                data.append(x);
   
            last = data.pop();

        days = ctime
        loaded = False
        if("date" in last):
            loaded = True
            today = datetime.datetime.now().timestamp();
            today = today - (today % 86400)
            days = (today - xtb.strTime(last['date'])) * 1000

        if days > 106400000:
            with open(fname, "w") as fh:
                hist = s.call('getChartLastRequest', {"info": {"period": 1440,
                                                               "start": ctime - days,
                                                               "symbol": syms[i]['symbol']}},
                                                     xtb.histTransform(xtb.dayDate))
                if i % 4 == 0: time.sleep(1)

                dw = csv.DictWriter(fh, hist[0].keys())
                dw.writeheader()
                dw.writerows(data)
                dw.writerows(hist)

                print("{0}/{1}: {2} +{3}/{4} {5}\r".format(i, len(syms), syms[i]['symbol'], days/86400000, len(hist), loaded), end="")
    except Exception as e:
        sys.stderr.write("{0}\n".format(str(e)))

s.close()
