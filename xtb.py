#!/usr/bin/env python3

# This script provides simple access to XTB trading environment.
# based on code from XTB:
# http://developers.xstore.pro/api/wrappers.html

import json
import socket
import logging
import time
import ssl
import sys
import time
import datetime
import getpass
import re
import os

# First of all, you need to provide your XTB UID:
API_UID = os.environ["XTB_UID"];


from threading import Thread

# set to true on debug environment only
DEBUG = False

#default connection properites
DEFAULT_XAPI_ADDRESS        = 'xapi.xtb.com'
DEFAULT_XAPI_PORT           = 5124
DEFUALT_XAPI_STREAMING_PORT = 5125

# wrapper name and version
WRAPPER_NAME    = 'python'
WRAPPER_VERSION = '2.5.0'

# API inter-command timeout (in ms)
API_SEND_TIMEOUT = 100

# max connection tries
API_MAX_CONN_TRIES = 3

# logger properties
logger = logging.getLogger("jsonSocket")
FORMAT = '[%(asctime)-15s][%(funcName)s:%(lineno)d] %(message)s'
logging.basicConfig(format=FORMAT)

def short(str, maxlen=30):
    if(len(str) < maxlen):
        return str
    else:
        return str[0:30]

if DEBUG:
    logger.setLevel(logging.DEBUG)
else:
    logger.setLevel(logging.CRITICAL)


class TransactionSide(object):
    BUY = 0
    SELL = 1
    BUY_LIMIT = 2
    SELL_LIMIT = 3
    BUY_STOP = 4
    SELL_STOP = 5
    
class TransactionType(object):
    ORDER_OPEN = 0
    ORDER_CLOSE = 2
    ORDER_MODIFY = 3
    ORDER_DELETE = 4

class JsonSocket(object):
    def __init__(self, address, port, encrypt = False):
        self._ssl = encrypt 
        if self._ssl != True:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        else:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket = ssl.wrap_socket(sock)
        self.conn = self.socket
        self._timeout = None
        self._address = address
        self._port = port
        self._decoder = json.JSONDecoder()
        self._receivedData = ''

    def connect(self):
        for i in range(API_MAX_CONN_TRIES):
            try:
                self.socket.connect( (self.address, self.port) )
            except socket.error as msg:
                logger.error("SockThread Error: %s" % msg)
                time.sleep(0.25);
                continue
            logger.info("Socket connected")
            return True
        return False

    def _sendObj(self, obj):
        msg = json.dumps(obj)
        if(obj["command"] == "login"):
            self._waitingSend(msg, logMsg="Sending login request")
        else:
            self._waitingSend(msg)

    def _waitingSend(self, msg, logMsg=None):
        if self.socket:
            sent = 0
            msg = msg.encode('utf-8')
            while sent < len(msg):
                sent += self.conn.send(msg[sent:])
                if(logMsg):
                    logger.info(logMsg)
                else:
                    logger.info('Sent: ' + str(msg))
                time.sleep(API_SEND_TIMEOUT/1000)

    def _read(self, bytesSize=4096):
        if not self.socket:
            raise RuntimeError("socket connection broken")
        while True:
            char = self.conn.recv(bytesSize).decode()
            self._receivedData += char
            try:
                (resp, size) = self._decoder.raw_decode(self._receivedData)
                if size == len(self._receivedData):
                    self._receivedData = ''
                    break
                elif size < len(self._receivedData):
                    self._receivedData = self._receivedData[size:].strip()
                    break
            except ValueError as e:
                continue
        logger.info('Received: ' + short(str(resp), maxlen=100))
        return resp

    def _readObj(self):
        msg = self._read()
        return msg

    def close(self):
        logger.debug("Closing socket")
        self._closeSocket()
        if self.socket is not self.conn:
            logger.debug("Closing connection socket")
            self._closeConnection()

    def _closeSocket(self):
        self.socket.close()

    def _closeConnection(self):
        self.conn.close()

    def _get_timeout(self):
        return self._timeout

    def _set_timeout(self, timeout):
        self._timeout = timeout
        self.socket.settimeout(timeout)

    def _get_address(self):
        return self._address

    def _set_address(self, address):
        pass

    def _get_port(self):
        return self._port

    def _set_port(self, port):
        pass

    def _get_encrypt(self):
        return self._ssl

    def _set_encrypt(self, encrypt):
        pass

    timeout = property(_get_timeout, _set_timeout, doc='Get/set the socket timeout')
    address = property(_get_address, _set_address, doc='read only property socket address')
    port = property(_get_port, _set_port, doc='read only property socket port')
    encrypt = property(_get_encrypt, _set_encrypt, doc='read only property socket port')
    
    
class APIClient(JsonSocket):
    def __init__(self, address=DEFAULT_XAPI_ADDRESS, port=DEFAULT_XAPI_PORT, encrypt=True):
        super(APIClient, self).__init__(address, port, encrypt)
        if(not self.connect()):
            raise Exception("Cannot connect to " + address + ":" + str(port) + " after " + str(API_MAX_CONN_TRIES) + " retries")

    def execute(self, dictionary):
        self._sendObj(dictionary)
        return self._readObj()    

    def disconnect(self):
        self.close()
        
    def commandExecute(self,commandName, arguments=None):
        return self.execute(baseCommand(commandName, arguments))

class APIStreamClient(JsonSocket):
    def __init__(self, address=DEFAULT_XAPI_ADDRESS, port=DEFUALT_XAPI_STREAMING_PORT, encrypt=True, ssId=None, 
                 tickFun=None, tradeFun=None, balanceFun=None, tradeStatusFun=None, profitFun=None, newsFun=None):
        super(APIStreamClient, self).__init__(address, port, encrypt)
        self._ssId = ssId

        self._tickFun = tickFun
        self._tradeFun = tradeFun
        self._balanceFun = balanceFun
        self._tradeStatusFun = tradeStatusFun
        self._profitFun = profitFun
        self._newsFun = newsFun
        
        if(not self.connect()):
            raise Exception("Cannot connect to streaming on " + address + ":" + str(port) + " after " + str(API_MAX_CONN_TRIES) + " retries")

        self._running = True
        self._t = Thread(target=self._readStream, args=())
        self._t.setDaemon(True)
        self._t.start()

    def _readStream(self):
        while (self._running):
                msg = self._readObj()
                logger.info("Stream received: " + str(msg))
                if (msg["command"]=='tickPrices'):
                    self._tickFun(msg)
                elif (msg["command"]=='trade'):
                    self._tradeFun(msg)
                elif (msg["command"]=="balance"):
                    self._balanceFun(msg)
                elif (msg["command"]=="tradeStatus"):
                    self._tradeStatusFun(msg)
                elif (msg["command"]=="profit"):
                    self._profitFun(msg)
                elif (msg["command"]=="news"):
                    self._newsFun(msg)
    
    def disconnect(self):
        self._running = False
        self._t.join()
        self.close()

    def execute(self, dictionary):
        self._sendObj(dictionary)

    def subscribePrice(self, symbol):
        self.execute(dict(command='getTickPrices', symbol=symbol, streamSessionId=self._ssId))
        
    def subscribePrices(self, symbols):
        for symbolX in symbols:
            self.subscribePrice(symbolX)
    
    def subscribeTrades(self):
        self.execute(dict(command='getTrades', streamSessionId=self._ssId))
        
    def subscribeBalance(self):
        self.execute(dict(command='getBalance', streamSessionId=self._ssId))

    def subscribeTradeStatus(self):
        self.execute(dict(command='getTradeStatus', streamSessionId=self._ssId))

    def subscribeProfits(self):
        self.execute(dict(command='getProfits', streamSessionId=self._ssId))

    def subscribeNews(self):
        self.execute(dict(command='getNews', streamSessionId=self._ssId))


    def unsubscribePrice(self, symbol):
        self.execute(dict(command='stopTickPrices', symbol=symbol, streamSessionId=self._ssId))
        
    def unsubscribePrices(self, symbols):
        for symbolX in symbols:
            self.unsubscribePrice(symbolX)
    
    def unsubscribeTrades(self):
        self.execute(dict(command='stopTrades', streamSessionId=self._ssId))
        
    def unsubscribeBalance(self):
        self.execute(dict(command='stopBalance', streamSessionId=self._ssId))

    def unsubscribeTradeStatus(self):
        self.execute(dict(command='stopTradeStatus', streamSessionId=self._ssId))

    def unsubscribeProfits(self):
        self.execute(dict(command='stopProfits', streamSessionId=self._ssId))

    def unsubscribeNews(self):
        self.execute(dict(command='stopNews', streamSessionId=self._ssId))


# Command templates
def baseCommand(commandName, arguments=None):
    if arguments==None:
        arguments = dict()
    return dict([('command', commandName), ('arguments', arguments)])

def loginCommand(userId, password, appName=''):
    return baseCommand('login', dict(userId=userId, password=password, appName=appName))

def passwd():
    try:
        return os.environ["XTB_PASS"];
    except:
        return getpass.getpass();

def query(name, args, respTransform):
    userId = API_UID;
    password = passwd();

    client = APIClient()
    
    loginResponse = client.execute(loginCommand(userId=userId, password=password, appName="myapp"))
    logger.info(str(loginResponse)) 

    if(loginResponse['status'] == False):
        print('Login failed. Error code: {0}'.format(loginResponse['errorCode']))
        return

    ssid = loginResponse['streamSessionId']
    
    resp = client.commandExecute(name, arguments=args)
    if(resp['status'] == True):
        data = resp['returnData']
        ans = respTransform(data)
        resp = client.commandExecute('logout')
        client.disconnect()
        return ans
    else:
        print(resp);

    return resp;

def timeQuery(name, argsF, transform):
    userId = API_UID;
    password = passwd();

    client = APIClient()
    
    loginResponse = client.execute(loginCommand(userId=userId, password=password, appName="myapp"))
    logger.info(str(loginResponse)) 

    if(loginResponse['status'] == False):
        print('Login failed. Error code: {0}'.format(loginResponse['errorCode']))
        return

    ssid = loginResponse['streamSessionId']
    
    resp = client.commandExecute('getServerTime', arguments={})
    if(resp['status'] != True):
        return resp;

    ctime = resp['returnData']['time']

    resp = client.commandExecute(name, arguments=argsF(ctime))
    if(resp['status'] == True):
        ans = transform(resp)
        resp = client.commandExecute('logout')
        client.disconnect()
        return ans
    else:
        print(resp);

    return resp;

def tradesAns(data):
    rows = ["symbol", "change(%)", "open_price", "close_price", "profit", "nominalValue"]
    print(" ".join(map(lambda key: "{x:12s}".format(x=key), rows)))
    for sym in data:
        sym['change(%)'] = round((float(sym['close_price']) - float(sym['open_price']))
                                 /float(sym['open_price']) *100, 4)
        print(" ".join(map(lambda key: "{x:12s}".format(x=str(sym[key])), rows)))

    return data

def priceTransform(data):
    data = data['quotations']
    print(data)
    return data

def timeAns(x):
    print(x)
    return x

def strTime(str):
    return int(time.mktime(datetime.datetime.strptime(str, "%Y-%m-%d").timetuple()))

def normDate(dateStr) :
    date = re.sub(",[^,]+$", "", dateStr)
    date = datetime.datetime.strptime(date, "%b %d, %Y")
    return date.strftime("%Y-%m-%d")

def histTransform(data):
    digits = data['returnData']['digits']
    mult = 10**digits
    data = data['returnData']['rateInfos']
    for it in data:
        open = it['open'] / mult
        ans = { "date" : normDate(it['ctmString']),
                "price" : round(open + it['close'] / mult,2),
                "open" : round(open,2),
                "high" : round(open + it['high'] / mult,2),
                "low" : round(open + it['low'] / mult,2) }
        print(",".join(map(lambda key: "{x:s}".format(x=str(ans[key])), ans.keys())))

    return data

commands = {
    "trades": lambda args: query('getTrades', {"openedOnly":True}, tradesAns),
    "price": lambda args: query('getTickPrices', {"timestamp": int(args[0]), "symbols": args[1:], "level": 0}, priceTransform),
    "now": lambda args: query('getServerTime', {}, timeAns),
    "history": lambda args: timeQuery('getChartRangeRequest', lambda ctime:{"info": {"period": 1440, "start":ctime-(int(args[0]) * 1000 * 3600 * 24), "end":ctime, "symbol": args[1], "ticks":0}}, histTransform)
}

def main(opt, args):
    commands[opt](args)
    
    
if __name__ == "__main__":
    if(len(sys.argv)>1):
        main(sys.argv[1], sys.argv[2:])
    else:
        main('trades', [])	
