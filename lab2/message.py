from uuid import getnode as get_mac
from struct import *
import time, sys
import sys

show_mac = lambda m : ":".join(map(lambda x: "%02x" % x, pack("!Q", m)[2:]))

class UDPMessage:
    def __init__(self, mac, time):
        self.mac = mac
        self.time = time

    def pack_msg(self):
        return pack("!6sQ", pack("!Q", self.mac)[2:], self.time)

    def __str__(self):
        return "mac = " + show_mac(self.mac) + ", time = " + time.ctime(self.time)

def create_message():
    return UDPMessage(get_mac(), int(round(time.time())))

def unpack_msg(msg):
    m, t = unpack("!6sQ", msg)
    return UDPMessage(int.from_bytes(m, "big"), t)

class ChatMessage:
    def __init__(self, time, mac, lth, msg):
        self.time = time
        self.mac = mac
        self.msglen = lth
        self.msg = msg

    def pack_msg(self):
        #print(pack("!Q", self.time))
        #print(pack("6s", pack("!Q", self.mac)[2:]))
        #print(pack("!Q", self.msglen))
        fmt = "!Q6sL" + str(self.msglen) + "s"
        return pack(fmt, self.time, pack("!Q", self.mac)[2:], self.msglen, bytes(self.msg, 'UTF-8'))

    def __str__(self):
        return "mac = " + show_mac(self.mac) + ", time= " + time.ctime(self.time) + ", msg= " + self.msg

def create_chat_message(msg, time):
    return ChatMessage(time, get_mac(), len(bytes(msg, 'UTF-8')), msg)

def unpack_chat_msg(msg):
    fmt = "!Q6sL"+str(len(msg)-18) + "s"
    (t, mac, l, m) = unpack(fmt, msg)
    return ChatMessage(t, int.from_bytes(mac, "big"), l, m.decode('UTF-8'))

class ClientInfo:
    def __init__(self, time_offset, ip):
        self.time_offset = time_offset
        self.ip = ip
        self.msg_cnt = 0
