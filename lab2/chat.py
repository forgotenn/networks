from message import *
from socket import *
from multiprocessing import Process

clients = dict()
my_msg = []

UDP_PORT = 1235
UDP_IP = "255.255.255.255"
TCP_PORT = 1236
BUFFER_SIZE = 1024

def TCPsender(msg, client):
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect((client.ip, TCP_PORT))
    to_send = create_chat_message(msg, int(round(time.time())) - client.time_offset).pack_msg()
    sock.send(to_send)
    sock.close()

def TCPReceiver():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
    sock.listen(1)
    while True:
        conn, addr = sock.accept()
        print('Connection address:', addr)
        data = conn.recv(BUFFER_SIZE)
        print("received data:" + unpack_chat_msg(data) + "from ip = " + str(addr[0]))
        conn.close()
    sock.close()

def UDPsender():
    sock = socket(AF_INET, SOCK_DGRAM)
    sock.setsockopt(SOL_SOCKET, SO_BROADCAST, 1)
    while True:
        toSend = create_message().pack_msg()
        sock.sendto(toSend, (UDP_IP, UDP_PORT))
        time.sleep(5)

def UDPreceiver():
    global clients
    sock = socket(AF_INET, SOCK_DGRAM)
    sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
    sock.bind(('<broadcast>', UDP_PORT))
    while True:
        data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
        data = unpack_msg(data)
        if clients.get(data.mac) == None:
            clients[data.mac] = ClientInfo(int(round(time.time())) - data.time, addr[0])
        client = clients[data.mac]
        if clients[data.mac].msg_cnt < len(my_msg):
            for i in range(clients[data.mac].msg_cnt, len(my_msg)):
                TCPsender(my_msg[i], clients[data.mac])
                clients[data.mac] += 1

if __name__ == '__main__':
    Process(target=UDPsender).start()
    Process(target=UDPreceiver()).start()
    Process(target=TCPReceiver()).start()
    while True:
        m = input_var = input()
        my_msg.append(m)