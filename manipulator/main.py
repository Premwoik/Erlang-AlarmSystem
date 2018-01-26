import socket
import threading
import time


def loop(soc: socket.socket):
    while True:
        rec = soc.recv(1024)
        if len(rec) == 0:
            break
        display(rec)


def display(rec):
    try:
        print(str(rec, 'utf-8'))
    except :
        print(rec)


if __name__ == "__main__":
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 8091))
    threading.Thread(target=loop, args=[s]).start()
    while True:
        in_ = input('>')
        s.send(str.encode(in_))
        if in_ == 'quit':
            break
        time.sleep(0.5)
