import socket
import threading
import time

def loop(soc: socket.socket):
    while True:
        print(str(soc.recv(1024), 'utf-8'))


if __name__ == "__main__":
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('localhost', 8091))
    threading.Thread(target=loop, args=[s]).start()
    while True:
        s.send(str.encode(input('>')))
        time.sleep(0.1)

