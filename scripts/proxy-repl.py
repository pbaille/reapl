#!/usr/bin/env python3

import socket
import json
import bencodepy
import sys

def start_forwarder():
    # create udp socket to send data
    send_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    # create udp socket to receive data
    receive_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    receive_socket.bind(('localhost', 9997))
    print("Welcome to reapl:")
    print("--------")
    while True:
            msg = sys.stdin.read()
            # if the message is not empty, process it
            if msg:
                print(msg)
                # encode input using bencode
                msg = bencodepy.encode({'code': msg.strip()})

                # send data to external process
                send_socket.sendto(msg, ('localhost', 9999))

                # receive response from external process
                data, addr = receive_socket.recvfrom(1024)

                # parse JSON response and pretty print it
                data = json.loads(data)
                pretty_json = json.dumps(data, indent=4, sort_keys=True)
                print(">---")
                print(pretty_json)
                print("--------")
            # if the message is empty, stop the loop

start_forwarder()
