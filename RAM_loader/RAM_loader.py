import serial
import time
import sys

# set up the serial connection to the arduino board
serial_port = "/dev/cu.usbserial-AR0KL0FJ"
baud_rate = "9600"

connection = serial.Serial(serial_port, baud_rate, timeout=5)

time.sleep(2) # wait for arduino

with open(sys.argv[1], "rb") as file:
    byte = file.read(1)
    while byte:
        connection.flush()

        print("Python sent: " + str(bytes(byte)))
        connection.write(bytes(byte))
        
        time.sleep(0.3)

        response = connection.read(connection.inWaiting()) # read all characters in buffer
        print ("Message from arduino: " + str(response))

        byte = file.read(1)

connection.close()