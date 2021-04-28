import serial
import time
import sys

if __name__ == "main":
    # set up the serial connection to the arduino board
    serial_port = "/dev/ttyACM0"
    baud_rate = "9600"

    connection = serial.Serial(serial_port, baud_rate, timeout=5)

    time.sleep(2) # wait for arduino

    with open(sys.argv[0], "rb") as file:
        byte = file.read(1)
        while byte:
            connection.flush()

            print("Python sent: " + bytes(byte))
            connection.write(bytes(byte))
            
            time.sleep(1)

            response = connection.read(connection.inWaiting()) # read all characters in buffer
            print ("Message from arduino: " + response)

            byte = file.read(1)


connection.close()