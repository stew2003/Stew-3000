import serial
import time
import sys
import os

# set up the serial connection to the arduino board
serial_port = "/dev/cu.usbserial-AR0KL0FJ"
baud_rate = "9600"

MAX_LOADABLE_BINARY_SIZE = 256

connection = serial.Serial(serial_port, baud_rate, timeout=5)

time.sleep(2) # wait for arduino

with open(sys.argv[1], "rb") as file:
    # never load something too large for the machine
    file_size = os.fstat(file.fileno()).st_size
    if file_size > MAX_LOADABLE_BINARY_SIZE:
        print(f"error: cannot load binary of size {file_size} bytes (> {MAX_LOADABLE_BINARY_SIZE} bytes)")
        sys.exit(1)

    byte = file.read(1)
    while byte:
        connection.flush()

        print("Loader script sent: " + str(bytes(byte)))
        connection.write(bytes(byte))
        
        time.sleep(0.3)

        response = connection.read(connection.inWaiting()) # read all characters in buffer
        print ("Message from arduino: " + str(response))

        byte = file.read(1)

connection.close()