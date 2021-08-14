import serial
import time
import sys
import os

from serial.serialutil import SerialException

# set up the serial connection to the arduino board
serial_port = "/dev/cu.usbserial-AR0KL0FJ"
baud_rate = "9600"

MAX_LOADABLE_BINARY_SIZE = 256

try:
    connection = serial.Serial(serial_port, baud_rate, timeout=5)
except SerialException:
    print("Plug in the Arduino!")
    sys.exit(-1)

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

        print(f"Loader script sent: 0x{bytes(byte).hex()}")
        connection.write(bytes(byte))
        
        time.sleep(0.2)

        # read all characters in buffer
        response = connection.read(connection.inWaiting())

        # message comes back as bytes and it has \r\n which we strip off
        print (f"Message from arduino: {response.decode('utf-8')[:-2]}")

        byte = file.read(1)

connection.close()