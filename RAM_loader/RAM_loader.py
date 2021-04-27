import serial
import time

# send a single byte to the arduino
def send(connection, byte):
    connection.write(bytes(byte))

# receive data from the arduino
def receive(connection):
    return connection.read()


# set up the serial connection to the arduino board
serial_port = "/dev/ttyACM0"
baud_rate = "9600"
connection = serial.Serial(serial_port, baud_rate)

connection.close()