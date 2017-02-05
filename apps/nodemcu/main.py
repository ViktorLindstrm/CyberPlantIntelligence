import time
import urequests
import machine

sensorid = "real1"
url = "http://192.168.1.135:8080/sensor/"+sensorid
headers = {'content-type': 'application/json'}

rtc = machine.RTC()
rtc.irq(trigger=rtc.ALARM0, wake=machine.DEEPSLEEP)

pin = machine.Pin(5,machine.Pin.OUT)  
adc = machine.ADC(0)

def measure():
    pin.high()
    time.sleep_ms(100)
    ret = adc.read()
    pin.low()
    return int(ret/1024 * 100)

def send_data():
    val = measure()
    data = '{"data":"%s"}' % (val)
    urequests.post(url,data=data,headers=headers)
    return

def run():
    rtc.alarm(rtc.ALARM0, 900000)
    send_data()
    time.sleep(120)
    machine.deepsleep()
    return

# send_data()
def loop():
    while True:
        send_data()
        time.sleep(600)


