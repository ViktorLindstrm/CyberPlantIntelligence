#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const int AnalogIn  = A0;
const char *SERVER_WIFI_SSID = "*******";
const char *SERVER_WIFI_PASS = "*******";
const long sleepTimeS = 30 * 60;
const String name = "real2";

void setupWiFi()
{
   Serial.print("Connecting to WiFi ");
   WiFi.begin(SERVER_WIFI_SSID,SERVER_WIFI_PASS);
   while(WiFi.status() != WL_CONNECTED)
   {
     delay(500);
     Serial.print(".");
   }

   Serial.println("Connected");
}

void stopWiFi(){
  WiFi.disconnect();
}
void setup() {
   
  pinMode(2, OUTPUT);
  digitalWrite(2, LOW);
  delay(500);
  int readingIn = analogRead(AnalogIn);
  int retVal = ((readingIn * 100) / 1024);
  setupWiFi();

  HTTPClient http;
  http.begin("http://192.168.1.135:8080/sensor/"+name);
  http.addHeader("Content-Type", "application/json");

  
  String pld = "{\"data\":\"";
  pld +=retVal;
  pld +=+"\"}";
  
  http.POST(pld);
  http.end();
 
  ESP.deepSleep(sleepTimeS * 1000000);
}

void loop() {
}
