"""
A dummy tracker, used for integration testing. Ignores HTTP
parameters and just returns a dummy result, which it gets
from RESPONSE_PATH (see below).

Based on:
  https://pythonbasics.org/webserver/
"""

from http.server import BaseHTTPRequestHandler, HTTPServer

HOST_NAME = "localhost"
PORT = 4242
RESPONSE_PATH = "/tmp/tracker-response"

class DummyTracker(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "application/octet-stream")
        self.end_headers()
        with open(RESPONSE_PATH, "rb") as f:
            self.wfile.write(f.read())

if __name__ == "__main__":        
    server = HTTPServer((HOST_NAME, PORT), DummyTracker)

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass

    server.server_close()
