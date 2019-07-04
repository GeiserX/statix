import argparse
from unifi.controller import Controller

c = Controller('X.X.X.X', 'admin', 'PASSWORD', '8443', 'v3', 'emartinez')
aps = c.get_aps()
ap_names = dict([(ap['mac'], ap['name']) for ap in aps])
clients = c.get_clients()
clients.sort(key=lambda x: -x['rssi'])
print(clients)

c = Controller('Y.Y.Y.Y', 'admin', 'PASSWORD', '8443', 'v3', 'em-oficinas-centrales')
aps = c.get_aps()
ap_names = dict([(ap['mac'], ap['name']) for ap in aps])
clients = c.get_clients()
clients.sort(key=lambda x: -x['rssi'])
print(clients)
