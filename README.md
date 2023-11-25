# Lights

A Clojure program to set your Hue lights to various, randomly generated color
palettes. Palettes shift continuously over time, trying to avoid becoming too
white. Great for cocktail parties, or "I'm not sure what I want right now".

You can group bulbs by naming them (e.g.) "Hall light 1", "Hall light 2", ....
These lights will be identified as a cluster, and assigned similar colors.

There's a bunch of horrible color manipulation code in here which is almost
certainly wrong. I've never gotten some parts to make sense; the Hue API
documentation is often confusing. Gamut checking is almost certainly broken,
for instance. If you can fix this, please do.

## Getting Started

## Auth

First, you'll need the address of your Hue bridge. You can find this in the Hue
app. Or, if it's on the same LAN as your machine, check `ip addr show wlan0 |
grep inet` to get the CIDR spec for your wireless interface. Replace `wlan0`
with your network interface, or just use `ip addr` for a full list.

Say your wifi address is 10.0.0.10/24. You can use `nmap` to find any Philips
devices on the network.

```
$ sudo nmap -sP 10.0.0.10/24 | grep -B 2 Philips
Nmap scan report for 10.0.0.20
Host is up (0.013s latency).
MAC Address: EC:B5:FA:92:EA:DA (Philips Lighting BV)
```

If your address is 10.0.0.20, you can run...

```
lein run auth --address 10.0.0.20
```

... which will ask you to press your bridge's button, and generate credentials
used to interact with the lights. These credentials are saved in `.config.edn`
in the current directory.

## Running

To have a party:

```
lein run party
```

To change the lights every 2 seconds, run:

```
lein run party -i 2
```

## License

Copyright © 2015 Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
