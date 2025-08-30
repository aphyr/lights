# Lights

A Clojure program to set your Hue lights to various, randomly generated color
palettes. Palettes shift continuously over time, trying to avoid becoming too
white. Great for cocktail parties, or "I'm not sure what I want right now".

[![A demonstration video of the lights program being triggered by presses of a wireless remote button](https://img.youtube.com/v1/SWW0u2QkY1s/0.jpg)](https://www.youtube.com/watch?v=SWW0u2QkY1s)

You can group bulbs by naming them (e.g.) "Hall light 1", "Hall light 2", ....
These lights will be identified as a cluster, and assigned similar colors.

You'll also find a v small implementation of part of the Hue v2 API in here,
and a surprisingly complete colorspace conversion library. Might be worth
pulling these out into other libraries later---if you'd like to do this, email
aphyr@aphyr.com

## Getting Started

## Auth

To set up the app, run `lein run auth`. It'll automatically discover a Hue
bridge on your network. If you'd like to pass a specific bridge, provide its IP
like so:

```
lein run auth --address 10.0.0.20
```

Auth will ask you to press your bridge's button, and generate credentials
used to interact with the lights. These credentials are saved in `.config.edn`
in the current directory.

## Running

To set the lights once, taking 2 seconds to transition:

```
lein run once -i 2
```

To have a party:

```
lein run party
```

To change the lights every 5 seconds, run:

```
lein run party -i 5
```

To cap the maximum brightness at 60%, use `-b 50`.

If you want to call this from home-assistant or some other automation, it includes the world's tiniest web server. Use:

```
lein run serve -i 1
```

This listens on port 8946 for any request, which triggers the calculation of a
new palette, which is applied in 1 second. Great for making a button that gives
your house a random color scheme. If you need a little systemd file to run it,
use `lein uberjar` to build a fat jar, drop your `.config.edn` somewhere
useful, then make a daemon out with something like this in
`/etc/systemd/system/lights.service`:

```
[Unit]
Description=Daemon to control Hue lights
After=network.target

[Service]
User=lights
Group=lights
WorkingDirectory=/home/lights
ExecStart=/usr/bin/java -jar /home/lights/lights.jar serve -i 1

[Install]
WantedBy=multi-user.target
```

## Notes

The Hue API, as far as I can tell, only lets you update one light at a time.
This can mean any color scheme takes 5-10 seconds to apply. To work around
this, we create a zone called `global` with every light, and a scene, also
called `global`, which includes the state of every light. We update this scene
and recall it in order to set the lights. This is a little impolite, in that it
leaves global state sitting around on the Hue bridge, but the speedup is huge.

## License

Copyright © Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
