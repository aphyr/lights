# Lights

A Clojure program to set your Hue lights to various, randomly generated color
palettes. Palettes shift continuously over time, trying to avoid becoming too
white. Great for cocktail parties, or "I'm not sure what I want right now".

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

To have a party:

```
lein run party
```

To change the lights every 5 seconds, run:

```
lein run party -i 5
```

## License

Copyright © 2015-2024 Kyle Kingsbury

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
