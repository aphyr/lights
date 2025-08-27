#!/bin/bash

# Builds a native binary using graalvm's `native-image` command.

# First, build a jar
lein do clean, uberjar &&

# Now, run it with the native image agent, to collect a config file that knows about our dependencies
java -agentlib:native-image-agent=config-output-dir=native-image-config -jar target/lights-*-standalone.jar graalvm-profile -i 1 &&

# Note that this will randomly hang like 70% of the time in "Performing
# analysis" for ??? reasons.
# Trying to get static working...
# From https://github.com/oracle/graal/issues/8639
#NATIVE_IMAGE_DEPRECATED_BUILDER_SANITATION=true \
#  --static \
#  --libc=musl \
native-image \
  --initialize-at-build-time \
  --enable-url-protocols=https \
  --trace-object-instantiation=java.security.SecureRandom \
  --initialize-at-run-time='org.apache.http.impl.auth.NTLMEngineImpl,clojure.core.reducers__init,clojure.core.server__init,clojure.pprint__init,clojure.stacktrace__init,clojure.spec.alpha__init,clojure.pprint.dispatch__init,clojure.java.io__init' \
  --gc=G1 \
  -H:ConfigurationFileDirectories='./native-image-config' \
  -jar ./target/lights-*-standalone.jar \
  -o ./target/lights
