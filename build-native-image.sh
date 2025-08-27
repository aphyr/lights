#!/bin/bash

# Builds a native binary using graalvm's `native-image` command.
lein do clean, uberjar &&
native-image \
  --initialize-at-build-time \
  --enable-url-protocols=https \
  --trace-object-instantiation=java.security.SecureRandom \
  --initialize-at-run-time=org.apache.http.impl.auth.NTLMEngineImpl \
  --gc=G1 \
  -jar ./target/lights-*-standalone.jar \
  -o ./target/lights
