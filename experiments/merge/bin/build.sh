#!/usr/bin/env bash

cd "$experiments/merge" && elm make src/Main.elm --debug --output=app.js
