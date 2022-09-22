#!/usr/bin/env bash

cd "$experiments/score" && elm make src/Main.elm --debug --output=app.js
