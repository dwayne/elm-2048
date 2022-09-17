#!/usr/bin/env bash

set -e

cd "$experiments/score"

elm make src/Main.elm --output=app.js --debug
