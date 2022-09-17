#!/usr/bin/env bash

set -e

cd "$experiments/merge"

elm make src/Main.elm --output=app.js --debug
