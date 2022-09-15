#!/usr/bin/env bash

sass="$PROTOTYPE/node_modules/.bin/sass"

"$sass" "$PROTOTYPE/styles/index.scss" "$PROTOTYPE/index.css"
