#!/usr/bin/env bash

port="${1:-8000}"

caddy file-server --browse --root $PROTOTYPE --listen :"$port"
