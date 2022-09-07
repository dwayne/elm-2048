#!/usr/bin/env bash

set -e

port="${1:-8000}"

caddy file-server --browse --root experiments --listen :"$port"
