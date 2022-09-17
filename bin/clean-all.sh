#!/usr/bin/env bash

set -e

for clean in $(find "$project" -name clean.sh -type f); do
  $clean
done
