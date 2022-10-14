#!/usr/bin/env bash

cd "$prototype" \
&& sass --embed-sources sass/index.scss index.css \
&& sed -i s/{{ROOT}}// index.css
