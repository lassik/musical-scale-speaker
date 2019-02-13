#!/bin/sh
set -eux
cd "$(dirname "$0")"
elm make --output=elm.js src/Main.elm
