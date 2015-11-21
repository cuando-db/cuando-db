#!/usr/bin/env bash
../cuando init example schema.json prefs.json dims.json
../cuando merge example doc.json
../cuando query example query.json
