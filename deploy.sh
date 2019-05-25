#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
npm run webpack:production
rsync -av build/ pelam.fi:/var/www/knektkajen/