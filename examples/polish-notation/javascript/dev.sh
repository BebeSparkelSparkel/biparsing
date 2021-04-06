#!/bin/sh
xterm -e npm run test-watch &
xterm -e vim -S session.vim &
xterm -e npm run lint-watch &
