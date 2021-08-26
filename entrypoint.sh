#!/bin/bash

QUICKLISP_HOME=.qlot/

exec ros -S . \
  -e '(progn (format *error-output* "~&Loading...~%") (ql:quickload :external-resources :silent t))' \
  -e '(external-resources::main)'
