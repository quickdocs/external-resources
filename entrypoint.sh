#!/bin/bash

exec sbcl --noinform --non-interactive --load .qlot/setup.lisp \
  --eval '(progn (format *error-output* "~&Loading...~%") (ql:quickload :external-resources :silent t))' \
  --eval '(external-resources::main)'
