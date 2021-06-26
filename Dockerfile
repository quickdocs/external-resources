FROM clfoundation/sbcl

WORKDIR /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
RUN set -x; \
  sbcl --noinform --non-interactive --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp && \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"

ENTRYPOINT ["/app/entrypoint.sh"]