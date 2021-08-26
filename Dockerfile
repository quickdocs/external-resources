FROM fukamachi/qlot

WORKDIR /app
COPY . /app
RUN qlot install

FROM clfoundation/sbcl:slim

WORKDIR /app
COPY --from=0 /app /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf"

ENTRYPOINT ["/app/entrypoint.sh"]
