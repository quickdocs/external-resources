FROM fukamachi/qlot

WORKDIR /app
COPY . /app
RUN qlot install

FROM fukamachi/sbcl
COPY --from=0 /app /app

WORKDIR /app

ENTRYPOINT ["/app/entrypoint.sh"]
