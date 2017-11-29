FROM haskell
EXPOSE 80
RUN stack setup
ADD . /app
WORKDIR /app
RUN make install
ENTRYPOINT ~/.local/bin/Kaesekaestchen start