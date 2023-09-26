# install as a local package

    raco pkg install --scope user

If I change the source code, run the following to update the linked (installed in place) package **nlp**:

    raco make main.rkt
