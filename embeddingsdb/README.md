# WORK IN PROGRESS - IGNORE FOR NOW!!

## install as a local package

    raco pkg remove
    raco pkg install --scope user

If I change the source code, run the following to update the linked (installed in place) package **embeddingsdb**:

    raco make main.rkt
