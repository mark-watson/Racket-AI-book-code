# install as a local package

    raco pkg install --scope user

When you create a package with "--scope user" you are defining a link to the location of your Racket library source code. For example, after doing this install, look in the file:

    ~/Library/Racket/8.10/links.rktd

On my system, I have the following list added:

    ("nlp" (up up up #"GITHUB" #"Racket-AI-book-code" #"nlp"))

