# THESE EXAMPLES ARE NOT YET IN BOOK

# Install dependencies

On macOS:

brew install poppler

Then install the following in your .profile, .zshrc, .bashrc, etc.:

export DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/lib/

Then install Racket packages:

raco pkg install pdf-read
raco pkg install racket-poppler

Note: curently on macOS, I can't get these exampels to work in DrRacket, so run on the comand line; for example:

racket pdf2text.rkt
