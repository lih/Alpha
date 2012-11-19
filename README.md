Welcome to Alpha !
==================

Alpha is a simple programming language, whose compiler is written in Haskell (although
later versions might be written in Alpha). For the moment, it only handles the x86_64
architecture and, though it might be compiled and run on another, it is strongly advised
for your own safety that you do not do so (then again, if you love segfaults, who am I to
judge :-D )

For more information about the language itself and its quirks and turns, I invite you to
visit the [Alpha Lang Website][alpha-lang]

Compiling Alpha
---------------

In order to compile Alpha, you will need the 'cabal-install' tool for building Haskell
projects. Using cabal-install, you will just have to execute the following commands (in
the project root directory) to compile the compiler:

    cabal configure --user
    cabal install

Note: Alpha uses some recent version of certain Haskell libraries and might not compile on
old or out-of-date systems. Unfortunately, the only way to fix that is to update your
system, for there are enough problems as it is trying to write the compiler without having
to deal with backward compatibility. I hope you can understand.

Running Alpha
-------------

Once the compile step has completed (hopefully without too much hassle), you may happily
compile your Alpha programs with that simple command:

    alpha <language>:<symbol>

where `<symbol>` and `<language>` are respectively the name of the entry symbol to your
program and the name of the language in which the compiler might find that symbol. 

Alpha also has an interactive mode that you may use by calling it without arguments. That
mode isn't very talkative yet so don't be alarmed if you stumble upon it by chance and
just hit Ctrl+D (or you might enter some commands. Just because it doesn't echo anything
doesn't mean it doesn't work :-) )

For more information about exactly what languages and symbols are, you might want to check
out the [Alpha Lang Documentation][alpha-doc]. If you want more immediate help on how to
run Alpha, you may also run the command 'alpha -h' which shows you the options supported
by your version of Alpha.

[alpha-lang]: http://www.alpha-lang.info/ "The Alpha Lang Website"
[alpha-doc]: http://www.alpha-lang.info/way/spec.html "Learn About Alpha"
