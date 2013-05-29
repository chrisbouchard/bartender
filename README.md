Introduction
------------

Bartender is a statusbar server written in Haskell and built around the dzen2
statusbar. Client programs connect to the server and publish widgets.

Bartender is built around a very lightweight protocol. Clients announce
themselves to the server, and then proceed to send periodic updates which are
displayed on the bar by the server. Clients are dropped after a specified
timeout if no updates are received.

Installation
------------

Bartender uses `cabal` to build and install itself, so installation should be
as simple as running

    cabal build

followed by

    cabal install

You can run

    cabal haddock

to generate HTML documentation of the StatusBar library.

Running
-------

To run bartender, run the `bartender` executable. Note that this will dump all
output to the terminal (i.e., standard out). Most users will want to direct
point bartender at an instance of dzen. Assuming `dzen2` is installed in your
path, you can run them at the same time in a Unix-style shell by running

    bartender | dzen2

For a list of optional arguments and flags, run `bartender --help`.

Writing a Client
----------------

Two sample clients are included: `TestClient.hs` and `WatchClient.hs`. The
first is a basic client that displays the current date and time. The second is
a useful client that watches the output of a command and displays that output,
much like the `watch` command in the Unix shell.

A client program runs a `BarClient` monad, which handles connecting to the
server and encapsulates the protocol.

Dependencies
------------

The following cabal packages are needed to build Status-Bar-Server:

* base
* cmdargs
* hslogger
* mtl
* network
* old-locale
* process
* stm
* time
* transformers

They should be downloaded as needed by the build system.

