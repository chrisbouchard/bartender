Introduction
------------

Status-Bar-Server is exactly what it sounds like: a server built around the
dzen2 statusbar. Client programs connect to the server and publish widgets.

Satus-Bar-Server is built around a very simple protocol. Clients announce
themselves to the server, and then proceed to send periodic updates which are
displayed on the bar by the server. Clients are dropped after a specified
timeout if no updates are received.

Installation
------------

Status-Bar-Server uses `cabal` to build and install itself, so installation
should be as simple as running

    cabal build

followed by

    cabal install

You can run

    cabal haddock

to generate HTML documentation of the StatusBar library.

Running
-------

To run the Status-Bar-Server, run the `StatusBar` executable. For a list of
optional arguments and flags, run

    StatusBar --help

Writing a Client
----------------

Two sample clients are included: `TestClient.hs` and `WatchClient.hs`. The
first is a simple client that display the current date and time. The second is
a useful client that watches the output of a command and displays that output,
much like the `watch` command in the Unix shell.

A client simply runs a `BarClient` monad, which handles connecting to the
server and encapsulates the protocol.

Dependencies
------------

The following Haskell packages are needed to build Status-Bar-Server:

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

