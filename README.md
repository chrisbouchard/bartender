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
output to the terminal (i.e., standard out). Most users will want to point
bartender at an instance of dzen. Assuming `dzen2` is installed in your path,
you can run them at the same time in a Unix-style shell by running

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

The following cabal packages are needed to build bartender:

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

Future Work
-----------

Bartender is still very alpha code. Right now it's completely dependent on
dzen, but the eventual goal is to be status-bar agnostic. Different status-bars
will be handled by plugins.

Another plan is an extended protocol with support for user interaction, such as
mouse clicks. This will probably require the client to register with the server
so events can be sent. This could also allow the server to notify clients when
they are about to be dropped.

This was my first Haskell project of any significant scope. I'm sure a lot of
the code needs to be cleaned up, and hopefully some of it can be generalized
and replaced with existing library functions.

