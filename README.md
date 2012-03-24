# v6hex: IPv6 hex address library for Erlang

* Version 0.0.1
* written by Kenji Rikitake
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2012 Kenji Rikitake. All rights reserved.

See LICENSE.txt for the license (MIT license).

## Tested platforms

* FreeBSD/amd64 9.0-RELEASE with Erlang/OTP R15B

## Building 

* Use GNU make and then

    make compile

(Note: on FreeBSD, GNU make should be invoked as `gmake`.)

The build script is Basho's rebar at <https://github.com/basho/rebar>,
which will be automatically fetched under the directory `support/`.

## Documentation (not yet)

* For the HTML documentation files of the Erlang source 

    make doc

    The documentation will be accessible at `doc/index.html`.

## Testing (not yet)

* For unit testing with EUnit, do

    make eunit

## TODO

* Edoc-compatible documentation

## Code authors:

* Kenji Rikitake
* Tuncer Ayaz for `support/getrebar` script

## THANKS to:

* Francesco Cesarini
* Fr&eacute;d&eacute;ric Trottier-H&eacute;bert

## ACKNOWLEDGMENTS

Erlang Solutions kindly gave Kenji Rikitake
an opportunity to give a presentation
about this software at Erlang Factory SF Bay 2012.

