# v6hex: IPv6 hex address library for Erlang

(and more IPv6 goodies, including v6-to-v4 resolver codes)

* Version 0.0.3 17-MAR-2014
* written by Kenji Rikitake
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2012, 2014 Kenji Rikitake. All rights reserved.

See LICENSE.txt for the license (MIT license).

## Tested platforms

* FreeBSD/amd64 10.0-STABLE with Erlang/OTP 17.1
* OS X 10.9.3 with Erlang/OTP 17.1

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
* Ryosuke Nakai

## ACKNOWLEDGMENTS

Erlang Solutions kindly gave Kenji Rikitake
an opportunity to give a presentation
about this software at Erlang Factory SF Bay 2012.

