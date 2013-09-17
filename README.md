iota
====

iota (Inter-dependency Objective Testing Apparatus) - a tool to enforce clean separation of responsibilities in Erlang code


API declarations
================

A module can be declared as an "API module" (meaning it's the module other applications should invoke)
for your application in two ways.

You can explicitly declare the module as an API through a special '-iota' attribute:

    ...
    -iota([{is_api, true}]).
    ...

Alternatively you can declare which exported functions are part of the module's API:

    ...
    -api(all). %% Declares all exported functions are part of the module's API.
    ...

or

    ...
    -export([ foo/1, bar/0 ]).
    -api([ foo/1 ]). %% Declares only foo/1 as part of the module's API.
    ...

Declaring the API functions implicitly declares the module as an API module, and only declaring the
module as an API module implicitly declares all exported functions as part of the API.

If the module declares all it's exported functions as part of its API, iota will emit a warning.

Running iota
============

You can run iota simply by doing:

    $ make
    $ ./iota all <path_to_your_application>

Please note that your application must be compiled before, as iota uses xref for the more
meaningful checks and your declarations need to be baked in as well. :)

A sample application is provided in /priv.

Future work
===========

* Improve internal consistency analysis
* Add support for more kinds of checking (e.g. layer checks through external
declarations)
* Add support for whitelisting applications (i.e for applications which will
never have iota annotations)