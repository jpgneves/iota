[![Build Status](https://travis-ci.org/jpgneves/iota.png?branch=master)](https://travis-ci.org/jpgneves/iota)

iota
====

iota (Inter-dependency Objective Testing Apparatus) - a tool to enforce clean
separation of responsibilities in Erlang code


API declarations
================

A module can be declared as an API module for an application (meaning it's
a module other applications should invoke for functionality) in two ways.

You can explicitly declare the entire module as an API:
```erlang
-api(all). %% Declares all exported functions as part of the module's API.
```

or you can declare which exported functions are part of the API:
```erlang
-export([ foo/1, bar/0 ]).
-api([ foo/1 ]). %% Declares only foo/1 as part of the module's API.
```

Declaring the API functions implicitly declares the module as an API module,
and only declaring the module as an API module implicitly declares all
exported functions as part of the API.

If the module declares all its exported functions as part of its API, iota
will emit a warning.

Additionally, iota will emit warnings if you declare unexported functions
as part of your API, and errors if any application calls non-API functions
in modules belonging to other applications.

Intra-module function calls are accepted regardless if they are declared
as API or not.

Running iota
============

You can run iota simply by doing (all examples use the applications provided
in ```priv```):

    $ make escriptize
    $ ./iota priv/test_app1
    ERROR: test_app2_mod:baz/0 calls non-API function test_app1_mod:xpto/2

    ERROR: test_app1_mod:foo/0 calls non-API module test_app2_mod

    ===== iota report =====
    {test_app1_mod,foo,0} - Errors: 1, Warnings: 0
    {test_app2_mod,baz,0} - Errors: 1, Warnings: 0
    =======================
    Total - Errors:2 Warnings:0

Please note that your application must be compiled before, as iota uses xref for the more
meaningful checks and your declarations need to be baked in as well. :)

You can also ask iota to describe the API of all applications on the given path
for you:

    $ ./iota priv/test_app1 describe-api
    ===== iota report =====
    API for test_app1:
      [{xpto,1}]
    API for test_app2:
      []

Future work
===========

* Improve internal consistency analysis
* Add support for more kinds of checking (e.g. layer checks through external
declarations)
* Add support for whitelisting applications (i.e for applications which will
never have iota annotations)

NOTE
====

When running ```make test``` you'll see that the coverage of some modules is
very low. This is due to the use of
[moka](https://github.com/samuelrivas/moka) for mocking.