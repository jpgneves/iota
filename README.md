iota
====

iota (Inter-dependency Objective Testing Apparatus) - a tool to enforce clean separation of responsibilities in Erlang code

Layer declarations
==================

Add a 'layer' attribute to your module, like this:

    -module(foo).
    -layer(presentation).
    ...

iota currently enforces a single layer per module, and the layer _must_ take the form of an Erlang atom.


API declarations
================

A module can be declared as an "API module" (meaning it's the module other applications or layers should invoke)
for your application in two ways.

You can explicitly declare the module as an API through a special '-iota' attribute:

    ...
    -iota([{is_api, true}]).
    ...

Alternatively you can declare which exported functions are part of the module's API:

    ...
    -api(all). %% Declares all exported functions are part of the module's API.
    ...

    ...
    -export([ foo/1, bar/0 ]).
    -api([ foo/1 ]). %% Declares only foo/1 as part of the module's API.
    ...

Declaring the API functions implicitly declares the module as an API module, and only declaring the module as an API
module implicitly declares all exported functions as part of the API.

If the module declares all it's exported functions as part of its API, iota will emit a warning.
