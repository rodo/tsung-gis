.. py:module:: randomcoord

===============
randomcoord.erl
===============

Tsung Exports
=============

Functions exported by the ``randomcoord`` module callable by Tsung

* rcoord_array/1

* url/1

rcoord_array/1
--------------

Function :index:`rcoord_array/1` return a tuple of coordinates {Lat,
Lon}, Lat and Lon are float values.

Paramaters : classical tuple of tsung datas {Pid, DynVars}

Return : float tuple

.. code-block:: erlang

   10> randomcoord:rcoord_array({Pid, ts_dynvars:new()}).
   ["70.205974","33.121879"]





rcoord_array/4
--------------

Parameters : Left, Bottom, Right, Top

Return : array of float

Return an array of 4 value representing a bbox 

Other exports
==============

Functions exported by the ``randomcoord`` module not callable in a
Tsung scenario

* rcoord/0

* rcoord/1

* rcoord/2

* rcoord/4

Geolocalized randoms
====================

It is possible to specify country in DynVars to reduce the random
size, this is done by define a DynVars called :index:`country` in your
scenario.


Countries already defined
-------------------------

* France

* Germany

* Portugal

* Spain

Define a new country
--------------------

It's easy as create a new function `rcoord/1` with the country name as
paramater, and return the value of rcoord/4 with the desired bbox. 

In the following example the country name is `groland` which is defined
by 7.14,42.84,14.07,56.84

.. code-block:: erlang

   rcoord("france")->
                rcoord(7.14,42.84,14.07,56.84);
