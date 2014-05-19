.. py:module:: tms

=======
tms.erl
=======

Tsung Exports
=============

The following functions are directly callable in Tsung's scenario.

* move_first/1

* move_next/1

* move_north/1, move_south/1, move_west/1, move_east/1

* move_first_layers/1

* move_north_layers/1

* move_south_layers/1

* move_east_layers/1

* move_west_layers/1

* move_random_layers/1

* zoom_more_layers/1

* zoom_less_layers/1

* zoom_random_layers/1

* action_random_layers/1

* get_urlblock/1

* urlzxy/1

Other exports
=============

The following functions are exported but are not callable directly on
a Tsung scenario.

* get_urlfrom/2

Dynamic Variables
=================

.. index:: map_height

map_height
----------

Define the map's height in pixel, integer value.

.. code-block:: xml

   <setdynvars sourcetype="value" value="500">
       <var name="map_height" />
   </setdynvars>



.. index:: map_width

map_width
---------

Define the map's width in pixel, integer value.

.. code-block:: xml

   <setdynvars sourcetype="value" value="700">
       <var name="map_width" />
   </setdynvars>


.. index:: tms_layers

tms_layers
----------

List of layer's names in comma separated value format.

.. code-block:: xml

   <setdynvars sourcetype="value" value="roads,rivers,sea">
       <var name="tms_layers" />
   </setdynvars>



Moving functions
================

.. index:: move_first_layers/1

move_first_layers/1
-------------------

First move on all layers defined in DynVars, 

Return a list of string representing tiles's urls

Required var : tms_layers, first_url

Sample usage :

.. code-block:: xml

   <setdynvars sourcetype="erlang" callback="tms:move_first_layers">
     <var name="list_url" />
   </setdynvars>

   <foreach name="element" in="list_url">
     <request subst="true">
       <http url="/%%_element%%.png" method="GET" version="1.1"/>
     </request>
   </foreach>


Result

.. code-block:: erlang

   ["a/2/1/1","a/2/1/2","a/2/2/1","a/2/2/2",
    "b/2/1/1","b/2/1/2","b/2/2/1","b/2/2/2",
    "c/2/1/1","c/2/1/2","c/2/2/1","c/2/2/2"],

move_north_layers/1
-------------------

As move_first_layers the function will return a list of string
representing tiles's urls. Urls are compute 

Required var : tms_layers, list_url

Sample usage :

