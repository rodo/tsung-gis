.. py:module:: xyz

=======
xyz.erl
=======

Tsung Exports
=============

The following functions are directly callable in Tsung's scenarios.

* action_random/1

* move_first/1

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

The following functions are exported but are not callable directly in
Tsung's scenarios.

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


.. index:: xyz_layers

xyz_layers
----------

List of layer's names in comma separated value format.

.. code-block:: xml

   <setdynvars sourcetype="value" value="roads,rivers,sea">
       <var name="xyz_layers" />
   </setdynvars>



Moving functions
================

.. index:: move_first_layers/1

move_first_layers/1
-------------------

First move on all layers defined in DynVars, 

Return a list of string representing tiles's urls

Required var : xyz_layers, first_url

Sample usage :

.. code-block:: xml

   <setdynvars sourcetype="erlang" callback="xyz:move_first_layers">
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

Required var : xyz_layers, list_url

Sample usage :

