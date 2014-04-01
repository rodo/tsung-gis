.. py:module:: wkb

=======
wkb.erl
=======

Erlang library to convert and generate datas in OGC
`Well Known Binary <http://en.wikipedia.org/wiki/Well-known_text#Well-known_binary>`_
format defined by the Open Geospatial Consortium (OGC) and
described in their Simple Feature Access.

Tsung Exports
=============

Functions exported and callable direcxtly in a Tsung's scenario

.. index:: wkb_point/1

wkb_point/1
-----------

Return a point with random coordinate in WKB format.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   24> wkb:wkb_point({Pid, DynVars}).          
   "0000000001C03FB4A8BE87B3804042B06F3D378054"


Other exports
=============

.. index:: float_to_wkb/1

float_to_wkb/1
--------------

Convert a float to OGC Well Known Binary format

Parameters : float

Return : string

.. code-block:: erlang

   79> wkb:float_to_wkb(42.14). 
   "404511EB851EB852"


.. index:: wkb_point/2

wkb_point/2
-----------

.. index:: wkb_linestring/1

wkb_linestring/1
----------------

Return a linestring encoded in WKB format.

Paramaters : Atrray of tuples {lat, lon}

Return : string

.. code-block:: erlang

   84> wkb:wkb_linestring([{42.5, -5.5},{135.2,5}]).         
   "000000000200000002C016000000000000404540000000000040140000000000004060E66666666666"
