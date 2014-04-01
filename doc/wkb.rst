.. py:module:: wkb

=======
wkb.erl
=======

Convert and generate datas in `Well Known Binary
<http://en.wikipedia.org/wiki/Well-known_text#Well-known_binary>`
format defined by the Open Geospatial Consortium (OGC) and
described in their Simple Feature Access.

Tsung Exports
=============

Functions exported and callable by Tsung

* wkb_point/1

wkb_point/1
-----------

Retrun a point witn random coordinate in WKB format.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   24> wkb:wkb_point({Pid, DynVars}).          
   "0000000001C03FB4A8BE87B3804042B06F3D378054"


Other exports
=============

* float_to_wkb/1

* wkb_point/1

* wkb_point/2
