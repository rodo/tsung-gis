.. py:module:: postgis

===========
postgis.erl
===========

The ``postgis`` module uses the ``randomcoord`` module to generate
random datas. All dynamic variables defined in 

Tsung Exports
=============

Functions exported by the **postgis** module callable by Tsung. All
functions which name that begins with **r_** return random data.

* r_box2d/1, r_box2d_srid/1

* r_point/1, r_point_srid/1




r_point/1
---------

Function :index:`r_point/1` return a tuple of coordinates {Lat,
Lon}, Lat and Lon are float values. 

Paramaters : classical tuple of tsung datas {Pid, DynVars}

Return : string

.. code-block:: erlang

   12> postgis:r_point({Pid, DynVars}).
   "ST_Point(69.896366, 63.997280)"

r_point_srid/1
--------------

The :index:`r_point_srid/1` functions works like :ref:`r_point/1 <r_point/1>` but
enclose the ``ST_Point()`` result in ``SetSRID()`` postgis function.

Paramaters : classical tuple of tsung datas {Pid, DynVars}

Return : string

.. code-block:: erlang

   12> postgis:r_point_srid({Pid, DynVars}).
   "ST_SetSRID(ST_Point(69.896366, 63.997280), 4326)"
