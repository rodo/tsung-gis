.. py:module:: postgis

===========
postgis.erl
===========

Build SQL commend to manipulate geometric objects in a 
`Postgis <http://postgis.net/>`_ database.

The ``postgis`` module uses the ``randomcoord`` module to generate
random datas.

Tsung Exports
=============

Functions exported by the **postgis** module callable by Tsung. All
functions which name that begins with **r_** return random data.

* r_box2d/1, r_box2d_srid/1

* r_point/1, r_point_srid/1

.. _rpoint_one:
.. index:: r_point/1

r_point/1
---------

The function `r_point/1` returns a SQL command to build an `ST_Point <http://postgis.net/docs/ST_Point.html>`_.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   12> postgis:r_point({Pid, DynVars}).
   "ST_Point(69.896366, 63.997280)"

.. index:: r_point_srid/1

r_point_srid/1
--------------

The function `r_point_srid/1` works like :ref:`r_point/1 <rpoint_one>` but
enclose the result in ``SetSRID()`` postgis function.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   12> postgis:r_point_srid({Pid, DynVars}).
   "ST_SetSRID(ST_Point(69.896366, 63.997280), 4326)"

.. _rbox2d_one:

r_box2d/1
---------

The function `r_box2d/1` returns a SQL command to build a 2
dimension box with the `ST_MakeBox2D <http://postgis.net/docs/ST_MakeBox2D.html>`.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   11> postgis:r_box2d_srid({Pid, Dynvars}).

   "ST_SetSRID(ST_MakeBox2d(ST_Point(-72.170899, 51.890770),
   ST_Point(30.764721, 75.803965))), 4326)"


.. index:: r_box2d_srid/1

r_box2d_srid/1
--------------

The function `r_box2d_srid/1` works like :ref:`r_box2d/1 <rbox2d_one>` but
enclose the result in ``SetSRID()`` postgis function.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   10> postgis:r_box2d({Pid, Dynvars}).
   "ST_MakeBox2d(ST_Point(67.792555, -58.145776), ST_Point(163.686023,
   88.730874))"
