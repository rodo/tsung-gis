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

* r_point/1, `r_point_srid/1`_

<<<<<<< HEAD


=======
.. index:: r_point/1
>>>>>>> f04b7c9cd849f5151cdd4e7808bd21ba9372a959

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

<<<<<<< HEAD
The :index:`r_point_srid/1` functions works like :ref:`r_point/1 <r_point/1>` but
enclose the ``ST_Point()`` result in ``SetSRID()`` postgis function.
=======
The function `r_point_srid/1` works `r_point/1`_ but
enclose the result in ``SetSRID()`` postgis function.
>>>>>>> f04b7c9cd849f5151cdd4e7808bd21ba9372a959

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   12> postgis:r_point_srid({Pid, DynVars}).
   "ST_SetSRID(ST_Point(69.896366, 63.997280), 4326)"

r_box2d/1
---------

The function `r_box2d/1` returns a SQL command to build a 2
dimension box with the `ST_MakeBox2D <http://postgis.net/docs/ST_MakeBox2D.html>`_.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   11> postgis:r_box2d_srid({Pid, Dynvars}).

   "ST_SetSRID(ST_MakeBox2d(ST_Point(-72.170899, 51.890770),
   ST_Point(30.764721, 75.803965))), 4326)"


.. index:: r_box2d_srid/1

r_box2d_srid/1
--------------

The function `r_box2d_srid/1` works like `r_box2d/1`_ but
enclose the result in ``SetSRID()`` postgis function.

Paramaters : Tsung tuple {Pid, DynVars}

Return : string

.. code-block:: erlang

   10> postgis:r_box2d({Pid, Dynvars}).
   "ST_MakeBox2d(ST_Point(67.792555, -58.145776), ST_Point(163.686023,
   88.730874))"
