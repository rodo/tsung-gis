.. py:module:: slippymap

=============
slippymap.erl
=============

Tsung Exports
=============

* deg2num/3

* num2deg/3

* num2bbox/3

* tmstowms/1

* tile2lat/2

* tile2lon/2


deg2num/3
=========

.. code-block:: erlang

   3> slippymap:deg2num(48.84098,2.58741,18).    
   {132956,90202}

num2deg/3
=========

.. code-block:: erlang

   5> slippymap:num2deg(132960,90200,18).
   {2.59277344,48.84302835}
