plantsys
=====
Contains following modules

        [sup]
        /  \
       /    \
     [Mng]   \
          [node_sup]
            / ...
          [node] ..

Modules
========
sup
----
Overall supervisor of mangaer and supervisor for nodes
mng
----
Manages nodes. Handles start, stop and updates of the nodes
node_sup
---------
supervisor of the nodes
node
----
contain raw data and settings for the nodes. Thought is that one node is one
or watering station.



Build
=======

    $ rebar3 compile
