# Reorganization of sources for integration into project "Neo"

Current task
------------

Sorting and pairing down original sources.

General idea
------------

A game engine has a large source code footprint and many different moving pieces (of which menuing and interfacing with X11 are a key part).

Paradigms
---------

- Direct system bindings live at Engine/neo-api- * .ads while platform specific code lives in its corresponding Engine/Systems/ * directory
- All menuing goes in Engine/neo-world-menu.ad *
- Texture and asset loading code goes in Engine/neo-data- * - * .ad *
