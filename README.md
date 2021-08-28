# cubeViz2

## This project is a continuation of the work that started as an experiment written in Elm.

In contrast to the previous experiment, I decided to not implement a type checker, and just rely on Agda. Thanks to this, this tool accepts the usual Agda code and not some weird-agda-like-clone like the earlier version.

One consequence of this is that this tool accepts fully normalized code, with all implicit arguments elaborated. (Agda is able to produce such terms in interactive mode). 

|two dimensional example|three dimensional example|
|--|--|
| [![2d example](https://i.imgur.com/D0vHW6qm.jpeg)](https://vimeo.com/589313743) | [![3d example](https://i.imgur.com/epATNXym.jpg)](https://vimeo.com/589305663) |



