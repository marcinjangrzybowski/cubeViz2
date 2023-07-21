# cubeViz2

_work in progress_

## This project is a continuation of the [work](https://github.com/marcinjangrzybowski/cubeViz) that started as an experiment written in Elm.

In contrast to the previous experiment, I decided to not implement a type checker, and just rely on Agda. Thanks to this, this tool accepts the usual Agda code and not some weird-agda-like-clone like the earlier version.

One consequence of this is that this tool accepts fully normalized code, with all implicit arguments elaborated. (Agda is able to produce such terms in interactive mode). 

# Visual Representation of Cubical Agda Code

The goal of this project is to provide visual representations of cubical Agda code. The convention of drawing these diagrams is independent of the dimension of the term, by which we mean the number of free interval variables in the term. This means that it's possible to produce these diagrams for terms of arbitrary dimension. Currently, an interface for exploring two-dimensional diagrams is implemented, but plans are in place to eventually expand this to four-dimensional and higher-dimensional diagrams.

## Integration with Agda Compiler

This version of the tool is directly integrated with the Agda compiler. To use it, you need to switch to the modified Agda branch, but plans are underway to migrate it to communicate with the compiler via reflection and macro machinery.

## From OpenGL to WebGL

Initially, the diagrams were displayed by OpenGL directly from a Haskell executable, but this led to numerous portability issues. Essentially, it was only functional on Linux, and compiling it on macOS or Windows proved challenging. This prompted a switch to a WebGL interface.

## Self-Contained Web Application

Now, the tool produces a self-contained web application that allows for interaction with the visualizations. For three-dimensional diagrams, it's possible to explore and interact with them in VR using a browser's WebXR interface.

here are some examples:

(scroll down to the definition with the blue button, and you can see the diagram, or open it in a new window in case you want to experience it full screen or on a VR device)

Cubical Agda library:
https://marcinjangrzybowski.github.io/cubeViz2-gen/code/Cubical.Foundations.GroupoidLaws.html

https://marcinjangrzybowski.github.io/cubeViz2-gen/code/Cubical.Foundations.Path.html

Brunerie Cobordism (by Tom Jack):
https://marcinjangrzybowski.github.io/cubeViz2-gen/code/Cubical.Experiments.BrunerieCobordism.html

screencasts from VR sessions:

https://www.youtube.com/watch?v=QB93KFwUNz4 
https://www.youtube.com/watch?v=TJoXI9OOeqA

## Links from html code

The custom branch of agda, can generate html code representations where suitable definitions are accompanied by links to diagrams (tbhere si separate repository with demo of this feature)

## Usecase!

Tom Jack futured diagrams generated using this tool in his presentation:

https://hott.github.io/HoTT-2023/slides/jack.pdf

Let me know if you have some nice diagrams to showcase :)

Stay tuned for more updates!

## OpenGL backend:


|two dimensional example|three dimensional example|
|--|--|
| [![2d example](https://i.imgur.com/D0vHW6qm.jpeg)](https://vimeo.com/589313743) | [![3d example](https://i.imgur.com/epATNXym.jpg)](https://vimeo.com/589305663) |



