Clojure Fucking Simple Ray Tracer
=========================
This is a solution of [Alternativa Platform](http://alternativaplatform.com) company test task.

Task
----

You need to build simple ray tracer. There is an XML file with scene contains different scene objects:

* Each object contains many polygons.
* Each polygon contains one or more faces.
* Each face contains three points.
* Each point contains three coordinates.

Sample scene file can be found on src/test/resources/simpleScene.xml

Ray has two points: origin point and directional vector. Rays are created from STDIN by parsing string following format: "1;2,3,4;5,6,7" where 1 - ray id, {2,3,4} - origin point coordinates, {5,6,7} - directional vector coordinates.

You need to trace the ray and detect which object will it hit and print the trace result in following format: "1:pyramid" where 1 is ray ID, "pyramid" - hit object name (if there is no intersection, print the ray id and empty string ("2:"));