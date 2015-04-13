# algo2-tp1


El workflow con git es mas o menos asi:

0. Antes que nada, tienen que copiar el repositorio en su computadora, con ```git clone https://github.com/gciruelos/algo2-tp1.git```
1. Terminan de hacer los cambios que quieren.
2. Cuando lo que hicieron esta correcto (no pusheen cosas incorrectas), agreguen los cambios a su copia local del repositorio, con ```git add .```
3. Despues hacen efectivos todos los cambios con un commit ```git commit -m "mensaje"```. En mensaje pongan que cambios hicieron.
4. Luego tienen que pushear los cambios al repositorio remoto, o sea al que accedemos todos, para eso hay que hacer ```git push```.

Aca puede pasar que les de algun tipo de error. Lean el error, en general puede ser 2 cosas, o pusieron mal su contraseña, o les dice algo como ```Updates were rejected because the remote contains work that you do not have locally.```. Eso significa que alguien pusheo algo y entonces el repositorio que ustedes tienen localmente no esta up-to-date. Para arreglar eso tienen que hacer ```git pull``` y despues de nuevo ```git push```. 

TODO
----

Las cosas que faltan hacer:

* revisar exporta's (especialmente conexion y topologia)
* Revisar que este todo bien!!! :P


Latex
-----

Para compilar el tp, lo que tienen que hacer es ```pdflatex tp.tex```, les va a generar varios archivos, entre ellos un .pdf, ese es el .pdf del tp.
No se preocupen por borrar los archivos que genero la compilacion para que no se pusheen al repo porque ya agregué las extensiones al archivo ```.gitignore```, es decir, todos los archivos que terminen en esa extension, git los va a ignorar.

Haskell
-------
Los archivos en Haskell sirven para testear que la especificacion no tenga fallas de tipos o logicos. Anda con ```ghc version 7.8.4```.
