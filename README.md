# Tarea-2-Paradigma-Funcional 💻
Sobre este repositorio se desarrollará una aplicación sustentada en el paradigma funcional, capaz de soportar una interfaz visual y una lógica ligada a los movimientos de un cubo rubik de distintas dimensiones.

# Simulación de Cubo Rubik en Racket 🧩

Este proyecto implementa la lógica y la interfaz visual básica de un cubo Rubik en el lenguaje **Racket**, utilizando principios del **paradigma funcional**. Fue desarrollado como parte del curso de *Paradigmas de Programación*, con el objetivo de aplicar conceptos clave del paradigma funcional en una estructura de datos dinámica e interactiva.

## 🎯 Objetivo

Desarrollar una representación lógica y visual del cubo Rubik que permita simular sus movimientos, respetando las reglas de rotación y estructura del cubo. El proyecto pone énfasis en el uso de **funciones puras** y la **recursión**, elementos esenciales del enfoque funcional.

## 🧠 Características
- Representación funcional del estado del cubo.
- La representacion se hará de manera que la funcion principal sea la siguiente: `(RS X Cubo Movs)`, siendo X el parametro que define la dimension del cubo (2x2,3x3,4x4,5x5 o 6x6), mientras que Cubo será una lista con el estado inicial del Cubo y por último Movs contendrá una lista con los movimientos basicos que se quieran aplicar al cubo. 
- Implementación de movimientos básicos. Para mover las filas (F), seguido del numero de fila (1,2,3..6), y por último A(**A**rriba) o B(a**B**ajo) según si el movimiento es hacia arriba o abajo, de la misma manera para las columnas. Ejemplos: F1A, C3B, F3B, C4A).
- Separación clara entre la implementación de la parte lógica y el desarrollo visual.
- Sin uso de variables mutables, loops imperativos o estructuras propias del paradigma imperativo.

## 🛠️ Tecnologías y herramientas


- **Lenguaje:** Racket
- **Paradigma:** Funcional
- **Librerías:**
  - `toolkit` para la interfaz gráfica funcional.
  - `racket/gui/base` como base de ventanas y gráficos.


## 📚 Contenidos del repositorio

- `main.rkt`: archivo principal con la lógica funcional. 
- `visual.rkt`: archivo con la parte visual de la aplicación
- `readme.md`: este archivo.
- `screenshots/`: ejemplos visuales del cubo (opcional).

## 🚀 Ejecución

Para ejecutar el programa, abre `main.rkt` en DrRacket y haz clic en *Run*. Asegúrate de tener seleccionadas las librerías de `2htdp` en el lenguaje.

## 📌 Estado del proyecto

En desarrollo. Actualmente se encuentra implementada la lógica base del cubo y una interfaz visual sencilla. Se planea agregar:

- Rotaciones múltiples
- Aleatorización de estado inicial
- Validación de estado resuelto
- Animaciones de movimientos

## 📖 Créditos

Proyecto desarrollado por [Tu Nombre] como parte del curso de *Paradigmas de Programación* (2025).

---

¡Gracias por visitar el repositorio! Cualquier sugerencia o comentario es bienvenido.
