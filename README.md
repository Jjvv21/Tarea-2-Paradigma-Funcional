# Tarea-2-Paradigma-Funcional 💻
Sobre este repositorio se desarrollará una aplicación sustentada en el paradigma funcional, capaz de soportar una interfaz visual y una lógica ligada a los movimientos de un cubo rubik de distintas dimensiones.

# Simulación de Cubo Rubik en Racket 🧩

Este proyecto implementa la lógica y la interfaz visual básica de un cubo Rubik en el lenguaje **Racket**, utilizando principios del **paradigma funcional**. Fue desarrollado como parte del curso de *Paradigmas de Programación*, con el objetivo de aplicar conceptos clave del paradigma funcional en una estructura de datos dinámica e interactiva.

## 🎯 Objetivo

Desarrollar una representación lógica y visual del cubo Rubik que permita simular sus movimientos, respetando las reglas de rotación y estructura del cubo. El proyecto pone énfasis en la **inmutabilidad**, el uso de **funciones puras** y la **recursión**, elementos esenciales del enfoque funcional.

## 🧠 Características

- Representación funcional del estado del cubo.
- Implementación de movimientos básicos (U, D, L, R, F, B y sus variantes inversas).
- Visualización interactiva usando la librería `2htdp/image` y `2htdp/universe`.
- Separación clara entre lógica y vista.
- Sin uso de variables mutables, loops imperativos o estructuras propias del paradigma imperativo.

## 🛠️ Tecnologías y herramientas

- **Lenguaje:** Racket
- **Paradigma:** Funcional
- **Librerías:** 
  - `2htdp/image` para la representación visual.
  - `2htdp/universe` para la interacción y visualización en tiempo real.

## 📚 Contenidos del repositorio

- `main.rkt`: archivo principal con la lógica funcional y visual.
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
