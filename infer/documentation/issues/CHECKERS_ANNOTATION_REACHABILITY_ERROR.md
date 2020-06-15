A method annotated with an annotation `@A` transitively calls a method annotated `@B` where the combination of annotations is forbidden (for example, `@UiThread` calling `@WorkerThread`).
