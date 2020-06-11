This checker warns you when the initialization of global variable contain a
method or function call. The warning wants to make you aware that some functions
are expensive. As the global variables are initialized before main() is called,
these initializations can slow down the start-up time of an app.
