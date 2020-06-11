This check fires when a pointer to an Obj-C object is tagged with an `assign`
property (similar to the `-Warc-unsafe-retained-assign` compiler flag). Not
holding a strong reference to the object makes it easy to accidentally create
and use a dangling pointer.
