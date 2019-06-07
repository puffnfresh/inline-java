

## Linear interface

The linear interface for jni and jvm tracks usage of object local references as
linear arguments.

Class references aren't tracked because they are less used and usually cached,
which makes multiplicities less useful. 

Global references are also less used and share references across threads, which
also makes them less interesting to track.

Most functions taking tracked references return the same references so they can
be used again. The main exception are the _call_ methods which take a list of
`JValue` (`[JValue]`). Returning a `[JValue]` would make it rather tedious to
extract the object references from it to reuse them. If the reference arguments
of _call_ functions need to be reused, then they have to be duplicated before
the call. Duplication is very cheap compared to JNI calls.

_call_ functions have to delete their reference arguments then, which requires
the ability to discriminate tracked references from the rest. The tracked
references would be deleted before returning.

Some functions like `setObjectArrayElement array index obj` will almost always have
to return back the array so it can be used, but the object being set may no longer
be needed. In this case we could offer a variant `setObjectArrayElement_` that only
returns the array reference.

Because we are tracking local references only, we can depend on local JNI frames
to cleanup references when an exception occurs.
