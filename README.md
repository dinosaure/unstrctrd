### Unstrctrd

An header of an email as a formal format described by RFC5322. After `mrmime`,
it reveals that the more general form for any values of fields (like a date, an email address, etc.)
is the _unstructured_ form.

This library wants to provide the first ground of how to parse an email header. From that,
we want to post-process _unstructured_ values to cast them to any expected values like
email address.
